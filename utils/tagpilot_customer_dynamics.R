# ============================================================================
# Customer Dynamics Analysis - Z-Score Based Method
# ============================================================================
#
# Purpose: Calculate customer dynamics using z-score based statistical method
#          as specified in 顧客動態計算方式調整_20251025.md
#
# Author: Development Team
# Date Created: 2025-11-01
# Last Updated: 2025-11-01
#
# Reference Documents:
#   - /docs/suggestion/subscription/顧客動態計算方式調整_20251025.md
#   - /documents/02_architecture/logic_revised.md
#
# ============================================================================

#' Analyze Customer Dynamics Using Z-Score Method
#'
#' Implements the statistical z-score based customer dynamics classification
#' as per the specification document.
#'
#' @param transaction_data Data frame with columns: customer_id, transaction_date, transaction_amount
#' @param method Character. "z_score" for statistical method, "fixed_threshold" for 7/14/21 days,
#'               "auto" to automatically choose based on data quality
#' @param k Numeric. Tolerance multiplier for W calculation (default 2.5)
#' @param min_window Numeric. Minimum observation window in days (default 90)
#' @param cap_days Numeric. Maximum observation window (default: calculated from data)
#' @param active_threshold Numeric. Z-score threshold for active customers (default 0.5)
#' @param sleepy_threshold Numeric. Z-score threshold for sleepy customers (default -1.0)
#' @param half_sleepy_threshold Numeric. Z-score threshold for half-sleepy customers (default -1.5)
#' @param use_recency_guardrail Logical. Apply recency check for active customers (default TRUE)
#'
#' @return List with components:
#'   - customer_data: Data frame with z_i, F_i_w, customer_dynamics, and related metrics
#'   - parameters: List of calculation parameters used
#'   - validation: Data quality validation results
#'
#' @export
analyze_customer_dynamics_new <- function(
  transaction_data,
  method = "auto",
  k = 2.5,
  min_window = 90,
  cap_days = NULL,
  active_threshold = 0.5,
  sleepy_threshold = -1.0,
  half_sleepy_threshold = -1.5,
  use_recency_guardrail = TRUE
) {

  message("[DEBUG-FN] ========== analyze_customer_dynamics_new() START ==========")
  message("[DEBUG-FN] Function parameters:")
  message("[DEBUG-FN]   method = ", method)
  message("[DEBUG-FN]   k = ", k)
  message("[DEBUG-FN]   min_window = ", min_window)
  message("[DEBUG-FN]   active_threshold = ", active_threshold)
  message("[DEBUG-FN]   sleepy_threshold = ", sleepy_threshold)
  message("[DEBUG-FN]   half_sleepy_threshold = ", half_sleepy_threshold)
  message("[DEBUG-FN]   use_recency_guardrail = ", use_recency_guardrail)
  message("[DEBUG-FN]   Input data rows: ", nrow(transaction_data))
  message("[DEBUG-FN]   Input data columns: ", paste(names(transaction_data), collapse = ", "))

  # ══════════════════════════════════════════════════════════════════════════
  # Standardize column names
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 0: Standardizing column names...")

    if ("payment_time" %in% names(transaction_data) && !"transaction_date" %in% names(transaction_data)) {
      transaction_data <- transaction_data %>%
        rename(transaction_date = payment_time)
      message("[DEBUG-FN]   ✓ Renamed payment_time to transaction_date")
    }

    if ("lineitem_price" %in% names(transaction_data) && !"transaction_amount" %in% names(transaction_data)) {
      transaction_data <- transaction_data %>%
        rename(transaction_amount = lineitem_price)
      message("[DEBUG-FN]   ✓ Renamed lineitem_price to transaction_amount")
    }

    message("[DEBUG-FN] ✓ Step 0 completed: Column names standardized")
  }, error = function(e) {
    message("[ERROR-FN] Step 0 FAILED: Column name standardization - ", e$message)
    stop("Column name standardization failed: ", e$message)
  })

  # Load configuration if available
  tryCatch({
    if (file.exists("config/customer_dynamics_config.R")) {
      message("[DEBUG-FN] Loading configuration from config/customer_dynamics_config.R...")
      source("config/customer_dynamics_config.R")
      config <- get_customer_dynamics_config()

      # Override with config values if not explicitly provided
      if (missing(method)) method <- config$method
      if (missing(k)) k <- config$zscore$k
      if (missing(min_window)) min_window <- config$zscore$min_window
      if (missing(active_threshold)) active_threshold <- config$zscore$active_threshold
      if (missing(sleepy_threshold)) sleepy_threshold <- config$zscore$sleepy_threshold
      if (missing(half_sleepy_threshold)) half_sleepy_threshold <- config$zscore$half_sleepy_threshold
      if (missing(use_recency_guardrail)) use_recency_guardrail <- config$zscore$use_recency_guardrail
      message("[DEBUG-FN]   ✓ Configuration loaded and applied")
    } else {
      message("[DEBUG-FN] No config file found, using default parameters")
    }
  }, error = function(e) {
    message("[WARN-FN] Config loading failed (using defaults): ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 1: Calculate cap_days (觀察期)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 1: Calculating cap_days (observation period)...")

    if (is.null(cap_days)) {
      cap_days <- as.numeric(difftime(
        max(transaction_data$transaction_date),
        min(transaction_data$transaction_date),
        units = "days"
      )) + 1
      message("[DEBUG-FN]   ✓ Calculated cap_days = ", cap_days)
    } else {
      message("[DEBUG-FN]   ✓ Using provided cap_days = ", cap_days)
    }

    message("[DEBUG-FN] ✓ Step 1 completed: cap_days = ", cap_days)
  }, error = function(e) {
    message("[ERROR-FN] Step 1 FAILED: cap_days calculation - ", e$message)
    stop("cap_days calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 2: Calculate μ_ind (產業平均購買間隔中位數)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 2: Calculating μ_ind (industry median interval)...")

    # Group by customer and calculate inter-purchase intervals
    ipt_data <- transaction_data %>%
      arrange(customer_id, transaction_date) %>%
      group_by(customer_id) %>%
      mutate(
        ipt = as.numeric(difftime(transaction_date, lag(transaction_date), units = "days"))
      ) %>%
      filter(!is.na(ipt)) %>%
      ungroup()

    message("[DEBUG-FN]   ✓ Calculated IPT for ", nrow(ipt_data), " repeat purchases")

    # Check if we have enough data
    if (nrow(ipt_data) == 0) {
      warning("No repeat customers (ni >= 2) found. Cannot calculate μ_ind. Falling back to fixed threshold method.")
      message("[WARN-FN]   No repeat customers found, fallback to fixed_threshold method")
      method <- "fixed_threshold"
    }

    # Calculate μ_ind (median of all inter-purchase intervals)
    mu_ind <- if (nrow(ipt_data) > 0) {
      median(ipt_data$ipt, na.rm = TRUE)
    } else {
      30  # Default fallback
    }

    message("[DEBUG-FN] ✓ Step 2 completed: μ_ind = ", round(mu_ind, 1), " days")
  }, error = function(e) {
    message("[ERROR-FN] Step 2 FAILED: μ_ind calculation - ", e$message)
    stop("μ_ind calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 3: Calculate W (活躍觀察期)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 3: Calculating W (active observation window)...")

    # Helper function: round to nearest multiple of 7
    round_to_7 <- function(x) {
      round(x / 7) * 7
    }

    W <- min(
      cap_days,
      max(
        min_window,
        round_to_7(k * mu_ind)
      )
    )

    message("[DEBUG-FN] ✓ Step 3 completed: W = ", W, " days")
    message("[DEBUG-FN]   (cap_days=", cap_days, ", min_window=", min_window, ", k*μ_ind=", round(k * mu_ind, 1), ")")
  }, error = function(e) {
    message("[ERROR-FN] Step 3 FAILED: W calculation - ", e$message)
    stop("W calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 4: Calculate F_i,w (最近 W 天內的購買次數)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 4: Calculating F_i,w (purchase frequency in last W days)...")

    today <- max(transaction_data$transaction_date)
    message("[DEBUG-FN]   today = ", today, " (class: ", class(today), ")")
    message("[DEBUG-FN]   W = ", W, " (class: ", class(W), ")")

    # Ensure today is Date type and W is numeric
    today <- as.Date(today)
    W_days <- as.numeric(W)

    message("[DEBUG-FN]   After conversion: today = ", today, " (class: ", class(today), ")")
    message("[DEBUG-FN]   After conversion: W_days = ", W_days, " (class: ", class(W_days), ")")

    w_days_ago <- today - W_days
    message("[DEBUG-FN]   w_days_ago = ", w_days_ago, " (class: ", class(w_days_ago), ")")

    F_i_w_data <- transaction_data %>%
      filter(transaction_date >= w_days_ago) %>%
      group_by(customer_id) %>%
      summarise(
        F_i_w = n(),
        .groups = "drop"
      )

    message("[DEBUG-FN] ✓ Step 4 completed: F_i,w calculated for ", nrow(F_i_w_data), " customers")
    message("[DEBUG-FN]   Window: ", w_days_ago, " to ", today)
  }, error = function(e) {
    message("[ERROR-FN] Step 4 FAILED: F_i,w calculation - ", e$message)
    message("[ERROR-FN]   today class: ", class(today))
    message("[ERROR-FN]   W class: ", class(W))
    stop("F_i,w calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 5: Get customer summary data
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 5: Creating customer summary data...")
    message("[DEBUG-FN]   Using today = ", today)

    customer_summary <- transaction_data %>%
      group_by(customer_id) %>%
      summarise(
        ni = n(),
        time_first = min(as.Date(transaction_date)),
        time_last = max(as.Date(transaction_date)),
        r_value = as.numeric(difftime(today, max(as.Date(transaction_date)), units = "days")),
        m_value = sum(transaction_amount, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        customer_age_days = as.numeric(difftime(today, time_first, units = "days")),
        ipt = pmax(as.numeric(difftime(time_last, time_first, units = "days")), 1),
        f_value = ni,
        total_spent = m_value,
        m_value = m_value / ni  # Average per transaction
      )

    message("[DEBUG-FN]   ✓ Customer summary created: ", nrow(customer_summary), " customers")

    # Merge F_i_w
    customer_summary <- customer_summary %>%
      left_join(F_i_w_data, by = "customer_id") %>%
      mutate(F_i_w = if_else(is.na(F_i_w), 0, F_i_w))

    message("[DEBUG-FN] ✓ Step 5 completed: F_i,w merged")
  }, error = function(e) {
    message("[ERROR-FN] Step 5 FAILED: Customer summary - ", e$message)
    stop("Customer summary failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 6: Calculate λ_w and σ_w (排除新客後)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 6: Calculating λ_w and σ_w (industry benchmarks)...")

    non_newbie <- customer_summary %>%
      filter(ni >= 2)

    message("[DEBUG-FN]   ✓ Non-newbie customers: ", nrow(non_newbie))

    if (nrow(non_newbie) < 10) {
      warning("Insufficient non-newbie customers (< 10). Falling back to fixed threshold method.")
      message("[WARN-FN]   Insufficient data (< 10 repeat customers), fallback to fixed_threshold")
      method <- "fixed_threshold"
    }

    lambda_w <- mean(non_newbie$F_i_w, na.rm = TRUE)
    sigma_w <- sd(non_newbie$F_i_w, na.rm = TRUE)

    message("[DEBUG-FN]   λ_w (mean F_i,w) = ", round(lambda_w, 2))
    message("[DEBUG-FN]   σ_w (SD F_i,w) = ", round(sigma_w, 2))

    # Handle edge case: sigma_w = 0
    if (is.na(sigma_w) || sigma_w == 0) {
      warning("Standard deviation is 0 or NA. Cannot calculate z-scores. Falling back to fixed threshold method.")
      message("[WARN-FN]   σ_w is 0 or NA, fallback to fixed_threshold")
      method <- "fixed_threshold"
      sigma_w <- 1  # Prevent division by zero
    }

    message("[DEBUG-FN] ✓ Step 6 completed")
  }, error = function(e) {
    message("[ERROR-FN] Step 6 FAILED: λ_w and σ_w calculation - ", e$message)
    stop("λ_w and σ_w calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 7: Calculate z_i scores
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 7: Calculating z_i scores...")

    customer_summary <- customer_summary %>%
      mutate(
        z_i = (F_i_w - lambda_w) / sigma_w
      )

    message("[DEBUG-FN] ✓ Step 7 completed: z_i calculated")
    message("[DEBUG-FN]   z_i range: [", round(min(customer_summary$z_i, na.rm=TRUE), 2), ", ",
            round(max(customer_summary$z_i, na.rm=TRUE), 2), "]")
  }, error = function(e) {
    message("[ERROR-FN] Step 7 FAILED: z_i calculation - ", e$message)
    stop("z_i calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 8: Classify customer_dynamics
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 8: Classifying customer_dynamics...")
    message("[DEBUG-FN]   Current method: ", method)
    message("[DEBUG-FN]   non_newbie count: ", nrow(non_newbie))
    message("[DEBUG-FN]   cap_days: ", cap_days)

    if (method == "fixed_threshold" || (method == "auto" && (nrow(non_newbie) < 10 || cap_days < 365))) {
      # Use fixed threshold method (7/14/21 days)
      message("[DEBUG-FN]   Using fixed_threshold method (7/14/21 days)")

      customer_summary <- customer_summary %>%
        mutate(
          customer_dynamics = case_when(
            is.na(r_value) ~ "unknown",
            ni == 1 ~ "newbie",
            r_value <= 7 ~ "active",
            r_value <= 14 ~ "sleepy",
            r_value <= 21 ~ "half_sleepy",
            TRUE ~ "dormant"
          ),
          method_used = "fixed_threshold"
        )

      message("[DEBUG-FN] ✓ Step 8 completed: fixed_threshold classification done")

    } else {
      # Use z-score method
      message("[DEBUG-FN]   Using z_score method")
      message("[DEBUG-FN]   CRITICAL: Storing parameters as local variables...")

      # CRITICAL FIX: Store ALL function parameters as local variables before using in mutate
      # to avoid "non-numeric argument to binary operator" error
      use_guardrail <- use_recency_guardrail
      active_thresh <- active_threshold
      sleepy_thresh <- sleepy_threshold
      half_sleepy_thresh <- half_sleepy_threshold

      message("[DEBUG-FN]     use_guardrail = ", use_guardrail, " (type: ", class(use_guardrail), ")")
      message("[DEBUG-FN]     active_thresh = ", active_thresh, " (type: ", class(active_thresh), ")")
      message("[DEBUG-FN]     sleepy_thresh = ", sleepy_thresh, " (type: ", class(sleepy_thresh), ")")
      message("[DEBUG-FN]     half_sleepy_thresh = ", half_sleepy_thresh, " (type: ", class(half_sleepy_thresh), ")")
      message("[DEBUG-FN]     mu_ind = ", round(mu_ind, 2), " (type: ", class(mu_ind), ")")

      message("[DEBUG-FN]   Starting case_when() classification...")

      customer_summary <- customer_summary %>%
        mutate(
          customer_dynamics = case_when(
            is.na(r_value) ~ "unknown",
            ni == 1 ~ "newbie",
            use_guardrail & z_i >= active_thresh & r_value <= mu_ind ~ "active",
            !use_guardrail & z_i >= active_thresh ~ "active",
            z_i >= sleepy_thresh & z_i < active_thresh ~ "sleepy",
            z_i >= half_sleepy_thresh & z_i < sleepy_thresh ~ "half_sleepy",
            z_i < half_sleepy_thresh ~ "dormant",
            TRUE ~ "unknown"
          ),
          method_used = "z_score"
        )

      message("[DEBUG-FN] ✓ Step 8 completed: z_score classification done")
    }

    message("[DEBUG-FN]   Customer dynamics distribution:")
    dynamics_table <- table(customer_summary$customer_dynamics)
    for (i in seq_along(dynamics_table)) {
      message("[DEBUG-FN]     ", names(dynamics_table)[i], ": ", dynamics_table[i])
    }

  }, error = function(e) {
    message("[ERROR-FN] ========== Step 8 FAILED: Customer dynamics classification ==========")
    message("[ERROR-FN] Error message: ", e$message)
    message("[ERROR-FN] Error call: ", deparse(e$call))
    message("[ERROR-FN] Current state:")
    message("[ERROR-FN]   method = ", method)
    message("[ERROR-FN]   use_recency_guardrail = ", use_recency_guardrail, " (", class(use_recency_guardrail), ")")
    message("[ERROR-FN]   active_threshold = ", active_threshold, " (", class(active_threshold), ")")
    message("[ERROR-FN]   sleepy_threshold = ", sleepy_threshold, " (", class(sleepy_threshold), ")")
    message("[ERROR-FN]   half_sleepy_threshold = ", half_sleepy_threshold, " (", class(half_sleepy_threshold), ")")
    message("[ERROR-FN]   mu_ind = ", if(exists("mu_ind")) round(mu_ind, 2) else "NOT DEFINED", " (", if(exists("mu_ind")) class(mu_ind) else "N/A", ")")
    message("[ERROR-FN] ======================================================================")
    stop("Customer dynamics classification failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 8.5: Calculate value_level (needed by calculate_rfm_tags)
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 8.5: Calculating value_level...")

    m_values_valid <- customer_summary$m_value[!is.na(customer_summary$m_value)]

    if (length(m_values_valid) > 0) {
      # 使用分位數方法，並確保永遠有低中高三群
      n_customers <- length(m_values_valid)

      # 計算分位數
      m_p20 <- quantile(m_values_valid, 0.2, na.rm = TRUE, names = FALSE)
      m_p80 <- quantile(m_values_valid, 0.8, na.rm = TRUE, names = FALSE)
      m_min <- min(m_values_valid, na.rm = TRUE)
      m_max <- max(m_values_valid, na.rm = TRUE)

      message("[DEBUG-FN]   M-value range: [", round(m_min, 2), ", ", round(m_max, 2), "]")
      message("[DEBUG-FN]   P20 = ", round(m_p20, 2))
      message("[DEBUG-FN]   P80 = ", round(m_p80, 2))

      # 檢查是否所有值相同（或接近相同）
      if (abs(m_max - m_min) < 0.01 || m_p20 == m_p80) {
        # 特殊處理：所有值相同或太接近，強制分成三等份
        message("[DEBUG-FN]   ⚠️  M-values too similar, forcing equal split into 3 groups")

        # 排序後均分
        customer_summary <- customer_summary %>%
          arrange(m_value) %>%
          mutate(
            value_rank = row_number(),
            value_level = case_when(
              is.na(m_value) ~ "未知",
              value_rank <= ceiling(n() * 0.2) ~ "低",
              value_rank <= ceiling(n() * 0.8) ~ "中",
              TRUE ~ "高"
            )
          ) %>%
          select(-value_rank)

      } else if (m_p20 == m_min) {
        # P20 = 最小值：會缺少「低」群，需要特殊處理
        message("[DEBUG-FN]   ⚠️  P20 equals min, adjusting to ensure '低' group exists")

        # 強制最底部 20% 為「低」
        customer_summary <- customer_summary %>%
          arrange(m_value) %>%
          mutate(
            value_rank = row_number(),
            value_level = case_when(
              is.na(m_value) ~ "未知",
              value_rank <= ceiling(n() * 0.2) ~ "低",
              m_value >= m_p80 ~ "高",
              TRUE ~ "中"
            )
          ) %>%
          select(-value_rank)

      } else if (m_p80 == m_max) {
        # P80 = 最大值：會缺少「高」群，需要特殊處理
        message("[DEBUG-FN]   ⚠️  P80 equals max, adjusting to ensure '高' group exists")

        # 強制最頂部 20% 為「高」
        customer_summary <- customer_summary %>%
          arrange(desc(m_value)) %>%
          mutate(
            value_rank = row_number(),
            value_level = case_when(
              is.na(m_value) ~ "未知",
              value_rank <= ceiling(n() * 0.2) ~ "高",
              m_value < m_p20 ~ "低",
              TRUE ~ "中"
            )
          ) %>%
          select(-value_rank)

      } else {
        # 正常情況：使用 P20/P80 分位數
        message("[DEBUG-FN]   Using P20/P80 percentile method for value_level")

        customer_summary <- customer_summary %>%
          mutate(
            value_level = case_when(
              is.na(m_value) ~ "未知",
              m_value >= m_p80 ~ "高",    # Top 20%
              m_value >= m_p20 ~ "中",    # Middle 60%
              TRUE ~ "低"                 # Bottom 20%
            )
          )
      }

      # Debug: Show distribution
      value_dist <- table(customer_summary$value_level)
      message("[DEBUG-FN]   Value level distribution:")
      for (i in seq_along(value_dist)) {
        message("[DEBUG-FN]     ", names(value_dist)[i], ": ", value_dist[i], " (",
                round(100 * value_dist[i] / sum(value_dist), 1), "%)")
      }

      # 驗證是否真的有三群（排除「未知」）
      non_unknown_groups <- setdiff(names(value_dist), "未知")
      if (length(non_unknown_groups) < 3) {
        message("[WARN-FN]   Only ", length(non_unknown_groups), " groups generated: ", paste(non_unknown_groups, collapse = ", "))
      }

    } else {
      message("[WARN-FN]   No valid m_value, all marked as 未知")
      customer_summary <- customer_summary %>%
        mutate(value_level = "未知")
    }

    message("[DEBUG-FN] ✓ Step 8.5 completed: value_level calculated")
  }, error = function(e) {
    message("[ERROR-FN] Step 8.5 FAILED: value_level calculation - ", e$message)
    stop("value_level calculation failed: ", e$message)
  })

  # ══════════════════════════════════════════════════════════════════════════
  # Step 9: Prepare output
  # ══════════════════════════════════════════════════════════════════════════
  tryCatch({
    message("[DEBUG-FN] Step 9: Preparing output...")

    parameters <- list(
      method = method,
      cap_days = cap_days,
      mu_ind = mu_ind,
      W = W,
      k = k,
      lambda_w = lambda_w,
      sigma_w = sigma_w,
      min_window = min_window,
      thresholds = list(
        active = active_threshold,
        sleepy = sleepy_threshold,
        half_sleepy = half_sleepy_threshold
      ),
      use_recency_guardrail = use_recency_guardrail
    )

    validation <- list(
      total_customers = nrow(customer_summary),
      newbie_count = sum(customer_summary$ni == 1),
      repeat_customers = nrow(non_newbie),
      observation_period_days = cap_days,
      sufficient_data = cap_days >= 365 && nrow(non_newbie) >= 30,
      method_used = unique(customer_summary$method_used)[1]
    )

    message("[DEBUG-FN] ✓ Step 9 completed: Output prepared")
    message("[DEBUG-FN] ========== analyze_customer_dynamics_new() COMPLETED SUCCESSFULLY ==========")
    message("[DEBUG-FN] Final summary:")
    message("[DEBUG-FN]   Total customers: ", validation$total_customers)
    message("[DEBUG-FN]   Method used: ", validation$method_used)
    message("[DEBUG-FN]   Newbies: ", validation$newbie_count)
    message("[DEBUG-FN]   Repeat customers: ", validation$repeat_customers)

    # Return results
    list(
      customer_data = customer_summary,
      parameters = parameters,
      validation = validation
    )
  }, error = function(e) {
    message("[ERROR-FN] Step 9 FAILED: Output preparation - ", e$message)
    stop("Output preparation failed: ", e$message)
  })
}


#' Print Customer Dynamics Summary
#'
#' @param analysis_result Output from analyze_customer_dynamics_new()
#' @export
print_customer_dynamics_summary <- function(analysis_result) {
  cat("════════════════════════════════════════════════════════════\n")
  cat("  Customer Dynamics Analysis Summary\n")
  cat("════════════════════════════════════════════════════════════\n\n")

  cat("Method Used:", analysis_result$validation$method_used, "\n\n")

  cat("Parameters:\n")
  cat("  Observation Period (cap_days):", analysis_result$parameters$cap_days, "days\n")
  cat("  Industry Median Interval (μ_ind):", round(analysis_result$parameters$mu_ind, 1), "days\n")
  cat("  Active Window (W):", analysis_result$parameters$W, "days\n")

  if (analysis_result$validation$method_used == "z_score") {
    cat("  Tolerance Multiplier (k):", analysis_result$parameters$k, "\n")
    cat("  Mean F_i,w (λ_w):", round(analysis_result$parameters$lambda_w, 2), "\n")
    cat("  SD F_i,w (σ_w):", round(analysis_result$parameters$sigma_w, 2), "\n")
  }

  cat("\nCustomer Distribution:\n")
  dynamics_table <- table(analysis_result$customer_data$customer_dynamics)
  print(dynamics_table)
  cat("\n")

  cat("Validation:\n")
  cat("  Total Customers:", analysis_result$validation$total_customers, "\n")
  cat("  Newbies (ni=1):", analysis_result$validation$newbie_count, "\n")
  cat("  Repeat Customers:", analysis_result$validation$repeat_customers, "\n")
  cat("  Sufficient Data:", if_else(analysis_result$validation$sufficient_data, "✅ Yes", "⚠️  No"), "\n")

  cat("\n════════════════════════════════════════════════════════════\n")
}

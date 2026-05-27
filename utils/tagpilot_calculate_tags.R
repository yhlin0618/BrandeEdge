################################################################################
# Customer Tags Calculation Functions
# 客戶標籤計算工具函數
#
# Purpose: 模組化的標籤計算邏輯，符合 MP016 Modularity 原則
# Created: 2025-10-25
################################################################################

library(dplyr)

#' 計算客戶基數價值標籤（第二列）
#'
#' @param customer_data data.frame - 包含客戶基礎資訊的資料框
#' @return data.frame - 加入基數價值標籤的資料框
#'
#' 生成標籤:
#' - tag_001_avg_purchase_cycle: 平均購買週期
#' - tag_003_historical_total_value: 歷史總價值
#' - tag_004_avg_order_value: 平均客單價 (AOV)
calculate_base_value_tags <- function(customer_data) {
  customer_data %>%
    mutate(
      # tag_001: 平均購買週期（僅計算有回購的顧客）
      tag_001_avg_purchase_cycle = case_when(
        !is.na(ni) & ni >= 2 & !is.na(ipt) & ipt > 0 ~ ipt / pmax(ni - 1, 1),
        TRUE ~ NA_real_
      ),

      # tag_003: 歷史總價值（總消費金額）
      # ✅ 修正：m_value 在 DNA 分析中已經是總消費金額（total_spent）
      # 不需要再乘以 ni
      tag_003_historical_total_value = m_value,

      # tag_004: 平均客單價 (AOV)
      # ✅ 修正：需要用總消費金額除以交易次數
      tag_004_avg_order_value = m_value / ni
    )
}

#' 計算 RFM 分數（輔助函數）
#'
#' @param customer_data data.frame - 包含 R/F/M 值的資料框
#' @return data.frame - 加入 R/F/M 分數的資料框
#'
#' 分數規則:
#' - R分數（越小越好）: 1-5分，5分=最近購買
#' - F分數（越大越好）: 1-5分，5分=高頻率
#' - M分數（越大越好）: 1-5分，5分=高價值
#'
#' ✅ UPDATED: 計算所有客戶的 RFM 分數（移除 ni >= 4 限制）
calculate_rfm_scores <- function(customer_data) {
  # ✅ NEW: Calculate RFM scores for ALL customers (no ni >= 4 filter)
  # Calculate quantiles across ALL customers
  r_quantiles <- quantile(customer_data$r_value,
                          probs = c(0.2, 0.4, 0.6, 0.8),
                          na.rm = TRUE)

  f_quantiles <- quantile(customer_data$f_value,
                          probs = c(0.2, 0.4, 0.6, 0.8),
                          na.rm = TRUE)

  m_quantiles <- quantile(customer_data$m_value,
                          probs = c(0.2, 0.4, 0.6, 0.8),
                          na.rm = TRUE)

  # Calculate scores for ALL customers
  customer_data %>%
    mutate(
      # R score (lower is better, so reverse)
      r_score = case_when(
        r_value <= r_quantiles[1] ~ 5,  # Most recent
        r_value <= r_quantiles[2] ~ 4,
        r_value <= r_quantiles[3] ~ 3,
        r_value <= r_quantiles[4] ~ 2,
        TRUE ~ 1  # Least recent
      ),

      # F score (higher is better)
      f_score = case_when(
        f_value >= f_quantiles[4] ~ 5,  # Highest frequency
        f_value >= f_quantiles[3] ~ 4,
        f_value >= f_quantiles[2] ~ 3,
        f_value >= f_quantiles[1] ~ 2,
        TRUE ~ 1  # Lowest frequency (includes newbies - expected)
      ),

      # M score (higher is better)
      m_score = case_when(
        m_value >= m_quantiles[4] ~ 5,  # Highest value
        m_value >= m_quantiles[3] ~ 4,
        m_value >= m_quantiles[2] ~ 3,
        m_value >= m_quantiles[1] ~ 2,
        TRUE ~ 1  # Lowest value
      ),

      # tag_012: RFM total score (3-15 points, now for ALL customers)
      tag_012_rfm_score = r_score + f_score + m_score
    )
}

#' 計算 RFM 分析標籤（第三列）
#'
#' @param customer_data data.frame - 包含客戶基礎資訊的資料框
#' @return data.frame - 加入 RFM 標籤的資料框
#'
#' 生成標籤:
#' - tag_009_rfm_r: R值（最近購買天數）
#' - tag_010_rfm_f: F值（購買頻率）
#' - tag_011_rfm_m: M值（貨幣價值）
#' - tag_012_rfm_score: RFM總分（3-15分，✅ 所有客戶）
#' - tag_013_value_segment: 價值分群（高/中/低）
calculate_rfm_tags <- function(customer_data) {
  # 先建立基礎 RFM 標籤
  customer_data <- customer_data %>%
    mutate(
      # tag_009: R值（最近購買天數）
      tag_009_rfm_r = r_value,

      # tag_010: F值（購買頻率）
      tag_010_rfm_f = f_value,

      # tag_011: M值（貨幣價值）
      tag_011_rfm_m = m_value,

      # tag_013: 價值分群（使用已有的 value_level）
      tag_013_value_segment = value_level
    )

  # 計算 RFM 分數（需要所有客戶的 R/F/M 值）
  customer_data <- calculate_rfm_scores(customer_data)

  return(customer_data)
}

#' 計算客戶狀態標籤（第四列）
#'
#' @param customer_data data.frame - 包含客戶基礎資訊的資料框
#' @return data.frame - 加入客戶狀態標籤的資料框
#'
#' 生成標籤:
#' - tag_017_customer_dynamics: 客戶動態（中文）
#' - tag_018_churn_risk: 流失風險（高/中/低風險）
#' - tag_019_days_to_churn: 距離流失天數
calculate_status_tags <- function(customer_data) {
  customer_data %>%
    mutate(
      # tag_017: 客戶動態（轉換為中文）
      tag_017_customer_dynamics = case_when(
        customer_dynamics == "newbie" ~ "新客",
        customer_dynamics == "active" ~ "主力客",
        customer_dynamics == "sleepy" ~ "睡眠客",
        customer_dynamics == "half_sleepy" ~ "半睡客",
        customer_dynamics == "dormant" ~ "沉睡客",
        TRUE ~ customer_dynamics
      ),

      # tag_018: 流失風險
      # ✅ 修正：加入對新客和交易次數不足的特殊處理
      tag_018_churn_risk = case_when(
        # 新客：特殊處理（尚未建立購買模式）
        ni == 1 ~ "新客（無法評估）",
        # 交易次數不足（2-3次）：謹慎評估
        ni < 4 ~ if_else(r_value > 30, "中風險", "低風險"),  # 簡化邏輯
        # 一般客戶：基於 R值 與 平均購買週期的比較
        # Note: ipt is the inter-purchase time field from DNA analysis
        r_value > ipt * 2 ~ "高風險",    # 超過2倍購買週期
        r_value > ipt * 1.5 ~ "中風險",  # 超過1.5倍購買週期
        TRUE ~ "低風險"
      ),

      # tag_019: 距離流失天數（預估）
      # ✅ Issue #3修正：使用 ipt 作為平均購買間隔（由 DNA 分析提供）
      # 預估流失天數 = 2倍平均購買間隔 - 當前R值
      tag_019_days_to_churn = case_when(
        ni == 1 ~ NA_real_,  # 新客無法預測
        ni < 2 ~ NA_real_,   # 需至少2次購買才能計算間隔
        is.na(ipt) | ipt <= 0 ~ NA_real_,  # ipt 無效
        # 一般客戶：2倍平均購買間隔 - 當前R值（負值設為0）
        TRUE ~ pmax(0, round(ipt * 2 - r_value, 0))
      )
    )
}

#' 計算生命週期預測標籤（第六列）
#'
#' @param customer_data data.frame - 包含客戶基礎資訊的資料框
#' @param mu_ind numeric - 產業中位數購買間隔（可選，用於改善預測）
#' @return data.frame - 加入預測標籤的資料框
#'
#' ✅ UPDATED: 使用剩餘時間演算法
#'
#' 生成標籤:
#' - tag_030_next_purchase_amount: 下次購買預測金額
#' - tag_031_next_purchase_date: 下次購買預測日期（剩餘時間演算法）
#' - tag_031_prediction_method: 預測方法說明
#'
#' 演算法邏輯:
#' 1. 週期內客戶：今天 + 剩餘時間
#' 2. 逾期客戶：今天 + 完整週期
calculate_prediction_tags <- function(customer_data, mu_ind = NULL) {
  # ✅ UPDATED: Implement remaining time algorithm for next purchase prediction
  #
  # Algorithm:
  # 1. Use customer's ipt as expected cycle (always available from DNA analysis)
  # 2. Calculate time elapsed since last purchase (r_value = recency)
  # 3. Calculate remaining time = expected_cycle - time_elapsed
  # 4. If remaining_time > 0: predict today + remaining_time
  # 5. If remaining_time <= 0 (overdue): predict today + expected_cycle (next cycle)
  #
  # Note: mu_ind parameter is optional but not used since ipt is always available

  customer_data %>%
    mutate(
      # tag_030: Next purchase amount prediction (using average order value)
      # ✅ 修正：應使用平均訂單金額（m_value / ni），而非總消費金額
      tag_030_next_purchase_amount = case_when(
        ni == 0 ~ NA_real_,  # 無交易記錄
        is.na(m_value) ~ NA_real_,  # m_value 無效
        TRUE ~ m_value / ni  # 平均訂單金額 = 總消費 / 購買次數
      ),

      # tag_031: Next purchase date prediction (remaining time algorithm)
      # Step 1: Determine expected purchase cycle
      # Simply use customer's ipt (time span from first to last purchase)
      expected_cycle = ipt,

      # Step 2: Time elapsed since last purchase
      time_elapsed = r_value,  # Recency (days since last purchase)

      # Step 3: Calculate remaining time in current cycle
      remaining_time = pmax(0, expected_cycle - time_elapsed),

      # Step 4: Predict next purchase date
      tag_031_next_purchase_date = case_when(
        # Still within cycle: today + remaining time
        remaining_time > 0 ~ as.Date(Sys.time()) + remaining_time,

        # Overdue: predict one full cycle ahead
        TRUE ~ as.Date(Sys.time()) + expected_cycle
      ),

      # Optional: Add explanation field for debugging/display
      tag_031_prediction_method = case_when(
        remaining_time > 0 ~ sprintf("剩餘 %.0f 天（週期內）", remaining_time),
        TRUE ~ sprintf("已逾期 %.0f 天，預測下個週期", time_elapsed - expected_cycle)
      )
    ) %>%
    select(-expected_cycle, -time_elapsed, -remaining_time)  # Remove temporary columns
}

#' 計算所有客戶標籤（主函數）
#'
#' @param customer_data data.frame - 包含客戶基礎資訊的資料框
#' @return data.frame - 加入所有標籤的資料框
#'
#' 這個函數按順序呼叫所有標籤計算函數
#' 確保標籤計算的依賴關係正確（例如預測標籤依賴基數價值標籤）
calculate_all_customer_tags <- function(customer_data) {
  customer_data %>%
    calculate_base_value_tags() %>%      # 第二列：基數價值
    calculate_rfm_tags() %>%             # 第三列：RFM 分析（包含 RFM 分數）
    calculate_status_tags() %>%          # 第四列：客戶狀態
    calculate_prediction_tags()          # 第六列：生命週期預測
}

#' 獲取標籤摘要資訊
#'
#' @param customer_data data.frame - 包含標籤的客戶資料
#' @return list - 標籤摘要資訊
#'
#' 返回:
#' - total_tags: 總標籤數
#' - tag_groups: 各分類的標籤數量
#' - tag_names: 所有標籤名稱
get_tags_summary <- function(customer_data) {
  tag_cols <- names(customer_data)[grepl("^tag_", names(customer_data))]

  tag_groups <- list(
    "客戶基數價值" = c("tag_001_avg_purchase_cycle",
                    "tag_003_historical_total_value",
                    "tag_004_avg_order_value"),

    "RFM 分析" = c("tag_009_rfm_r",
                  "tag_010_rfm_f",
                  "tag_011_rfm_m",
                  "tag_012_rfm_score",
                  "tag_013_value_segment"),

    "客戶狀態" = c("tag_017_customer_dynamics",
                 "tag_018_churn_risk",
                 "tag_019_days_to_churn"),

    "生命週期預測" = c("tag_030_next_purchase_amount",
                    "tag_031_next_purchase_date")
  )

  # 計算各組可用標籤數
  group_counts <- lapply(tag_groups, function(group_tags) {
    length(intersect(group_tags, tag_cols))
  })

  list(
    total_tags = length(tag_cols),
    tag_groups = tag_groups,
    group_counts = group_counts,
    tag_names = tag_cols
  )
}

################################################################################
# 使用範例
################################################################################
#
# # 在 module_dna_multi_premium.R 中使用:
# source("utils/calculate_customer_tags.R")
#
# # 方式 1: 一次計算所有標籤
# customer_data <- calculate_all_customer_tags(customer_data)
#
# # 方式 2: 分步驟計算（用於調試或特殊需求）
# customer_data <- customer_data %>%
#   calculate_base_value_tags() %>%
#   calculate_rfm_tags() %>%
#   calculate_status_tags() %>%
#   calculate_prediction_tags()
#
# # 獲取標籤摘要
# tags_info <- get_tags_summary(customer_data)
# print(paste("總共生成", tags_info$total_tags, "個標籤"))
#
################################################################################

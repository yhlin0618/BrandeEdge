################################################################################
# BrandEdge Ideal Point Analysis Module
# 理想點分析模組
# Version: 2.0
# Last Updated: 2025-10-08
# Framework: InsightForge pattern with language support
################################################################################
#
# Purpose: 根據理想點計算各品牌的接近度評分並進行排名
#
# Principles Applied:
# - MP104: ETL Data Flow Separation Principle - 模組專注於業務邏輯展示
# - R092: Universal DBI Pattern - 使用標準化資料庫連接模式
# - P_UI_CONSISTENCY: InsightForge UI 一致性原則
# - P_LANGUAGE_SUPPORT: 多語言支援原則
#
################################################################################

# NULL coalescing operator (Following InsightForge standard pattern)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ========== UI Function ==========

#' Ideal Point Analysis Module – UI
#'
#' @param id Module namespace ID
#' @param module_config Optional module configuration from app_config.yaml
#' @param lang_texts Language texts from language manager (reactive or static)
#'
#' @return tagList of UI elements
#'
#' @details
#' Displays ideal point analysis with key factors and brand ranking table.
#' Uses InsightForge framework pattern with dynamic placeholder rendering.
idealPointModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Helper function to get text from lang_texts
  # Supports both static list and reactive expression
  get_lang_value <- function(path, default) {
    # Handle NULL lang_texts
    if (is.null(lang_texts)) return(default)

    # Handle reactive lang_texts
    texts <- if (is.function(lang_texts)) {
      tryCatch(lang_texts(), error = function(e) NULL)
    } else {
      lang_texts
    }

    if (is.null(texts)) return(default)

    # Parse path (support both . and $ separators)
    parts <- strsplit(path, "\\.|\\$")[[1]]
    current <- texts

    # Navigate through nested structure
    for (part in parts) {
      if (is.list(current) && part %in% names(current)) {
        current <- current[[part]]
      } else {
        return(default)
      }
    }

    if (is.null(current)) default else current
  }

  # Get translated texts with Chinese defaults (Following R_LANG_DEFAULT_ZH principle)
  no_data_title <- get_lang_value("ui.no_data.title", "理想點分析")
  no_data_message <- get_lang_value("ui.no_data.message", "請先上傳評論資料以進行理想點分析")
  upload_button <- get_lang_value("ui.no_data.upload_button", "前往資料上傳")

  tagList(
    # Dynamic placeholder (rendered in Server function when no data)
    uiOutput(ns("no_data_placeholder")),

    # Main content area (shown when data exists)
    uiOutput(ns("content_area"))
  )
}

# ========== Server Function ==========

#' Ideal Point Analysis Module – Server
#'
#' @param id Module namespace ID
#' @param data Reactive expression containing brand attribute data
#' @param raw Reactive expression containing raw brand data (with Brand column)
#' @param indicator Reactive expression containing indicator data for scoring
#' @param key_vars Reactive expression containing key factor variable names
#' @param lang_texts Reactive expression for language texts
#'
#' @return NULL (displays analysis results in UI)
#'
#' @details
#' Business Logic:
#' 1. Filters data for "Ideal" row to get ideal point values
#' 2. Calculates Score for each brand by summing indicator columns
#' 3. Ranks brands by Score in descending order
#' 4. Displays key factors and ranking table
#'
#' Principles:
#' - MP064: ETL-Derivation Separation - This is derivation/business logic
#' - DM_R036: No autodeinit() needed as no cleanup required
#' - P_REACTIVE_SAFETY: All reactive inputs wrapped with req()
idealPointModuleServer <- function(id, data, raw, indicator, key_vars, lang_texts = reactive(NULL), module_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========== Configuration Loading ==========
    # Load all configuration values with fallback defaults
    cfg_ideal_identifier <- if (!is.null(module_config$ideal_point$identifier)) {
      module_config$ideal_point$identifier
    } else { "Ideal" } # ✅

    cfg_table_page_length <- if (!is.null(module_config$ranking_table$page_length)) {
      module_config$ranking_table$page_length
    } else { 10 } # ✅

    # Exclude columns configuration
    cfg_exclude_cols <- if (!is.null(module_config$score_calculation$exclude_columns)) {
      module_config$score_calculation$exclude_columns
    } else { c("Variation", "sales", "rating") } # ✅

    message("📝 Ideal Point Module Config Loaded: ideal_id=", cfg_ideal_identifier,
            ", page_length=", cfg_table_page_length, ", exclude=", paste(cfg_exclude_cols, collapse = ","))

    # ── Helper function: Get text from reactive lang_texts ───────────────────
    # Safely retrieves language text with fallback to defaults
    get_lang_text <- function(path, default = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) {
        NULL
      })

      if (is.null(texts)) return(default)

      # Parse nested path
      parts <- strsplit(path, "\\.|\\$")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          return(default)
        }
      }
      return(result)
    }

    # ── Dynamic placeholder rendering ─────────────────────────────────────────
    # Shows placeholder when no data is available
    output$no_data_placeholder <- renderUI({
      has_data <- !is.null(data()) && nrow(data()) > 0

      if (!has_data) {
        div(
          class = "alert alert-info text-center",
          style = "margin: 50px auto; max-width: 600px;",
          icon("info-circle"), " ",
          strong(get_lang_text("ui.no_data.title", "理想點分析")),
          br(), br(),
          get_lang_text("ui.no_data.message", "請先上傳評論資料以進行理想點分析"),
          br(), br(),
          actionButton(ns("goto_upload"),
                      get_lang_text("ui.no_data.upload_button", "前往資料上傳"),
                      class = "btn-primary",
                      icon = icon("upload"))
        )
      }
    })

    # ── Navigation button handler ─────────────────────────────────────────────
    # Handles click on "Go to Upload" button
    observeEvent(input$goto_upload, {
      showNotification(
        get_lang_text("messages.redirect_to_upload", "請從側邊欄選擇「資料上傳」頁面"),
        type = "message",
        duration = 3
      )
    }, ignoreInit = TRUE)

    # ── Reactive: Ideal Row ───────────────────────────────────────────────────
    # Calculate ideal point using rating-weighted average (MK03 principle)
    # Business Logic: 使用評分加權平均計算理想點，高評分品牌對理想點貢獻更多
    # Formula: Ideal_j = Σ(Score_i × X_ij) / Σ(Score_i)
    ideal_row <- reactive({
      df <- data()
      req(nrow(df) > 0)

      # Check if data has "Ideal" row (backward compatibility)
      if (cfg_ideal_identifier %in% df$Variation) {
        return(df %>%
          filter(Variation == cfg_ideal_identifier) %>%
          select(where(~ is.numeric(.x) && !any(is.na(.x)))))
      }

      # MK03: Calculate ideal point using rating-weighted average
      # Get Score column as weights (if available)
      if ("Score" %in% names(df)) {
        weights <- df$Score
        weights <- weights / sum(weights, na.rm = TRUE)  # Normalize to sum to 1
      } else {
        # Equal weights if no Score column
        weights <- rep(1 / nrow(df), nrow(df))
      }

      # Select numeric columns (exclude Score, rank, Variation, etc.)
      numeric_cols <- df %>%
        select(where(is.numeric)) %>%
        select(-any_of(c("Score", "rank", "Rank")))

      # Calculate weighted average for each attribute as ideal point
      ideal_values <- sapply(numeric_cols, function(x) {
        sum(x * weights, na.rm = TRUE)
      })

      as.data.frame(t(ideal_values))
    })

    # ── Main content area rendering ───────────────────────────────────────────
    output$content_area <- renderUI({
      req(data())

      # Get language texts
      key_factors_label <- get_lang_text("ui.key_factors.label", "關鍵因素：")
      ranking_title <- get_lang_text("ui.ranking_table.title", "品牌排名")

      tagList(
        # Key factors display
        div(
          style = "margin-bottom: 20px; padding: 15px; background: #f8f9fa; border-radius: 5px;",
          h5(icon("key"), " ", get_lang_text("ui.sections.key_factors", "關鍵因素分析")),
          textOutput(ns("key_factors"))
        ),

        # Brand ranking table
        div(
          h5(icon("trophy"), " ", get_lang_text("ui.sections.ranking", "品牌理想點排名")),
          DTOutput(ns("ideal_rank"))
        )
      )
    })

    # ── Output: Key Factors Text ──────────────────────────────────────────────
    # Displays the key factor variables identified in the analysis
    output$key_factors <- renderText({
      req(key_vars())

      label <- get_lang_text("ui.key_factors.label", "關鍵因素：")
      factors <- paste(key_vars(), collapse = ", ")

      paste0(label, factors)
    })

    # ── Output: Ideal Point Ranking Table ─────────────────────────────────────
    # Business Logic: Calculate Score and rank brands by proximity to ideal point
    # MK03 Principle: Score = count of key factors where brand value >= ideal value
    output$ideal_rank <- renderDT({
      # Require all necessary reactive inputs
      ind <- indicator()
      ideal <- ideal_row()
      req(ind, nrow(ind) > 0)
      req(ideal, ncol(ideal) > 0)

      cat("\n🔍 [Ideal Point] Calculating brand rankings (MK03)...\n")
      cat("  - indicator() rows:", nrow(ind), "(should be brand-level)\n")
      cat("  - indicator() cols:", ncol(ind), "\n")

      # Get ideal values for comparison
      ideal_values <- as.numeric(ideal[1, ])
      names(ideal_values) <- names(ideal)

      cat("  - ideal_values:", paste(names(ideal_values), "=", round(ideal_values, 2), collapse=", "), "\n")

      # MK03: Identify key factors (attributes where ideal > cross-attribute average)
      cross_attr_avg <- mean(ideal_values, na.rm = TRUE)
      key_factor_names <- names(ideal_values[ideal_values > cross_attr_avg])

      cat("  - Cross-attribute average:", round(cross_attr_avg, 2), "\n")
      cat("  - Key factors:", paste(key_factor_names, collapse=", "), "\n")
      cat("  - Brands:", paste(ind$Variation, collapse=", "), "\n")

      # Filter out "Ideal" row if present
      df <- ind %>%
        filter(Variation != cfg_ideal_identifier) %>%
        mutate(Brand = Variation)

      # MK03: Calculate Score = count of key factors where brand value >= ideal value
      # Using 90% threshold for more realistic matching
      threshold_ratio <- 0.9

      ideal_attrs_col <- get_lang_text("ui.table.ideal_attributes", "達標屬性")

      # For each row, calculate score and find reached attributes
      score_list <- c()
      ideal_attrs_list <- lapply(1:nrow(df), function(i) {
        brand_row <- df[i, ]
        reached_attrs <- c()
        key_factor_count <- 0

        for (attr in names(ideal_values)) {
          if (attr %in% names(brand_row)) {
            brand_val <- as.numeric(brand_row[[attr]])
            ideal_val <- ideal_values[[attr]]

            if (!is.na(brand_val) && !is.na(ideal_val) && brand_val >= ideal_val * threshold_ratio) {
              # Only count and display key factors (not all attributes)
              if (attr %in% key_factor_names) {
                reached_attrs <- c(reached_attrs, attr)
                key_factor_count <- key_factor_count + 1
              }
            }
          }
        }

        # Store score for this brand
        score_list <<- c(score_list, key_factor_count)

        if (length(reached_attrs) == 0) {
          return(get_lang_text("labels.none", "無"))
        }
        paste(reached_attrs, collapse = "、")
      })

      df$Score <- score_list
      df[[ideal_attrs_col]] <- unlist(ideal_attrs_list)

      # Select and arrange final columns
      df <- df %>%
        select(Variation, Brand, Score, all_of(ideal_attrs_col)) %>%
        arrange(desc(Score))

      cat("  - After filtering 'Ideal':", nrow(df), "brands\n")
      cat("  ✅ Final ranking ready (MK03 key factor scoring)\n\n")

      # Render as DataTable with search and pagination
      # Following P_UI_DATATABLE principle: Use DT for interactive tables
      DT::datatable(
        df,
        rownames = FALSE,
        options = list(
          pageLength = cfg_table_page_length,
          searching = TRUE,
          language = list(
            search = get_lang_text("ui.table.search", "搜尋："),
            lengthMenu = get_lang_text("ui.table.length_menu", "顯示 _MENU_ 筆"),
            info = get_lang_text("ui.table.info", "顯示第 _START_ 至 _END_ 筆，共 _TOTAL_ 筆"),
            paginate = list(
              previous = get_lang_text("ui.table.previous", "上一頁"),
              `next` = get_lang_text("ui.table.next", "下一頁")
            )
          )
        )
      )
    })
  })
}

################################################################################
# End of module_brandedge_ideal_point.R
################################################################################

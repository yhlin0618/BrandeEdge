################################################################################
# BrandEdge Positioning Strategy Module
# 定位策略建議模組 - 四象限分析
# Version: 2.0
# Framework: InsightForge pattern
# Original: BrandEdge_premium/module.R lines 108-218 (strategyModule)
################################################################################
# MP104: ETL Data Flow Separation - Module consumes transformed data
# R092: Universal DBI Pattern - Uses reactive data pattern
# P: Configuration-driven development - Accepts module_config and lang_texts
################################################################################

library(shiny)
library(plotly)
library(dplyr)

# NULL 合併運算子 (Principle: Defensive programming)
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Positioning Strategy Module - UI
#'
#' @description
#' 定位策略四象限分析模組介面
#' 依據品牌關鍵屬性和非關鍵屬性的評分，將特徵分配到四個象限：
#' - 訴求（右上）：關鍵屬性且高於平均
#' - 改變（右下）：關鍵屬性但低於平均
#' - 改善（左上）：非關鍵屬性但高於平均
#' - 劣勢（左下）：非關鍵屬性且低於平均
#'
#' @param id Module namespace ID
#' @param module_config Module configuration from app_config.yaml (optional)
#' @param lang_texts Language texts reactive or list (optional)
#'
#' @return tagList containing UI elements
#'
#' @principle R092: Reactive data pattern for UI rendering
#' @principle P: Language-aware UI with fallback defaults
positioningStrategyModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # 獲取當前語言 (Principle: Language-aware rendering)
  current_language <- if (!is.null(lang_texts) && !is.null(lang_texts$language)) {
    lang_texts$language
  } else if (exists("get_current_language") && is.function(get_current_language)) {
    get_current_language()
  } else {
    "zh_TW"
  }

  # 獲取模組語言內容
  module_lang_content <- lang_texts %||% list(language = current_language)

  cat("🌍 [Positioning Strategy UI] 當前語言:", current_language, "\n")

  # 輔助函數：安全取得語言文字 (Principle: Defensive programming)
  get_lang_value <- function(path, default) {
    if (is.null(module_lang_content)) return(default)

    # 支援巢狀路徑 (e.g., "ui.select_brand.label")
    parts <- strsplit(path, "\\.")[[1]]
    result <- module_lang_content

    for (part in parts) {
      if (is.null(result) || !is.list(result)) return(default)
      result <- result[[part]]
      if (is.null(result)) return(default)
    }

    return(result)
  }

  # UI 元素文字 (Principle: Language-aware labels with defaults)
  run_strategy_label <- get_lang_value("ui.buttons.run_strategy", "策略探索")
  plot_height <- module_config$plot_height %||% "800px"

  # 返回 UI 結構 (Principle: Modular UI components)
  # 使用 uiOutput 渲染動態下拉選單，避免 renderUI 覆蓋問題
  tagList(
    fluidRow(
      column(12,
        div(
          style = "margin-bottom: 20px;",
          uiOutput(ns("variation_selector")),
          actionButton(
            ns("run_strategy"),
            label = run_strategy_label,
            class = "btn btn-primary",
            style = "margin-top: 10px;"
          )
        )
      )
    ),
    fluidRow(
      column(12,
        plotlyOutput(ns("strategy_plot"), height = plot_height)
      )
    ),
    fluidRow(
      column(12,
        div(
          style = "margin-top: 20px;",
          htmlOutput(ns("strategy_summary"))
        )
      )
    )
  )
}

#' Positioning Strategy Module - Server
#'
#' @description
#' 定位策略四象限分析邏輯
#' 核心業務邏輯：
#' 1. 根據選擇的 Variation 過濾數據
#' 2. 計算關鍵屬性 (key_vars) 和非關鍵屬性的評分總和
#' 3. 依據平均值分配到四個象限
#' 4. 使用 plotly 繪製四象限圖，每個象限以多列顯示特徵
#'
#' @param id Module namespace ID
#' @param data Reactive expression returning data frame with Variation and attributes
#' @param key_vars Reactive expression returning vector of key attribute names
#' @param lang_texts Reactive expression returning language texts (optional)
#' @param api_config API configuration from app_config$api$openai (共用模型設定)
#'
#' @return Module server function
#'
#' @principle MP064: Business logic in Derivation layer (this module)
#' @principle R092: Reactive data flow pattern
positioningStrategyModuleServer <- function(id, data, key_vars, lang_texts = reactive(NULL), module_config = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    # ========== Configuration Loading ==========
    # Load all configuration values with fallback defaults

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # Text layout configuration
    cfg_items_per_col <- if (!is.null(module_config$text_layout$items_per_column)) {
      module_config$text_layout$items_per_column
    } else { 6 } # ✅

    cfg_col_spacing <- if (!is.null(module_config$text_layout$column_spacing)) {
      module_config$text_layout$column_spacing
    } else { 3 } # ✅

    cfg_y_step <- if (!is.null(module_config$text_layout$y_step)) {
      module_config$text_layout$y_step
    } else { -1.5 } # ✅

    # Quadrant positions
    cfg_quadrants <- module_config$quadrant_positions
    if (!is.null(cfg_quadrants)) {
      appeal_pos <- cfg_quadrants$appeal$position
      change_pos <- cfg_quadrants$change$position
      improve_pos <- cfg_quadrants$improve$position
      weakness_pos <- cfg_quadrants$weakness$position
    } else {
      appeal_pos <- list(x = 5, y = 10)
      change_pos <- list(x = 5, y = -1)
      improve_pos <- list(x = -5, y = 10)
      weakness_pos <- list(x = -5, y = -1)
    } # ✅

    # Axis dimensions
    cfg_axis_range <- if (!is.null(module_config$axis_dimensions$range)) {
      module_config$axis_dimensions$range
    } else { c(-12, 12) } # ✅

    cfg_axis_line_y <- if (!is.null(module_config$axis_dimensions$center_line_y)) {
      module_config$axis_dimensions$center_line_y
    } else { c(-11, 11) } # ✅

    cfg_axis_line_x <- if (!is.null(module_config$axis_dimensions$center_line_x)) {
      module_config$axis_dimensions$center_line_x
    } else { c(-11, 11) } # ✅

    cfg_axis_line_width <- if (!is.null(module_config$axis_dimensions$line_width)) {
      module_config$axis_dimensions$line_width
    } else { 2 } # ✅

    # Font sizes
    cfg_title_font_size <- if (!is.null(module_config$font_sizes$quadrant_title)) {
      module_config$font_sizes$quadrant_title
    } else { 15 } # ✅

    cfg_text_font_size <- if (!is.null(module_config$font_sizes$feature_text)) {
      module_config$font_sizes$feature_text
    } else { 14 } # ✅

    cfg_global_font_size <- if (!is.null(module_config$font_sizes$global)) {
      module_config$font_sizes$global
    } else { 8 } # ✅

    # Margins
    cfg_margins <- if (!is.null(module_config$margins)) {
      module_config$margins
    } else {
      list(l = 60, r = 60, t = 60, b = 60)
    } # ✅

    # Title offset
    cfg_title_y_offset <- if (!is.null(module_config$text_layout$title_y_offset)) {
      module_config$text_layout$title_y_offset
    } else { 2 } # ✅

    message("📝 Positioning Strategy Module Config Loaded: items_per_col=", cfg_items_per_col,
            ", title_font=", cfg_title_font_size, ", text_font=", cfg_text_font_size)

    # ============================================
    # 🌍 語言管理狀態
    # ============================================
    current_language <- reactiveVal("zh_TW")
    module_lang_texts <- reactiveVal(NULL)

    # 輔助函數：安全取得語言文字 (Principle: Defensive programming)
    get_lang_text <- function(path, default = "") {
      texts <- module_lang_texts()
      if (is.null(texts)) return(default)

      # 解析路徑 (e.g., "quadrants.appeal")
      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (is.null(result) || !is.list(result)) return(default)
        result <- result[[part]]
        if (is.null(result)) return(default)
      }
      return(result)
    }

    # 初始化語言內容 (Principle: Reactive language initialization)
    observe({
      lang_data <- lang_texts()
      if (!is.null(lang_data)) {
        module_lang_texts(lang_data)
        if (!is.null(lang_data$language)) {
          current_language(lang_data$language)
        }
      }
    })

    # ============================================
    # 🔄 動態渲染 Variation 選擇器
    # ============================================
    # 使用 renderUI 避免外層 renderUI 覆蓋問題
    output$variation_selector <- renderUI({
      cat("\n🔍 [Positioning Strategy] === Rendering Variation Selector ===\n")

      df <- data()

      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
        cat("  ⚠️ No valid data yet\n")
        return(selectInput(
          session$ns("select_Variation"),
          label = get_lang_text("ui.select_variation.label", "選擇Variation"),
          choices = NULL,
          width = "300px"
        ))
      }

      if (!"Variation" %in% names(df)) {
        cat("  ❌ No 'Variation' column found!\n")
        return(selectInput(
          session$ns("select_Variation"),
          label = get_lang_text("ui.select_variation.label", "選擇Variation"),
          choices = NULL,
          width = "300px"
        ))
      }

      brands <- unique(df$Variation)
      cat("  ✅ Found", length(brands), "brands:", paste(brands, collapse=", "), "\n")

      selectInput(
        session$ns("select_Variation"),
        label = get_lang_text("ui.select_variation.label", "選擇Variation"),
        choices = brands,
        width = "300px"
      )
    })

    # ============================================
    # 📊 四象限策略圖表
    # ============================================
    # CRITICAL BUSINESS LOGIC - DO NOT MODIFY
    # Principle: Four-quadrant positioning analysis
    output$strategy_plot <- renderPlotly({
      req(input$select_Variation)

      # 取出當前 Variation 的 indicator row
      ind       <- data() %>% filter(Variation == input$select_Variation)
      key       <- key_vars()
      feats_key <- key
      feats_non <- setdiff(names(ind), c(key, "Variation"))

      # 計算關鍵屬性和非關鍵屬性的總分
      sums_key <- colSums(ind[feats_key])
      sums_non <- colSums(ind[feats_non])

      # 分象限邏輯 (CRITICAL: Do not modify logic, only labels)
      # 訴求：關鍵屬性且高於平均
      # 改變：關鍵屬性但低於平均
      # 改善：非關鍵屬性但高於平均
      # 劣勢：非關鍵屬性且低於平均

      # Get translated quadrant labels
      label_appeal <- get_lang_text("quadrants.appeal", "訴求")
      label_change <- get_lang_text("quadrants.change", "改變")
      label_improve <- get_lang_text("quadrants.improve", "改善")
      label_weakness <- get_lang_text("quadrants.weakness", "劣勢")

      quad_feats <- list()
      quad_feats[[label_appeal]] <- feats_key[sums_key >  mean(sums_key)]
      quad_feats[[label_change]] <- feats_key[sums_key <= mean(sums_key)]
      quad_feats[[label_improve]] <- feats_non[sums_non >  mean(sums_non)]
      quad_feats[[label_weakness]] <- feats_non[sums_non <= mean(sums_non)]

      # 輔助函數：把一組特徵拆成多列，每列最多N行 (Configuration-driven layout)
      # Principle: Multi-column text layout for readability
      make_multi_col <- function(feats, x_center, y_title, y_step = cfg_y_step) { # ✅
        n <- length(feats)
        if (n == 0) return(NULL)
        cols <- ceiling(n / cfg_items_per_col) # ✅ 需要的列數
        # split 會產生一個列表
        cols_list <- split(feats, rep(1:cols, each=cfg_items_per_col, length.out=n)) # ✅
        # 對每個子列表分別生成數據框
        dfs <- lapply(seq_along(cols_list), function(ci) {
          col_feats <- cols_list[[ci]]
          rows      <- length(col_feats)
          data.frame(
            text = col_feats,
            x    = x_center + (ci - (cols+1)/2) * cfg_col_spacing, # ✅
            y    = y_title + seq(1, by=y_step, length.out=rows)
          )
        })
        # dfs 本身就是一個 list，直接傳給 do.call
        do.call(rbind, dfs)
      }

      # 象限規格 (Configuration-driven positioning)
      specs <- list()
      specs[[label_appeal]] <- appeal_pos      # ✅ 右上：關鍵且優勢
      specs[[label_change]] <- change_pos      # ✅ 右下：關鍵但需改進
      specs[[label_improve]] <- improve_pos    # ✅ 左上：非關鍵但可改善
      specs[[label_weakness]] <- weakness_pos  # ✅ 左下：非關鍵且劣勢

      # 基礎空白座標系 (Configuration-driven layout)
      p <- plot_ly() %>%
        layout(
          shapes = list(
            list(type='line', x0=0, x1=0, y0=cfg_axis_line_y[1], y1=cfg_axis_line_y[2], line=list(width=cfg_axis_line_width)), # ✅
            list(type='line', x0=cfg_axis_line_x[1], x1=cfg_axis_line_x[2], y0=0, y1=0, line=list(width=cfg_axis_line_width)) # ✅
          ),
          xaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE, range=cfg_axis_range), # ✅
          yaxis=list(showgrid=FALSE, zeroline=FALSE, showticklabels=FALSE, range=cfg_axis_range), # ✅
          showlegend=FALSE
        )

      # 象限標題 (Configuration-driven font size)
      titles_list <- lapply(names(specs), function(nm) {
        sp <- specs[[nm]]
        data.frame(text = nm, x = sp$x, y = sp$y)
      })
      titles_df <- do.call(rbind, titles_list)
      p <- p %>% add_trace(
        data = titles_df, type='scatter', mode='text',
        x = ~x, y = ~y, text = ~text,
        textfont = list(size=cfg_title_font_size, color="black") # ✅
      )

      # 加入每個象限的特徵文字 (Configuration-driven layout)
      for (nm in names(quad_feats)) {
        sp    <- specs[[nm]]
        df_c  <- make_multi_col(quad_feats[[nm]], sp$x, sp$y - cfg_title_y_offset) # ✅
        if (!is.null(df_c)) {
          p <- p %>% add_trace(
            data = df_c, type='scatter', mode='text',
            x = ~x, y = ~y, text = ~text,
            textfont = list(size=cfg_text_font_size, color="blue") # ✅
          )
        }
      }

      # 最終佈局設定 (Configuration-driven margins and font)
      p %>% layout(
        margin = cfg_margins, # ✅
        font = list(size = cfg_global_font_size) # ✅
      )
    })

    # ============================================
    # 🤖 AI策略分析 - Strategy Exploration Button
    # ============================================
    # Reactive values for caching AI results
    rv <- reactiveValues(cache = list())

    # Handle strategy exploration button click
    observeEvent(input$run_strategy, {
      var_now <- req(input$select_Variation)

      # Get translated messages
      analyzing_msg <- get_lang_text("messages.processing.analyzing_strategy", "策略分析中...")

      withProgress(message = analyzing_msg, value = 0, {
        incProgress(0.2)

        # Get current brand data
        ind <- data() %>% filter(Variation == input$select_Variation)
        req(nrow(ind) > 0)

        key <- key_vars()
        feats_key <- key
        feats_non <- setdiff(names(ind), c(key, "Variation"))

        sums_key <- colSums(ind[feats_key, drop = FALSE])
        sums_non <- colSums(ind[feats_non, drop = FALSE])

        # Get translated quadrant labels
        label_appeal <- get_lang_text("quadrants.appeal", "訴求")
        label_change <- get_lang_text("quadrants.change", "改變")
        label_improve <- get_lang_text("quadrants.improve", "改善")
        label_weakness <- get_lang_text("quadrants.weakness", "劣勢")

        # Categorize features into quadrants
        quad_feats <- list(
          Variation = input$select_Variation
        )
        quad_feats[[label_appeal]] <- feats_key[sums_key > mean(sums_key)]
        quad_feats[[label_change]] <- feats_key[sums_key <= mean(sums_key)]
        quad_feats[[label_improve]] <- feats_non[sums_non > mean(sums_non)]
        quad_feats[[label_weakness]] <- feats_non[sums_non <= mean(sums_non)]

        features <- jsonlite::toJSON(quad_feats, auto_unbox = TRUE)
        incProgress(0.4)

        # Get current language for prompt
        current_lang <- if (!is.null(module_lang_texts()) && !is.null(module_lang_texts()$language)) {
          module_lang_texts()$language
        } else {
          "zh_TW"
        }

        # Load prompts for current language
        prompts_df <- tryCatch({
          source("utils/prompt_manager.R", local = TRUE)
          load_prompts(language = current_lang, app_name = "brandedge")
        }, error = function(e) {
          cat("⚠️ [Positioning Strategy] Failed to load prompts:", e$message, "\n")
          NULL
        })

        # Prepare GPT messages using prompt system
        messages <- tryCatch({
          prepare_gpt_messages(
            var_id = "positioning_strategy",
            variables = list(features = features),
            prompts_df = prompts_df
          )
        }, error = function(e) {
          cat("⚠️ [Positioning Strategy] Failed to prepare messages:", e$message, "\n")
          list(
            list(role = "system", content = "You are a brand positioning strategy expert."),
            list(role = "user", content = paste0("Analyze these features and provide strategic recommendations: ", features))
          )
        })

        # Call OpenAI API（使用共用 API 設定的模型）
        cat("🤖 [Positioning Strategy] Calling OpenAI API with model:", cfg_ai_model, "\n")
        txt <- tryCatch({
          source("utils/hint_system.R", local = TRUE)
          result <- chat_api(messages, model = cfg_ai_model)
          cat("✅ [Positioning Strategy] API call successful, result length:", nchar(result), "\n")
          result
        }, error = function(e) {
          cat("❌ [Positioning Strategy] API call failed:", e$message, "\n")
          error_msg <- get_lang_text("messages.error.api_failed", "❌ API調用失敗")
          paste(error_msg, ":", e$message)
        })

        # Cache the result
        cat("💾 [Positioning Strategy] Caching result for brand:", var_now, "\n")
        rv$cache[[var_now]] <- txt
        cat("📦 [Positioning Strategy] Cache now contains", length(rv$cache), "entries\n")
        incProgress(0.9)
      })
    })

    # ============================================
    # 📄 Strategy Summary Output
    # ============================================
    output$strategy_summary <- renderUI({
      cat("🖼️ [Positioning Strategy] Rendering strategy_summary output\n")
      var_now <- req(input$select_Variation)
      cat("🔍 [Positioning Strategy] Current brand:", var_now, "\n")
      cat("📦 [Positioning Strategy] Cache entries:", length(rv$cache), "\n")
      txt <- rv$cache[[var_now]]
      cat("📝 [Positioning Strategy] Retrieved txt is NULL:", is.null(txt), "\n")

      # If no analysis yet, show prompt message
      if (is.null(txt)) {
        prompt_msg <- get_lang_text("messages.info.click_strategy_button",
                                    "尚未產生策略分析，請點擊「策略探索」。")
        cat("ℹ️ [Positioning Strategy] Showing prompt message\n")
        return(HTML(paste0("<i style='color:gray'>", prompt_msg, "</i>")))
      }

      cat("✅ [Positioning Strategy] txt length:", nchar(txt), "\n")
      cat("📝 [Positioning Strategy] txt preview (first 200 chars):", substr(txt, 1, 200), "\n")

      # Strip code fences if present (fixed regex to only remove fence lines)
      res <- txt
      # Remove opening code fence (e.g., ```markdown or ```)
      res <- sub("^```[a-z]*\\s*\n", "", res, perl = TRUE)
      # Remove closing code fence
      res <- sub("\n```\\s*$", "", res, perl = TRUE)
      # Also handle if there's no newline before closing fence
      res <- sub("```\\s*$", "", res, perl = TRUE)
      cat("🔧 [Positioning Strategy] After stripping code fences, length:", nchar(res), "\n")
      if (nchar(res) > 0) {
        cat("📝 [Positioning Strategy] res preview (first 200 chars):", substr(res, 1, 200), "\n")
      }

      # Convert markdown to HTML
      html <- tryCatch({
        result <- markdown::markdownToHTML(text = res, fragment.only = TRUE)
        cat("✅ [Positioning Strategy] Markdown conversion succeeded, length:", nchar(result), "\n")

        # Check if result is empty or just whitespace
        if (is.null(result) || nchar(trimws(result)) == 0) {
          cat("⚠️ [Positioning Strategy] Markdown result is empty, using fallback\n")
          paste0("<div style='white-space: pre-wrap; padding: 15px; background: #f8f9fa; border-radius: 5px;'>",
                 gsub("\n", "<br>", res),
                 "</div>")
        } else {
          result
        }
      }, error = function(e) {
        cat("⚠️ [Positioning Strategy] Markdown conversion failed:", e$message, "\n")
        # Fallback: just wrap in <div> tags with styling
        fallback <- paste0("<div style='white-space: pre-wrap; padding: 15px; background: #f8f9fa; border-radius: 5px;'>",
                          gsub("\n", "<br>", res),
                          "</div>")
        cat("🔄 [Positioning Strategy] Using fallback HTML, length:", nchar(fallback), "\n")
        fallback
      })

      cat("📤 [Positioning Strategy] Final HTML length:", nchar(html), "\n")
      if (!is.null(html) && nchar(html) > 0) {
        cat("📤 [Positioning Strategy] Final HTML preview (first 200 chars):", substr(html, 1, 200), "\n")
      } else {
        cat("❌ [Positioning Strategy] WARNING: Final HTML is empty!\n")
        # Emergency fallback - just show the raw text
        html <- paste0("<div style='white-space: pre-wrap; padding: 15px; background: #fff3cd; border: 1px solid #ffc107; border-radius: 5px;'><strong>AI Analysis Result:</strong><br><br>",
                      gsub("\n", "<br>", txt),
                      "</div>")
      }
      HTML(html)
    })
  })
}

################################################################################
# Module Usage Example
################################################################################
# UI:
#   positioningStrategyModuleUI("strategy", module_config, lang_texts)
#
# Server:
#   positioningStrategyModuleServer("strategy",
#                                   data = reactive(indicator_data),
#                                   key_vars = reactive(c("屬性1", "屬性2")),
#                                   lang_texts = reactive_lang_texts)
#
# Data Requirements:
#   - data() must return data frame with 'Variation' column and attribute columns
#   - key_vars() must return vector of key attribute column names
#   - Attributes are divided into key (important) and non-key (others)
#
# Four Quadrants Logic:
#   - 訴求 (Appeal): Key attributes with above-average scores
#   - 改變 (Change): Key attributes with below-average scores
#   - 改善 (Improve): Non-key attributes with above-average scores
#   - 劣勢 (Weakness): Non-key attributes with below-average scores
################################################################################

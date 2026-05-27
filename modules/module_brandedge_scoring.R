################################################################################
# BrandEdge Scoring Module - Attribute Extraction & Scoring
################################################################################
# 此模組負責：
# 1. 從評論資料中萃取產品屬性（10-30個）
# 2. 對每則評論的每個屬性進行評分（1-5分）
# 3. 計算平均分數供後續分析模組使用
# Version: 1.0
# Last Updated: 2025-10-08
# Framework: InsightForge v4.0 + BrandEdge Framework

library(httr2)
library(jsonlite)
library(stringr)
library(dplyr)
# Note: future/furrr removed per CLOUD_P001 - parallel processing prohibited in Shiny Cloud

# Source prompt manager for language-aware AI prompts
source("utils/prompt_manager.R", encoding = "UTF-8")

# Source global chat_api for GPT-5 Responses API support
source("scripts/global_scripts/08_ai/fn_chat_api.R")

# NULL 合併運算子（已棄用，改用 ifelse）
# 注意：此運算符已不再使用，保留僅供參考
`%||%` <- function(x, y) {
  # 使用 isTRUE 確保返回單一邏輯值
  if (isTRUE(is.null(x)) || isTRUE(length(x) == 0)) y else x
}

################################################################################
# UI Function
################################################################################

#' BrandEdge Scoring Module UI
#' @param id module id
#' @param module_config Module configuration (optional)
#' @param lang_texts Static language texts (NOT reactive)
brandedgeScoringModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Helper function to get language text
  get_lang_text <- function(key, default = "") {
    if (is.null(lang_texts)) return(default)
    parts <- strsplit(key, "\\.")[[1]]
    value <- lang_texts
    for (part in parts) {
      if (is.list(value) && part %in% names(value)) {
        value <- value[[part]]
      } else {
        return(default)
      }
    }
    if (is.null(value)) default else as.character(value)
  }

  # Get translated texts
  title <- get_lang_text("title", "步驟 2：屬性評分")
  subtitle <- get_lang_text("subtitle", "萃取產品屬性並進行評分分析")

  # Section texts
  attr_section <- get_lang_text("ui.sections.attributes", "1. 屬性萃取設定")
  attr_count_label <- get_lang_text("ui.labels.attr_count", "屬性數量")
  gen_attr_btn <- get_lang_text("ui.buttons.generate_attributes", "產生屬性")

  scoring_section <- get_lang_text("ui.sections.scoring", "2. 評分設定")
  sample_size_label <- get_lang_text("ui.labels.sample_size", "每品牌抽樣評論數")
  start_scoring_btn <- get_lang_text("ui.buttons.start_scoring", "開始評分")

  results_section <- get_lang_text("ui.sections.results", "3. 評分結果")
  next_btn <- get_lang_text("ui.buttons.next", "下一步 ➡️")

  # Load configuration values from YAML (with fallback defaults)
  attr_min <- if (!is.null(module_config$attribute_extraction$num_attributes$min)) {
    module_config$attribute_extraction$num_attributes$min
  } else { 2 }  # 🆕 Changed from 10 to 2 to match config

  attr_max <- if (!is.null(module_config$attribute_extraction$num_attributes$max)) {
    module_config$attribute_extraction$num_attributes$max
  } else { 30 }

  attr_default <- if (!is.null(module_config$attribute_extraction$num_attributes$default)) {
    module_config$attribute_extraction$num_attributes$default
  } else { 15 }

  attr_step <- if (!is.null(module_config$attribute_extraction$num_attributes$step)) {
    module_config$attribute_extraction$num_attributes$step
  } else { 1 }

  # Scoring sampling configuration
  sample_min <- if (!is.null(module_config$scoring$review_sampling$min)) {
    module_config$scoring$review_sampling$min
  } else { 3 }  # 🆕 Changed from 10 to 3 to match config

  sample_max <- if (!is.null(module_config$scoring$review_sampling$max)) {
    module_config$scoring$review_sampling$max
  } else { 500 }

  sample_default <- if (!is.null(module_config$scoring$review_sampling$default)) {
    module_config$scoring$review_sampling$default
  } else { 50 }

  sample_step <- if (!is.null(module_config$scoring$review_sampling$step)) {
    module_config$scoring$review_sampling$step
  } else { 10 }

  # 🔍 Debug: Print config values to verify they're loaded
  cat("📝 [Scoring Module] Config values loaded:\n")
  cat("  - Attributes: min=", attr_min, ", max=", attr_max, ", default=", attr_default, ", step=", attr_step, "\n", sep="")
  cat("  - Reviews: min=", sample_min, ", max=", sample_max, ", default=", sample_default, ", step=", sample_step, "\n", sep="")

  # Verify if values came from config or fallback
  if (is.null(module_config$scoring$review_sampling$min)) {
    cat("  ⚠️ WARNING: review_sampling.min is NULL, using fallback!\n")
  } else {
    cat("  ✅ review_sampling.min loaded from config:", module_config$scoring$review_sampling$min, "\n")
  }

  tagList(
    div(
      id = ns("scoring_container"),
      class = "brandedge-scoring-module",

      # Header
      h4(icon("calculator"), " ", title),
      p(subtitle, style = "color: #666; margin-bottom: 20px;"),

      # 1. Attribute Extraction Section
      div(
        class = "section-box",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
        h5(icon("list"), " ", attr_section),
        p(get_lang_text("ui.descriptions.attr_extract", "系統將使用 AI 分析評論內容，自動識別最重要的產品屬性"),
          style = "color: #666; font-size: 0.95em;"),

        sliderInput(
          ns("num_attributes"),
          attr_count_label,
          min = attr_min,
          max = attr_max,
          value = attr_default,
          step = attr_step,
          ticks = FALSE
        ),

        actionButton(
          ns("gen_attributes"),
          gen_attr_btn,
          class = "btn-secondary",
          icon = icon("magic")
        ),

        br(), br(),

        # Generated attributes display
        div(
          id = ns("attributes_display"),
          uiOutput(ns("attributes_output"))
        )
      ),

      # 2. Scoring Section
      div(
        class = "section-box",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
        h5(icon("star"), " ", scoring_section),
        p(get_lang_text("ui.descriptions.scoring", "對抽樣評論進行屬性評分（1-5分制）"),
          style = "color: #666; font-size: 0.95em;"),

        sliderInput(
          ns("sample_size"),
          sample_size_label,
          min = sample_min,
          max = sample_max,
          value = sample_default,
          step = sample_step,
          ticks = FALSE
        ),

        actionButton(
          ns("start_scoring"),
          start_scoring_btn,
          class = "btn-primary",
          icon = icon("play")
        ),

        br(), br(),

        # Progress display
        div(
          id = ns("scoring_progress"),
          uiOutput(ns("progress_output"))
        )
      ),

      # 3. Results Section
      div(
        class = "section-box",
        style = "background: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
        h5(icon("table"), results_section),

        DTOutput(ns("results_table")),

        br(),

        actionButton(
          ns("proceed"),
          next_btn,
          class = "btn-success btn-lg",
          icon = icon("arrow-right")
        )
      )
    )
  )
}

################################################################################
# Server Function
################################################################################

#' BrandEdge Scoring Module Server
#' @param id module id
#' @param review_data Reactive containing review data from upload module
#' @param lang_texts Reactive language texts
#' @param con Database connection
#' @param user_info User information reactive
#' @param module_config Module configuration from YAML
#' @param api_config API configuration from app_config$api$openai (共用模型設定)
#' @return List containing scored_data reactive and proceed_scoring reactive trigger
brandedgeScoringModuleServer <- function(id, review_data, lang_texts = reactive(NULL), con = NULL, user_info = NULL, module_config = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    message("✅ BrandEdge Scoring Module Server 初始化")

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }
    message("📦 使用 AI 模型: ", cfg_ai_model)

    # Load configuration values from YAML (with fallback defaults)
    cfg_extract_sample_size <- if (!is.null(module_config$attribute_extraction$sampling_for_extraction$records_per_brand)) {
      module_config$attribute_extraction$sampling_for_extraction$records_per_brand
    } else { 30 }

    cfg_extract_max_reviews <- if (!is.null(module_config$attribute_extraction$sampling_for_extraction$max_total_reviews)) {
      module_config$attribute_extraction$sampling_for_extraction$max_total_reviews
    } else { 100 }

    cfg_extract_temperature <- if (!is.null(module_config$attribute_extraction$ai_settings$temperature)) {
      module_config$attribute_extraction$ai_settings$temperature
    } else { 0.3 }

    cfg_extract_max_tokens <- if (!is.null(module_config$attribute_extraction$ai_settings$max_tokens)) {
      module_config$attribute_extraction$ai_settings$max_tokens
    } else { 500 }

    cfg_scoring_temperature <- if (!is.null(module_config$scoring$ai_settings$temperature)) {
      module_config$scoring$ai_settings$temperature
    } else { 0.1 }

    cfg_scoring_max_tokens <- if (!is.null(module_config$scoring$ai_settings$max_tokens)) {
      module_config$scoring$ai_settings$max_tokens
    } else { 200 }

    cfg_parallel_workers <- if (!is.null(module_config$scoring$batch_processing$parallel_workers)) {
      module_config$scoring$batch_processing$parallel_workers
    } else { 4 }

    message("📝 配置載入完成: extract_temp=", cfg_extract_temperature,
            ", scoring_temp=", cfg_scoring_temperature,
            ", workers=", cfg_parallel_workers)

    # ========== Reactive Values ==========
    generated_attributes <- reactiveVal(NULL)
    scored_data <- reactiveVal(NULL)
    scoring_in_progress <- reactiveVal(FALSE)
    proceed_trigger <- reactiveVal(0)

    # ========== Helper Functions ==========

    # Get current language for prompt system
    get_current_language <- function() {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) NULL)

      # Try to extract language from texts metadata
      if (!is.null(texts) && !is.null(texts$metadata) && !is.null(texts$metadata$language)) {
        message("📌 [Language Debug] Found language in metadata: ", texts$metadata$language)
        return(texts$metadata$language)
      }

      # Try to get from global app_config
      if (exists("app_config", envir = .GlobalEnv)) {
        app_config <- get("app_config", envir = .GlobalEnv)
        if (!is.null(app_config$language) && !is.null(app_config$language$default)) {
          message("📌 [Language Debug] Using app_config language: ", app_config$language$default)
          return(app_config$language$default)
        }
      }

      # Fallback to Chinese
      message("⚠️ [Language Debug] No language found, falling back to zh_TW")
      return("zh_TW")
    }

    # Get language text helper
    get_msg <- function(path, default = "") {
      # ⚡ FIXED: Read from global_language_state instead of static lang_texts
      texts <- tryCatch({
        if (exists("global_language_state", envir = .GlobalEnv)) {
          lang_state <- get("global_language_state", envir = .GlobalEnv)

          if (!is.null(lang_state$language_content)) {
            current_lang <- lang_state$language_content$language

            # ⚡ FIX: Correct path is content$modules$brandedge_scoring
            if (!is.null(lang_state$language_content$content$modules)) {
              module_content <- lang_state$language_content$content$modules$brandedge_scoring
              module_content
            } else {
              NULL
            }
          } else {
            NULL
          }
        } else {
          # Fallback to lang_texts for backward compatibility
          if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
            lang_texts()
          } else {
            lang_texts
          }
        }
      }, error = function(e) {
        cat("⚠️ [get_msg ERROR]", path, "- Error:", e$message, "\n")
        NULL
      })

      if (is.null(texts)) {
        cat("⚠️ [get_msg] No texts available for path:", path, "- Using default:", default, "\n")
        return(default)
      }

      # Navigate path
      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          cat("⚠️ [get_msg] Path not found:", path, "at part:", part, "- Using default:", default, "\n")
          return(default)
        }
      }

      cat("✅ [get_msg] Retrieved:", path, "=", as.character(result), "\n")
      return(as.character(result))
    }

    # ========== Attribute Extraction Logic ==========
    # Note: chat_api() is sourced from scripts/global_scripts/08_ai/fn_chat_api.R
    # which correctly handles GPT-5 Responses API format

    observeEvent(input$gen_attributes, {
      req(review_data())
      data <- review_data()
      num_attrs <- input$num_attributes

      message(get_msg("console.starting_extraction", "🚀 開始屬性萃取，目標屬性數："), num_attrs)

      # Show progress bar with title (following InsightForge pattern)
      progress_title <- get_msg("messages.info.preparing_analysis", "正在分析評論內容，萃取產品屬性中...")

      # Check if shinyWidgets is available for progressSweetAlert
      if (requireNamespace("shinyWidgets", quietly = TRUE)) {
        shinyWidgets::progressSweetAlert(
          session = session,
          id = "extraction_progress",
          title = progress_title,
          display_pct = FALSE,
          value = 0
        )
      }

      # Show loading message in UI
      output$attributes_output <- renderUI({
        div(
          class = "alert alert-info",
          icon("spinner", class = "fa-spin"),
          " ", progress_title
        )
      })

      tryCatch({
        # Sample reviews for attribute extraction (from config)
        sampled_data <- data %>%
          group_by(Variation) %>%
          slice_head(n = cfg_extract_sample_size) %>%
          ungroup()

        # Prepare prompt for attribute extraction
        review_sample <- sampled_data %>%
          slice_head(n = min(cfg_extract_max_reviews, nrow(sampled_data))) %>%
          mutate(combined = paste(Title, Body, sep = " ")) %>%
          pull(combined) %>%
          paste(collapse = "\n---\n")

        # Get current language and use prompt_manager for language-aware prompts
        current_lang <- get_current_language()
        message("🌐 [Attribute Extraction] Using language: ", current_lang)

        # Prepare GPT messages using prompt_manager
        prompt_messages <- tryCatch({
          prepare_gpt_messages(
            var_id = "extract_attributes",
            variables = list(
              num_attributes = num_attrs,
              sample_reviews = review_sample
            ),
            language = current_lang,
            prompts_df = load_prompts(language = current_lang, app_name = "brandedge")
          )
        }, error = function(e) {
          message("⚠️ Prompt loading failed, using fallback: ", e$message)
          # Fallback to English
          list(
            list(
              role = "system",
              content = paste0(
                "You are a product analysis expert. Analyze customer reviews and extract ",
                num_attrs, " key product attributes that customers care about most. ",
                "Return ONLY a comma-separated list of attributes, no numbering, no explanations."
              )
            ),
            list(
              role = "user",
              content = paste0("Customer Reviews:\n\n", review_sample)
            )
          )
        })

        # Call OpenAI API (token usage is automatically tracked)
        # Note: temperature/max_tokens handled internally by global chat_api
        # 使用共用 API 設定的模型
        response <- chat_api(prompt_messages, model = cfg_ai_model)

        # Parse attributes
        attributes <- response %>%
          str_split(",") %>%
          unlist() %>%
          str_trim() %>%
          str_to_title() %>%
          unique()

        # Ensure we have the right number
        if (length(attributes) > num_attrs) {
          attributes <- attributes[1:num_attrs]
        } else if (length(attributes) < num_attrs) {
          message(get_msg("console.partial_extraction", "⚠️ 只萃取到"), " ", length(attributes), " 個屬性，少於目標數 ", num_attrs)
        }

        generated_attributes(attributes)
        message(get_msg("console.attributes_generated", "✅ Generated {count} attributes") %>%
                  gsub("\\{count\\}", length(attributes), .))

        # Close progress bar
        if (requireNamespace("shinyWidgets", quietly = TRUE)) {
          shinyWidgets::closeSweetAlert(session = session)
        }

        # Show notification to user (following InsightForge pattern)
        notify_template <- get_msg("messages.info.attributes_generated_notify", "✅ Generated {count} attributes!")
        attr_notify_msg <- gsub("\\{count\\}", length(attributes), notify_template)

        showNotification(
          attr_notify_msg,
          type = "message",
          duration = 3
        )

        # Display extracted attributes
        output$attributes_output <- renderUI({
          # Get display message
          display_msg <- get_msg("messages.info.attributes_generated", "✅ Generated {count} attributes: {attrs}")
          display_msg <- gsub("\\{count\\}", length(attributes), display_msg)
          display_msg <- gsub("\\{attrs\\}", "", display_msg)  # attrs shown in list below

          div(
            class = "alert alert-success",
            h6(icon("check-circle"), " ", display_msg),
            br(),
            tags$div(
              style = "background: white; padding: 15px; border-radius: 5px; border: 1px solid #dee2e6;",
              tags$ul(
                style = "columns: 2; -webkit-columns: 2; -moz-columns: 2; margin-bottom: 0; color: #333;",
                lapply(attributes, function(attr) {
                  tags$li(tags$strong(attr), style = "margin-bottom: 5px; color: #212529;")
                })
              )
            )
          )
        })

      }, error = function(e) {
        message(get_msg("console.extraction_failed", "❌ 屬性萃取失敗："), e$message)

        # Close progress bar on error
        if (requireNamespace("shinyWidgets", quietly = TRUE)) {
          shinyWidgets::closeSweetAlert(session = session)
        }

        # Show error notification
        error_msg <- get_msg("messages.error.extraction_failed", "屬性萃取失敗：")
        showNotification(
          paste0(error_msg, " ", e$message),
          type = "error",
          duration = 5
        )

        output$attributes_output <- renderUI({
          api_check <- get_msg("messages.error.check_api", "請確認 OpenAI API 設定正確且有足夠配額")

          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            " ", error_msg, e$message,
            br(), br(),
            api_check
          )
        })
      })
    })

    # ========== Scoring Logic ==========

    observeEvent(input$start_scoring, {
      req(review_data())
      req(generated_attributes())

      data <- review_data()
      attributes <- generated_attributes()
      sample_size <- input$sample_size

      message(get_msg("console.start_scoring", "🚀 開始評分，樣本數："), sample_size,
              get_msg("console.attributes_num", "，屬性數："), length(attributes))

      scoring_in_progress(TRUE)

      # Show progress
      output$progress_output <- renderUI({
        div(
          class = "alert alert-warning",
          icon("spinner", class = "fa-spin"),
          " ", get_msg("messages.info.scoring_in_progress", "正在評分中，請稍候... 這可能需要幾分鐘時間")
        )
      })

      tryCatch({
        # Sample reviews for scoring (up to sample_size per brand)
        sampled_reviews <- data %>%
          group_by(Variation) %>%
          slice_head(n = sample_size) %>%
          ungroup()

        message("📊 抽樣評論數：", nrow(sampled_reviews))
        total_reviews <- nrow(sampled_reviews)

        # Note: Parallel processing removed per CLOUD_P001
        # Shiny Cloud environments don't support furrr due to closure serialization limits
        # Using sequential processing with withProgress() instead (see lines ~657-680)

        # Get current language for scoring
        current_lang <- get_current_language()
        message("🌐 [Scoring] Using language: ", current_lang)

        # Track errors
        error_count <- 0

        # Prepare scoring function with language-aware prompts
        score_review <- function(review_text, attrs, lang = current_lang, review_idx = 1, brand_name = "") {
          tryCatch({
            # Use prompt_manager for language-aware scoring prompts
            prompt_messages <- tryCatch({
              prepare_gpt_messages(
                var_id = "score_attributes",
                variables = list(
                  attributes = paste(attrs, collapse = ", "),
                  review_text = review_text
                ),
                language = lang,
                prompts_df = load_prompts(language = lang, app_name = "brandedge")
              )
            }, error = function(e) {
              message("⚠️ Scoring prompt loading failed, using fallback: ", e$message)
              # Fallback prompt
              list(
                list(
                  role = "system",
                  content = paste0(
                    "You are a product reviewer. Rate the following review on these attributes: ",
                    paste(attrs, collapse = ", "),
                    ". For each attribute, give a score from 1-5 (1=very poor, 5=excellent). ",
                    "If the review doesn't mention an attribute, give it a score of 3 (neutral). ",
                    "Return ONLY comma-separated numbers in the same order as the attributes, ",
                    "no text, no explanations."
                  )
                ),
                list(
                  role = "user",
                  content = paste("Review:", review_text)
                )
              )
            })

            # Use chat_api (token usage is automatically tracked)
            # 使用共用 API 設定的模型
            response <- chat_api(prompt_messages, model = cfg_ai_model)

            # Parse scores
            scores <- response %>%
              str_extract_all("[1-5]") %>%
              unlist() %>%
              as.numeric()

            # Ensure correct length
            if (length(scores) < length(attrs)) {
              scores <- c(scores, rep(NA_real_, length(attrs) - length(scores)))
            } else if (length(scores) > length(attrs)) {
              scores <- scores[1:length(attrs)]
            }

            return(scores)

          }, error = function(e) {
            # Return NA on error (not forced 3)
            return(rep(NA_real_, length(attrs)))
          })
        }

        # Combine Title and Body for scoring
        sampled_reviews$combined_text <- paste(
          ifelse(is.na(sampled_reviews$Title) | sampled_reviews$Title == "", "", sampled_reviews$Title),
          ifelse(is.na(sampled_reviews$Body) | sampled_reviews$Body == "", "", sampled_reviews$Body),
          sep = " "
        )

        # ========================================================================
        # 循序評分處理（移除 furrr 平行處理 - CLOUD_P001）
        # - Shiny Cloud 環境不支援 furrr（closure 環境過大導致序列化失敗）
        # - 改用 for loop + 即時進度更新
        # ========================================================================

        # 準備評分所需的資料
        review_texts <- sampled_reviews$combined_text
        review_brands <- sampled_reviews$Variation

        message(sprintf("🚀 [Sequential Scoring] 開始循序評分 %d 則評論", total_reviews))

        # 循序評分 + 即時進度更新
        progress_msg <- get_msg("messages.info.scoring_in_progress", "正在評分中，請稍候...")
        all_scores <- withProgress(message = progress_msg, value = 0.1, {

          scores_list <- list()
          for (i in seq_len(total_reviews)) {
            # 即時更新進度（讓用戶知道不是卡住）
            setProgress(
              value = 0.1 + 0.8 * (i / total_reviews),
              detail = sprintf("評分中... %d/%d (%s)", i, total_reviews, review_brands[i])
            )

            scores_list[[i]] <- tryCatch({
              score_review(review_texts[i], attributes,
                          review_idx = i, brand_name = review_brands[i])
            }, error = function(e) {
              message(sprintf("⚠️ 評分錯誤 [%d]: %s", i, e$message))
              rep(NA_real_, length(attributes))
            })
          }

          setProgress(value = 0.9, detail = "整理評分結果...")
          scores_list
        })

        # 統計錯誤數（檢查 NA 結果）
        error_count <- sum(sapply(all_scores, function(x) all(is.na(x))))
        if (error_count > 0) {
          message(sprintf("⚠️ [Sequential Scoring] %d/%d 則評論評分失敗", error_count, total_reviews))
        }

        message(sprintf("✅ [Sequential Scoring] 完成！成功評分 %d/%d 則評論", total_reviews - error_count, total_reviews))

        # Convert to data frame
        scores_df <- do.call(rbind, all_scores) %>%
          as.data.frame()
        colnames(scores_df) <- attributes

        # Add Variation column
        scores_df$Variation <- sampled_reviews$Variation

        # Reorder columns (Variation first)
        scores_df <- scores_df %>%
          select(Variation, everything())

        # ✅ FIX: Aggregate by brand BEFORE storing
        # This matches the original BrandEdge app behavior
        cat("\n🔄 [Scoring Module] Aggregating scores by brand...\n")
        cat("  - Review-level rows:", nrow(scores_df), "\n")

        brand_scores <- scores_df %>%
          group_by(Variation) %>%
          summarise(across(where(is.numeric), ~mean(., na.rm = TRUE)), .groups = 'drop')

        # Filter out attributes with all NA or no variance (likely from API errors)
        original_ncol <- ncol(brand_scores)
        brand_scores <- brand_scores %>%
          select(Variation, where(~ {
            # Check if column is numeric and has valid variance
            result <- is.numeric(.) && !all(is.na(.)) && sd(., na.rm = TRUE) > 0
            # Return FALSE if result is NA (handles edge cases)
            isTRUE(result)
          }))

        removed_cols <- original_ncol - ncol(brand_scores)
        if (removed_cols > 0) {
          cat("  ⚠️ Removed", removed_cols, "attributes with all NA or no variance\n")
        }

        cat("  ✅ Aggregated to", nrow(brand_scores), "brands\n")
        cat("  - Brands:", paste(brand_scores$Variation, collapse=", "), "\n\n")

        # Store BRAND-AGGREGATED data (not review-level)
        scored_data(brand_scores)
        scoring_in_progress(FALSE)

        message(get_msg("console.scoring_complete", "✅ 評分完成，共"), " ", nrow(brand_scores), " ",
                get_msg("console.brands_count", "個品牌（來自 "), nrow(scores_df), " 則評論）")

        # Show success/warning notification with error count (following InsightForge pattern)
        if (error_count > 0) {
          # Show warning if there were errors
          scoring_warning_msg <- get_msg("messages.warning.scoring_warning",
                                         "⚠️ 評分完成，但有 {error_count}/{total} 筆資料因 API 錯誤而跳過")
          warning_msg <- gsub("\\{error_count\\}", error_count,
                             gsub("\\{total\\}", total_reviews, scoring_warning_msg))
          showNotification(warning_msg, type = "warning", duration = 10)
        } else {
          # Show success if no errors
          scoring_success_msg <- get_msg("messages.success.scoring_complete", "✅ 評分完成！成功處理 {total} 筆資料")
          success_msg <- gsub("\\{total\\}", total_reviews, scoring_success_msg)
          showNotification(success_msg, type = "message", duration = 5)
        }

        # Show appropriate UI message based on error count
        output$progress_output <- renderUI({
          if (error_count > 0) {
            # Warning message with error count
            div(
              class = "alert alert-warning",
              icon("exclamation-triangle"),
              " ", warning_msg
            )
          } else {
            # Success message
            div(
              class = "alert alert-success",
              icon("check-circle"),
              " ", success_msg
            )
          }
        })

        # Display results table
        output$results_table <- renderDT({
          req(scored_data())

          # ✅ scored_data() now already contains brand-aggregated scores
          # No need to aggregate again
          brand_scores <- scored_data() %>%
            mutate(across(where(is.numeric), ~ round(.x, 2)))

          datatable(
            brand_scores,
            options = list(
              pageLength = 10,
              scrollX = TRUE,
              dom = 'Bfrtip',
              buttons = c('copy', 'csv', 'excel')
            ),
            caption = get_msg("ui.tables.avg_scores_caption", "各品牌屬性平均分數")
          ) %>%
            formatStyle(
              columns = 2:ncol(brand_scores),
              backgroundColor = styleInterval(
                c(2.5, 3.5, 4.5),
                c('#f8d7da', '#fff3cd', '#d1ecf1', '#d4edda')
              )
            )
        })

      }, error = function(e) {
        message(get_msg("console.scoring_failed", "❌ 評分失敗："), e$message)
        scoring_in_progress(FALSE)

        # Show error notification
        error_msg <- get_msg("messages.error.scoring_failed", "評分失敗：")
        showNotification(
          paste0(error_msg, " ", e$message),
          type = "error",
          duration = 5
        )

        output$progress_output <- renderUI({
          div(
            class = "alert alert-danger",
            icon("exclamation-triangle"),
            " ", error_msg, e$message
          )
        })
      })
    })

    # ========== Proceed Button ==========

    observeEvent(input$proceed, {
      req(scored_data())

      proceed_trigger(proceed_trigger() + 1)

      showNotification(
        get_msg("messages.success.data_ready", "✅ 評分資料已準備完成，可以進行後續分析"),
        type = "message",
        duration = 3
      )
    })

    # ========== Return Values ==========

    return(list(
      scored_data = scored_data,
      attributes = generated_attributes,
      proceed_trigger = proceed_trigger
    ))
  })
}

message("✅ BrandEdge Scoring Module 載入完成")

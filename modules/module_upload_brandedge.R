################################################################################
# BrandEdge Upload Module - Framework Version
################################################################################

# 載入 UTF-8 清理函數
source("scripts/global_scripts/04_utils/string/fn_remove_illegal_utf8.R")

#' Upload & Preview Module – UI
#' @param id module id
#' @param module_config Module configuration (optional)
#' @param lang_texts Static language texts (NOT reactive)
uploadModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Extract texts with fallback
  # DO NOT call reactive() here - this runs during UI construction
  texts <- lang_texts
  ui_texts <- if (!is.null(texts) && !is.null(texts$ui)) texts$ui else list()
  buttons_texts <- if (!is.null(ui_texts$buttons)) ui_texts$buttons else list()

  div(id = ns("step1_box"),
      h4("步驟1: 只限定輸入某一特定產品類別（e.g. 滑鼠、筆電）"),

      fluidRow(
        column(
          width = 6,
          textInput(
            ns("primary_asin"),
            label = "請輸入您的商品 ASIN",
            value = "",
            placeholder = "例如：B09X18QWLT"
          )
        )
      ),
      br(),
      fluidRow(
        column(
          width = 6,
          numericInput(
            ns("reviews_per_asin"),
            label = "每個 ASIN 抓取評論數",
            value = 40,
            min = 1,
            max = 100,
            step = 5
          )
        )
      ),
      br(),
      h5("您是否有關心的競品，若有的話請輸入 ASIN"),
      fluidRow(
        column(
          width = 10,
          uiOutput(ns("competitor_asin_inputs"))
        ),
        column(
          width = 2,
          br(),
          actionButton(ns("add_competitor_asin"),
                       "+",
                       class = "btn-primary")
        )
      ),
      verbatimTextOutput(ns("step1_msg")),
      br(),
      actionButton(ns("to_step2"),
                  buttons_texts$next_step %||% "下一步 ➡️",
                  class = "btn-info"),
      br(),
      br(),
      uiOutput(ns("amazon_status")),
      br(),
      h5("Amazon 同產品類別競品"),
      DTOutput(ns("amazon_top10_tbl")),
      br(),
      h5("Amazon 顧客評論 Preview"),
      uiOutput(ns("amazon_review_status")),
      br(),
      h6("ASIN 評論抓取摘要"),
      DTOutput(ns("amazon_review_summary_tbl")),
      br(),
      h6("評論 Preview"),
      DTOutput(ns("amazon_review_preview_tbl"))
  )
}

#' Upload & Preview Module – Server
#'
#' @param con Database connection
#' @param user_info reactive – passed from login module
#' @param lang_texts Reactive language texts from unified_language_manager
#' @param module_config Module configuration from YAML
#' @return reactive containing the uploaded (raw) data.frame or NULL
uploadModuleServer <- function(id, con, user_info, lang_texts = reactive(NULL), module_config = NULL,
                               amazon_rank_results = reactive(NULL),
                               amazon_rank_status = reactive(NULL),
                               amazon_rank_error = reactive(NULL),
                               amazon_review_preview = reactive(NULL),
                               amazon_review_summary = reactive(NULL),
                               amazon_review_status = reactive(NULL),
                               amazon_review_error = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # 保留既有 review_data 介面，後續流程改接 asin_data。
    review_data <- reactiveVal(NULL)
    asin_data <- reactiveVal(NULL)
    competitor_asin_data <- reactiveVal(data.frame(asin = character(), source = character()))
    competitor_asin_count <- reactiveVal(1)
    max_competitor_asin_count <- 10

    message("📝 [upload_brandedge] ASIN 輸入模式啟用: max_competitor_asin_count=", max_competitor_asin_count)

    # Helper function to get message text
    get_msg <- function(path, fallback = "") {
      cat("\n🔍 [Upload Module get_msg] DEBUG START ===\n")
      cat("   Path requested:", path, "\n")
      cat("   Fallback:", fallback, "\n")

      # ⚡ FIXED: Read from global_language_state instead of static lang_texts
      texts <- tryCatch({
        if (exists("global_language_state", envir = .GlobalEnv)) {
          cat("   ✅ global_language_state EXISTS\n")
          lang_state <- get("global_language_state", envir = .GlobalEnv)

          if (!is.null(lang_state$language_content)) {
            current_lang <- lang_state$language_content$language
            cat("   Current language:", current_lang, "\n")

            # ⚡ FIX: Correct path is content$modules$upload_brandedge, not content$upload_brandedge
            if (!is.null(lang_state$language_content$content$modules)) {
              module_content <- lang_state$language_content$content$modules$upload_brandedge
              cat("   Module content retrieved:", !is.null(module_content), "\n")
              if (!is.null(module_content)) {
                cat("   Module content keys:", paste(names(module_content), collapse = ", "), "\n")
              }
              module_content
            } else {
              cat("   ⚠️ content$modules is NULL\n")
              NULL
            }
          } else {
            cat("   ⚠️ lang_state$language_content is NULL\n")
            NULL
          }
        } else {
          cat("   ⚠️ global_language_state DOES NOT EXIST - using fallback\n")
          # Fallback to lang_texts for backward compatibility
          if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
            lang_texts()
          } else {
            lang_texts
          }
        }
      }, error = function(e) {
        cat("   ❌ ERROR:", e$message, "\n")
        NULL
      })

      if (is.null(texts)) {
        cat("   ❌ texts is NULL - returning fallback:", fallback, "\n")
        cat("=== DEBUG END ===\n\n")
        return(fallback)
      }

      # Navigate path like "messages.upload.success"
      parts <- strsplit(path, "\\.")[[1]]
      cat("   Navigating path parts:", paste(parts, collapse = " -> "), "\n")
      result <- texts
      for (i in seq_along(parts)) {
        part <- parts[i]
        cat("   Step", i, "- Looking for:", part, "\n")
        if (!is.null(result[[part]])) {
          result <- result[[part]]
          cat("      ✅ Found (type:", class(result)[1], ")\n")
        } else {
          cat("      ❌ NOT FOUND - returning fallback\n")
          cat("      Available keys:", paste(names(result), collapse = ", "), "\n")
          cat("=== DEBUG END ===\n\n")
          return(fallback)
        }
      }
      cat("   ✅ FINAL RESULT:", as.character(result), "\n")
      cat("=== DEBUG END ===\n\n")
      return(result)
    }
    collect_competitor_asins <- reactive({
      raw_asins <- vapply(
        seq_len(competitor_asin_count()),
        function(index) {
          input[[paste0("competitor_asin_", index)]] %||% ""
        },
        character(1)
      )

      trimmed_asins <- trimws(raw_asins)
      trimmed_asins[nzchar(trimmed_asins)]
    })

    output$competitor_asin_inputs <- renderUI({
      tagList(
        lapply(seq_len(competitor_asin_count()), function(index) {
          textInput(
            session$ns(paste0("competitor_asin_", index)),
            label = paste0("競品 ASIN ", index),
            value = "",
            placeholder = "例如：B09X18QWLT"
          )
        })
      )
    })

    observeEvent(input$add_competitor_asin, {
      if (competitor_asin_count() >= max_competitor_asin_count) {
        showNotification(
          get_msg("messages.error.max_competitor_asin_count", "最多只能輸入 10 個競品 ASIN"),
          type = "warning"
        )
        return()
      }

      competitor_asin_count(competitor_asin_count() + 1)
    })

    output$amazon_status <- renderUI({
      status <- amazon_rank_status()
      error_message <- amazon_rank_error()

      if (!is.null(error_message) && nzchar(error_message)) {
        return(div(class = "alert alert-danger", error_message))
      }

      if (!is.null(status) && nzchar(status)) {
        alert_class <- if (identical(status, "Amazon 資料查詢中...")) {
          "alert alert-info"
        } else {
          "alert alert-success"
        }
        return(div(class = alert_class, status))
      }

      NULL
    })

    output$amazon_top10_tbl <- renderDT({
      results <- amazon_rank_results()
      if (is.null(results) || !is.data.frame(results) || nrow(results) == 0) {
        return(DT::datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
      }

      display_columns <- intersect(
        c("source_segment", "top_rank", "top_asin", "product_url", "source_category"),
        names(results)
      )

      DT::datatable(
        results[, display_columns, drop = FALSE],
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE,
        escape = FALSE
      )
    })

    output$amazon_review_status <- renderUI({
      status <- amazon_review_status()
      error_message <- amazon_review_error()

      if (!is.null(error_message) && nzchar(error_message)) {
        return(div(class = "alert alert-warning", error_message))
      }

      if (!is.null(status) && nzchar(status)) {
        alert_class <- if (grepl("查詢中", status, fixed = TRUE)) {
          "alert alert-info"
        } else {
          "alert alert-success"
        }
        return(div(class = alert_class, status))
      }

      div(class = "alert alert-secondary", "尚未執行 Amazon 評論抓取。")
    })

    output$amazon_review_summary_tbl <- renderDT({
      summary_df <- amazon_review_summary()
      if (is.null(summary_df) || !is.data.frame(summary_df) || nrow(summary_df) == 0) {
        return(DT::datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
      }

      display_columns <- intersect(
        c("asin", "reviews_requested", "reviews_found", "candidate_reviews_collected", "scrape_status", "status_message"),
        names(summary_df)
      )

      DT::datatable(
        summary_df[, display_columns, drop = FALSE],
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE
      )
    })

    output$amazon_review_preview_tbl <- renderDT({
      reviews <- amazon_review_preview()
      if (is.null(reviews) || !is.data.frame(reviews) || nrow(reviews) == 0) {
        return(DT::datatable(data.frame(), options = list(dom = "t"), rownames = FALSE))
      }

      display_columns <- intersect(
        c("asin", "star", "review_date", "title", "content", "author", "verified", "source"),
        names(reviews)
      )

      DT::datatable(
        reviews[, display_columns, drop = FALSE],
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # 「下一步」按鈕處理 - 先驗證 ASIN，後續爬蟲流程另接 asin_data。
    observeEvent(input$to_step2, {
      req(user_info())

      primary_asin <- trimws(input$primary_asin %||% "")
      if (!nzchar(primary_asin)) {
        msg <- get_msg("messages.error.no_asin_entered", "⚠️ 請輸入您的商品 ASIN")
        showNotification(msg, type = "error")
        return()
      }

      asin_data(data.frame(
        asin = primary_asin,
        stringsAsFactors = FALSE
      ))

      competitor_asins <- collect_competitor_asins()
      competitor_asin_data(data.frame(
        asin = competitor_asins,
        source = rep("user_competitor", length(competitor_asins)),
        stringsAsFactors = FALSE
      ))

      msg <- get_msg("messages.asin_ready", "✅ ASIN 已送出，後續將進行 Amazon 爬蟲處理")
      showNotification(msg, type = "message", duration = 5)
    })

    # ---- export ------------------------------------------------------------
    list(
      review_data  = reactive(review_data()),      # BrandEdge評論資料
      asin_data    = reactive(asin_data()),        # Amazon ASIN 輸入資料
      competitor_asin_data = reactive(competitor_asin_data()),
      reviews_per_asin = reactive({
        value <- suppressWarnings(as.integer(input$reviews_per_asin %||% 40L))
        if (is.na(value)) value <- 40L
        max(1L, min(100L, value))
      }),
      proceed_step = reactive({ input$to_step2 })   # a trigger to switch step outside
    )
  })
}
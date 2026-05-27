################################################################################
# Server Generator - 動態生成 Server 邏輯
################################################################################

#' Generate server logic dynamically based on configuration
#'
#' @param app_config Application configuration object
#' @param language_manager Language management object
#' @param module_loader Module loader functions
#' @return Server function for Shiny application
generate_server <- function(app_config, language_manager = NULL, module_loader = NULL) {

  # 返回 server 函數
  function(input, output, session) {

    # ==========================================
    # 初始化全局變數
    # ==========================================
    user_info <- reactiveVal(NULL)
    module_ui_update_trigger <- reactiveVal(0)
    module_instances <- list()

    # 設定登入狀態輸出
    output$user_logged_in <- reactive({
      !is.null(user_info())
    })
    outputOptions(output, "user_logged_in", suspendWhenHidden = FALSE)

    # ==========================================
    # 語言管理
    # ==========================================
    if (!is.null(language_manager)) {
      # 使用外部語言管理器
      global_lang_content <- language_manager$global_lang_content
      load_language_content <- language_manager$load_language_content
      get_module_texts <- language_manager$get_module_texts
    } else {
      # 使用預設語言管理
      source("utils/language_manager.R")
    }

    # ==========================================
    # 動態載入並初始化模組
    # ==========================================
    initialize_modules <- function() {
      cat("\n📦 === 初始化模組 ===\n")

      # 根據配置動態載入模組
      for (page in app_config$pages) {
        if (!is.null(page$module) && page$enabled) {
          module_name <- page$module
          module_id <- page$id

          cat("📦 載入模組:", module_name, "(ID:", module_id, ")\n")

          # 檢查模組檔案是否存在
          module_file <- paste0("modules/module_", module_name, ".R")
          if (file.exists(module_file)) {
            source(module_file)
          }

          # 處理命名映射關係
          name_mappings <- list(
            "scoring" = "score",
            "keyword_ads" = "keywordAds",
            "product_dev" = "productDev",
            "sales_model" = "salesModel",
            # BrandEdge submodule mappings
            "market_profile" = "marketProfile",
            "market_track" = "marketTrack",
            "advanced_attribute" = "advancedAttribute",
            "advanced_dna" = "advancedDNA",
            "brand_identity" = "brandIdentity"
          )

          base_name <- name_mappings[[module_name]]
          if (is.null(base_name)) base_name <- module_name

          # 動態載入模組 Server
          server_func_name <- paste0(base_name, "ModuleServer")

          if (exists(server_func_name)) {
            # 建立動態語言內容
            lang_texts <- reactive({
              current_lang <- global_lang_content()
              if (!is.null(current_lang)) {
                get_module_texts(current_lang, module_name)
              } else {
                NULL
              }
            })

            # 取得模組配置
            module_config <- app_config$module_configs[[module_name]]

            # 根據模組類型準備參數
            module_args <- list(
              id = module_id,
              module_config = module_config,
              lang_texts = lang_texts
            )

            # 特殊模組需要額外參數
            if (module_name == "login_v2") {
              module_args$con <- NULL  # 資料庫連接
              module_args$user_info <- user_info
            } else if (module_name %in% c("upload", "scoring", "sales_model")) {
              module_args$con <- NULL
              module_args$user_info <- user_info
              module_args$raw_data <- reactive({ NULL })  # 根據實際需求調整
            }

            # 初始化模組
            tryCatch({
              module_instances[[module_name]] <- do.call(
                server_func_name,
                module_args
              )
              cat("✅ 模組", module_name, "初始化成功\n")
            }, error = function(e) {
              cat("❌ 模組", module_name, "初始化失敗:", e$message, "\n")
            })
          } else {
            cat("⚠️ 找不到模組函數:", server_func_name, "\n")
          }
        }
      }

      cat("📦 === 模組初始化完成 ===\n\n")
    }

    # ==========================================
    # 動態 UI 渲染器
    # ==========================================
    setup_dynamic_ui_renderers <- function() {
      observe({
        module_ui_update_trigger()
        current_lang <- global_lang_content()

        if (!is.null(current_lang)) {
          cat("\n🎨 === 動態更新模組 UI ===\n")
          cat("🎨 當前語言:", current_lang$language, "\n")

          # 為每個啟用的模組生成 renderUI
          for (page in app_config$pages) {
            if (!is.null(page$module) && page$enabled) {
              local({
                page_id <- page$id
                module_name <- page$module
                output_id <- paste0(page_id, "_", module_name, "_ui_container")

                cat("🎨 更新模組 UI:", module_name, "\n")

                output[[output_id]] <- renderUI({
                  # 處理命名映射
                  name_mappings <- list(
                    "scoring" = "score",
                    "keyword_ads" = "keywordAds",
                    "product_dev" = "productDev",
                    "sales_model" = "salesModel",
                    "market_profile" = "marketProfile",
                    "market_track" = "marketTrack",
                    "advanced_attribute" = "advancedAttribute",
                    "advanced_dna" = "advancedDNA",
                    "brand_identity" = "brandIdentity"
                  )

                  base_name <- name_mappings[[module_name]]
                  if (is.null(base_name)) base_name <- module_name

                  ui_func_name <- paste0(base_name, "ModuleUI")
                  if (exists(ui_func_name)) {
                    module_config <- app_config$module_configs[[module_name]]
                    lang_texts <- get_module_texts(current_lang, module_name)

                    tryCatch({
                      do.call(ui_func_name, list(
                        id = page_id,
                        module_config = module_config,
                        lang_texts = lang_texts
                      ))
                    }, error = function(e) {
                      cat("❌ UI 渲染失敗:", module_name, "-", e$message, "\n")
                      div(
                        class = "alert alert-danger",
                        paste("模組載入失敗:", e$message)
                      )
                    })
                  }
                })
              })
            }
          }

          cat("🎨 === 模組 UI 更新完成 ===\n\n")
        }
      })
    }

    # ==========================================
    # 語言切換處理
    # ==========================================
    setup_language_handlers <- function() {
      # 統一的語言變更處理器
      observeEvent(input$global_language_change, {
        language_data <- input$global_language_change
        if (!is.null(language_data) && !is.null(language_data$language)) {
          handle_language_change(language_data)
        }
      }, ignoreInit = TRUE)

      # 監聽語言選擇下拉框
      observeEvent(input$language_selector, {
        new_lang <- input$language_selector
        if (!is.null(new_lang) && new_lang != "") {
          cat("\n🌍 語言選擇器變更:", new_lang, "\n")
          handle_language_change(list(
            language = new_lang,
            source = "dropdown"
          ))
        }
      }, ignoreInit = TRUE)
    }

    # 處理語言變更
    handle_language_change <- function(language_data) {
      cat("\n🌍 === 語言變更處理 ===\n")
      cat("🌍 請求語言:", language_data$language, "\n")
      cat("🌍 來源:", language_data$source %||% "unknown", "\n")

      # 載入新的語言內容
      tryCatch({
        # 根據語言代碼載入對應內容
        language_map <- c(
          "zh_TW" = "chinese",
          "en_US" = "english",
          "chinese" = "chinese",
          "english" = "english"
        )

        language_folder <- language_map[[language_data$language]]
        if (is.null(language_folder)) {
          language_folder <- "chinese"  # 預設中文
        }

        cat("🌍 載入語言資料夾:", language_folder, "\n")
        new_lang_content <- load_language_content(NULL, language = language_folder)

        # 確保語言標識正確
        new_lang_content$language <- language_data$language

        # 更新全局語言內容（已在 load_language_content 中自動同步，這裡不需要重複）
        # global_lang_content(new_lang_content)

        # 觸發 UI 更新
        module_ui_update_trigger(module_ui_update_trigger() + 1)

        # 顯示通知
        notification_msg <- if (language_data$language %in% c("en_US", "english")) {
          "Language switched to English"
        } else {
          "語言已切換至中文"
        }

        showNotification(
          notification_msg,
          type = "message",
          duration = 2
        )

        cat("🌍 === 語言變更完成 ===\n\n")

      }, error = function(e) {
        cat("❌ 語言切換失敗:", e$message, "\n")
        showNotification(
          "Language switch failed / 語言切換失敗",
          type = "error",
          duration = 3
        )
      })
    }

    # ==========================================
    # 事件處理器設定
    # ==========================================
    setup_event_handlers <- function() {
      # 登入成功處理
      observeEvent(input$login_success, {
        cat("\n🔐 === 登入成功事件 ===\n")
        user_data <- input$login_success
        if (!is.null(user_data)) {
          user_info(user_data)
          cat("🔐 用戶:", user_data$username %||% "unknown", "已登入\n")
        }
      }, ignoreInit = TRUE)

      # 登出處理
      observeEvent(input$logout, {
        cat("\n🔐 === 登出事件 ===\n")
        user_info(NULL)
        session$reload()
      }, ignoreInit = TRUE)

      # 模組間通訊
      if (!is.null(app_config$module_communications)) {
        for (comm in app_config$module_communications) {
          local({
            from_event <- comm$from
            to_action <- comm$to

            observeEvent(input[[from_event]], {
              event_data <- input[[from_event]]
              cat("\n📡 模組通訊:", from_event, "->", to_action, "\n")

              # 根據目標執行相應動作
              # 這裡可以擴展更多模組間通訊邏輯
            }, ignoreInit = TRUE)
          })
        }
      }
    }

    # ==========================================
    # 監控和除錯功能
    # ==========================================
    if (isTRUE(app_config$debug_mode)) {
      observe({
        cat("\n📊 === 系統狀態 ===\n")
        cat("📊 登入狀態:", !is.null(user_info()), "\n")
        cat("📊 當前語言:", global_lang_content()$language %||% "未設定", "\n")
        cat("📊 活躍模組數:", length(module_instances), "\n")
        cat("📊 === 狀態更新完成 ===\n\n")
      })
    }

    # ==========================================
    # 執行初始化
    # ==========================================
    cat("\n🚀 === 啟動應用程式 ===\n")
    cat("🚀 應用名稱:", app_config$app_name %||% "未命名", "\n")
    cat("🚀 版本:", app_config$version %||% "1.0.0", "\n")
    cat("🚀 除錯模式:", isTRUE(app_config$debug_mode), "\n")

    # 初始化各個組件
    initialize_modules()
    setup_dynamic_ui_renderers()
    setup_language_handlers()
    setup_event_handlers()

    cat("🚀 === 應用程式啟動完成 ===\n\n")

    # 返回模組實例供外部使用
    return(module_instances)
  }
}

# ==========================================
# 輔助函數
# ==========================================

#' Create module configuration from YAML
#'
#' @param config_path Path to configuration file
#' @return Processed configuration object
process_app_config <- function(config_path) {
  if (!file.exists(config_path)) {
    stop("Configuration file not found: ", config_path)
  }

  config <- yaml::read_yaml(config_path)

  # 處理預設值
  if (is.null(config$debug_mode)) {
    config$debug_mode <- FALSE
  }

  # 確保必要的結構存在
  if (is.null(config$pages)) {
    config$pages <- list()
  }

  if (is.null(config$module_configs)) {
    config$module_configs <- list()
  }

  return(config)
}

#' Extract enabled modules from configuration
#'
#' @param app_config Application configuration
#' @return Vector of enabled module names
get_enabled_modules <- function(app_config) {
  enabled_modules <- character(0)

  for (page in app_config$pages) {
    if (isTRUE(page$enabled) && !is.null(page$module)) {
      enabled_modules <- c(enabled_modules, page$module)
    }
  }

  return(unique(enabled_modules))
}
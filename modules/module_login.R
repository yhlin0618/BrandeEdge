################################################################################
# Login Module (Local) - YAML-Driven Text Management
# Converted from global module to support full YAML configuration
################################################################################

# 載入必要的資料存取工具
source("utils/data_access.R")  # 包含 tbl2 函數

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

#' Login & Register Module – UI (YAML-Driven)
#' @param id  module id
#' @param app_title  應用程式名稱 (從YAML載入)
#' @param app_icon   圖示路徑 (從配置載入)
#' @param lang_texts  語言內容 (從YAML載入)
#' @param contacts_md  聯絡資訊 markdown 檔案路徑 (預設: "md/contacts.md")
#' @param background_color  背景顏色 (預設: "#f5f6fa")
#' @param primary_color  主要色彩 (預設: "#007bff")
loginModuleUI <- function(id,
                         app_title = NULL,
                         app_icon = "assets/icons/app_icon.png",
                         lang_texts = NULL,
                         contacts_md = "md/contacts.md",
                         background_color = "#f5f6fa",
                         primary_color = "#007bff") {
  ns <- NS(id)

  # Load language configuration from insightforge.yaml
  language_choices <- tryCatch({
    insightforge_config <- yaml::read_yaml("config/yaml/insightforge_config/insightforge.yaml")
    languages_config <- yaml::read_yaml("config/languages.yaml")

    available_langs <- insightforge_config$language$available
    choices <- c()

    # Map language directory names to codes and display names
    for (lang_dir in available_langs) {
      lang_info <- languages_config$supported_languages[[
        which(sapply(languages_config$supported_languages, function(x) x$dir == lang_dir && x$enabled))
      ]]
      if (!is.null(lang_info)) {
        label <- paste(lang_info$flag, lang_info$display_name)
        choices[label] <- lang_info$code
      }
    }

    # Default language based on current lang_texts
    default_lang <- if (!is.null(lang_texts) && !is.null(lang_texts$fields$username)) {
      if (lang_texts$fields$username == "Username") "en_US"
      else if (lang_texts$fields$username == "ユーザー名") "ja_JP"
      else "zh_TW"
    } else {
      "zh_TW"
    }

    list(choices = choices, default = default_lang)
  }, error = function(e) {
    # Fallback if config files not available
    list(
      choices = c("🇹🇼 中文" = "zh_TW", "🇺🇸 English" = "en_US", "🇯🇵 日本語" = "ja_JP"),
      default = "zh_TW"
    )
  })

  tagList(
    tags$style(HTML(paste0(
      ".login-mobile-bg {",
      "  min-height: 100vh; display: flex; align-items: center; justify-content: center; background: ", background_color, "; }",
      ".login-mobile-card {",
      "  background: #fff; border-radius: 20px; box-shadow: 0 4px 24px rgba(0,0,0,0.08); padding: 2.5rem 1.5rem 2rem 1.5rem; max-width: 350px; width: 100%; margin: 0 auto; text-align: center; }",
      ".login-mobile-icon { margin-bottom: 1.5rem; }",
      ".login-mobile-title { font-size: 1.5rem; font-weight: 600; color: #222; margin-bottom: 1.5rem; }",
      ".login-mobile-btn { width: 100%; font-size: 1.1rem; padding: 0.75rem; border-radius: 8px; }",
      ".login-mobile-link { color: ", primary_color, "; margin-top: 1rem; display: block; }",
      ".login-mobile-form .form-group { text-align: left; }",
      ".login-mobile-form label { font-weight: 500; }",
      ".language-selector { position: absolute; top: 20px; right: 20px; z-index: 1000; }",
      ".language-selector select { border: 1px solid rgba(0,123,255,0.3); background: rgba(255,255,255,0.9);",
      "  color: #007bff; border-radius: 5px; padding: 0.4rem 0.6rem; font-size: 0.875rem; font-weight: 500;",
      "  cursor: pointer; transition: all 0.3s ease; backdrop-filter: blur(10px); box-shadow: 0 2px 8px rgba(0,0,0,0.1); }",
      ".language-selector select:hover { background: rgba(0,123,255,0.1); box-shadow: 0 4px 12px rgba(0,0,0,0.15); }",
      ".language-selector select:focus { outline: none; border-color: #007bff; box-shadow: 0 0 0 0.2rem rgba(0,123,255,0.25); }"
    ))),
    tags$script(HTML("
      // 完整的登入UI語言更新處理器
      Shiny.addCustomMessageHandler('update_login_ui', function(data) {
        console.log('更新登入UI語言:', data.language);

        // 更新輸入框標籤
        var usernameLabel = document.querySelector('label[for$=\"-login_user\"]');
        if (usernameLabel) usernameLabel.textContent = data.username_label;

        var passwordLabel = document.querySelector('label[for$=\"-login_pw\"]');
        if (passwordLabel) passwordLabel.textContent = data.password_label;

        var confirmPasswordLabel = document.querySelector('label[for$=\"-reg_pw2\"]');
        if (confirmPasswordLabel) confirmPasswordLabel.textContent = data.password_confirm_label;

        // 更新按鈕文字
        var loginBtn = document.querySelector('button[id$=\"-login_btn\"]');
        if (loginBtn) loginBtn.textContent = data.login_btn;

        var registerBtn = document.querySelector('button[id$=\"-register_btn\"]');
        if (registerBtn) registerBtn.textContent = data.register_btn;

        // 更新連結文字
        var registerLink = document.querySelector('a[id$=\"-to_register\"]');
        if (registerLink) registerLink.textContent = data.register_link;

        var loginLink = document.querySelector('a[id$=\"-to_login\"]');
        if (loginLink) loginLink.textContent = data.to_login;

        // 更新標題
        var appTitle = document.querySelector('.login-mobile-title');
        if (appTitle) appTitle.innerHTML = data.app_title;

        // 更新註冊頁面標題
        var registerTitle = document.querySelector('#' + data.moduleId + '-register_page h2');
        if (registerTitle) {
          registerTitle.textContent = data.language === 'en_US' ? 'Register New Account' : '註冊新帳號';
        }
      });

      // 初始化語言設定
      $(document).ready(function() {
        // 根據傳入的語言內容判斷初始語言
        var initialLang = 'zh_TW'; // 預設中文

        // 如果lang_texts存在且包含英文標記，則設定為英文
        if (window.initialLangTexts &&
            (window.initialLangTexts.fields && window.initialLangTexts.fields.username === 'Username')) {
          initialLang = 'en_US';
          console.log('🔔 JavaScript初始化：檢測到英文內容，設定為英文');
        } else {
          console.log('🔔 JavaScript初始化：使用中文');
        }

        // 確保按鈕狀態與語言內容一致
        console.log('🔔 JavaScript初始化：最終語言設定 =', initialLang);
      });
    ")),
    tags$script(HTML(paste0("
      // 設定初始語言內容
      window.initialLangTexts = ", if(!is.null(lang_texts)) jsonlite::toJSON(lang_texts, auto_unbox = TRUE) else "null", ";
    "))),
    div(class = "login-mobile-bg",
      # 語言選擇器 - 下拉選單
      div(class = "language-selector",
        selectInput(ns("language_selector"),
                   label = NULL,
                   choices = language_choices$choices,
                   selected = language_choices$default,
                   width = "140px")
      ),
      div(id = ns("login_page"), class = "login-mobile-card",
        div(class = "login-mobile-icon",
            img(src = app_icon, height = "90px", id = "icon_main")
        ),
        div(class = "login-mobile-title",
            HTML(app_title %||% "精準行銷平台")
        ),
        div(class = "login-mobile-form",
          textInput(ns("login_user"),
                   if (!is.null(lang_texts) && !is.null(lang_texts$fields$username)) lang_texts$fields$username else "帳號"),
          passwordInput(ns("login_pw"),
                       if (!is.null(lang_texts) && !is.null(lang_texts$fields$password)) lang_texts$fields$password else "密碼"),
          actionButton(ns("login_btn"),
                      if (!is.null(lang_texts) && !is.null(lang_texts$buttons$login)) lang_texts$buttons$login else "登入",
                      class = "btn-primary login-mobile-btn"),
          br(),
          actionLink(ns("to_register"),
                    if (!is.null(lang_texts) && !is.null(lang_texts$buttons$register)) lang_texts$buttons$register else "沒有帳號？點此註冊",
                    class = "login-mobile-link"),
          verbatimTextOutput(ns("login_msg"))
        )
      ),
      hidden(
        div(id = ns("register_page"), class = "login-mobile-card",
            h2(if (!is.null(lang_texts) && !is.null(lang_texts$buttons$register)) gsub("？.*", "", lang_texts$buttons$register) else "註冊新帳號",
               style = "font-size:1.2rem; margin-bottom:1.5rem;"),
            textInput(ns("reg_user"),
                     if (!is.null(lang_texts) && !is.null(lang_texts$fields$username)) lang_texts$fields$username else "帳號"),
            passwordInput(ns("reg_pw"),
                         if (!is.null(lang_texts) && !is.null(lang_texts$fields$password)) lang_texts$fields$password else "密碼"),
            passwordInput(ns("reg_pw2"),
                         if (!is.null(lang_texts) && !is.null(lang_texts$fields$password_confirm)) lang_texts$fields$password_confirm else "確認密碼"),
            actionButton(ns("register_btn"),
                        if (!is.null(lang_texts) && !is.null(lang_texts$buttons$register_btn)) lang_texts$buttons$register_btn else "註冊",
                        class = "btn-success login-mobile-btn"), br(),
            actionLink(ns("to_login"),
                      if (!is.null(lang_texts) && !is.null(lang_texts$buttons$to_login)) lang_texts$buttons$to_login else "已有帳號？回登入",
                      class = "login-mobile-link"),
            verbatimTextOutput(ns("register_msg"))
        )
      )
    )
  )
}

#' Login & Register Module – Server (YAML-Driven)
#' @param id module id
#' @param lang_texts 語言內容
#' @return reactiveValues list(user_info = reactive, logged_in = reactive, logout = function(), db_con = reactive)
loginModuleServer <- function(id, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {
    # ── 模組變數 ─────────────────────────────────────────────────────────────
    ns <- session$ns
    user_info <- reactiveVal(NULL)
    db_con <- reactiveVal(NULL)  # 延遲初始化的資料庫連接

    # NULL 合併運算子
    `%||%` <- function(x, y) {
      if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
    }

    # ── 延遲資料庫連接函數 ───────────────────────────────────────────────────
    get_db_connection <- function() {
      if (is.null(db_con())) {
        con <- tryCatch({
          # 嘗試連接資料庫
          source("database/db_connection.R")
          get_con()
        }, error = function(e) {
          # 使用語言內容或預設文字
          error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$db_connection_failed)) {
            gsub("\\{error\\}", e$message, lang_texts$console$db_connection_failed)
          } else {
            paste("⚠️ 資料庫連接失敗:", e$message)
          }
          cat(error_msg, "\n")
          NULL
        })

        if (!is.null(con)) {
          # 初始化資料表
          init_user_tables(con)
          db_con(con)
        }
      }
      return(db_con())
    }

    # ── 初始化使用者資料表 ───────────────────────────────────────────────────
    init_user_tables <- function(con) {
      if (inherits(con, "SQLiteConnection")) {
        # SQLite 語法
        dbExecute(con, "
          CREATE TABLE IF NOT EXISTS users (
            id           INTEGER PRIMARY KEY AUTOINCREMENT,
            username     TEXT UNIQUE,
            hash         TEXT,
            role         TEXT DEFAULT 'user',
            login_count  INTEGER DEFAULT 0
          );
        ")
      } else {
        # PostgreSQL 語法
        dbExecute(con, "
          CREATE TABLE IF NOT EXISTS users (
            id           SERIAL PRIMARY KEY,
            username     TEXT UNIQUE,
            hash         TEXT,
            role         TEXT DEFAULT 'user',
            login_count  INTEGER DEFAULT 0
          );
        ")
      }

      # 檢查並創建測試用戶
      existing_users <- dbGetQuery(con, "SELECT COUNT(*) as count FROM users")
      if (existing_users$count == 0) {
        # 使用語言內容或預設文字
        create_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$creating_test_user)) {
          lang_texts$console$creating_test_user
        } else {
          "📝 創建測試管理員用戶..."
        }
        cat(create_msg, "\n")
        dbExecute(con,
          "INSERT INTO users (username, hash, role, login_count) VALUES ($1, $2, 'admin', 0)",
          params = list("admin", bcrypt::hashpw("admin123"))
        )
      }
    }

    # ── UI 頁面切換 ──────────────────────────────────────────────────────────
    observeEvent(input$to_register, {
      shinyjs::hide("login_page")
      shinyjs::show("register_page")
    })

    observeEvent(input$to_login, {
      shinyjs::hide("register_page")
      shinyjs::show("login_page")
    })

    # ── 註冊處理 ────────────────────────────────────────────────────────────
    observeEvent(input$register_btn, {
      req(input$reg_user, input$reg_pw, input$reg_pw2)

      # 驗證密碼
      if (input$reg_pw != input$reg_pw2) {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$password_mismatch)) {
          lang_texts$console$password_mismatch
        } else {
          "❌ 兩次密碼不一致"
        }
        return(output$register_msg <- renderText(error_msg))
      }

      if (nchar(input$reg_pw) < 6) {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$password_too_short)) {
          lang_texts$console$password_too_short
        } else {
          "❌ 密碼至少需要6個字元"
        }
        return(output$register_msg <- renderText(error_msg))
      }

      # 延遲取得資料庫連接
      con <- get_db_connection()
      if (is.null(con)) {
        return(output$register_msg <- renderText("❌ 資料庫連接失敗"))
      }

      # 檢查帳號是否存在
      exists <- dbGetQuery(con,
        "SELECT 1 FROM users WHERE username = $1",
        params = list(input$reg_user)
      )

      if (nrow(exists) > 0) {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$user_exists)) {
          gsub("\\{username\\}", input$reg_user, lang_texts$console$user_exists)
        } else {
          "❌ 此帳號已被使用"
        }
        return(output$register_msg <- renderText(error_msg))
      }

      # 創建新用戶
      dbExecute(con,
        "INSERT INTO users (username, hash, role, login_count) VALUES ($1, $2, 'user', 0)",
        params = list(input$reg_user, bcrypt::hashpw(input$reg_pw))
      )

      success_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$register_success)) {
        gsub("\\{username\\}", input$reg_user, lang_texts$console$register_success)
      } else {
        "✅ 註冊成功！請登入"
      }
      output$register_msg <- renderText(success_msg)

      # 自動填入帳號並切換到登入頁
      updateTextInput(session, "login_user", value = input$reg_user)
      Sys.sleep(1)  # 短暫延遲讓用戶看到成功訊息
      shinyjs::hide("register_page")
      shinyjs::show("login_page")
    })

    # ── 登入處理 ────────────────────────────────────────────────────────────
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pw)

      # 顯示載入中
      output$login_msg <- renderText("驗證中...")

      # 延遲取得資料庫連接
      con <- get_db_connection()
      if (is.null(con)) {
        return(output$login_msg <- renderText("❌ 資料庫連接失敗，請稍後再試"))
      }

      # 查詢用戶
      user_row <- dbGetQuery(con,
        "SELECT * FROM users WHERE username = $1",
        params = list(input$login_user)
      )

      if (nrow(user_row) == 0) {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$user_not_found)) {
          gsub("\\{username\\}", input$login_user, lang_texts$console$user_not_found)
        } else {
          "❌ 帳號不存在"
        }
        return(output$login_msg <- renderText(error_msg))
      }

      # 檢查登入次數限制
      login_count <- user_row$login_count %||% 0
      if (user_row$role != "admin" && login_count >= 100) {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$console$login_limit_exceeded)) {
          lang_texts$console$login_limit_exceeded
        } else {
          "⛔ 已達登入次數上限，請聯絡管理員"
        }
        return(output$login_msg <- renderText(error_msg))
      }

      # 驗證密碼
      if (bcrypt::checkpw(input$login_pw, user_row$password_hash)) {
        # 更新登入次數（非管理員）
        if (user_row$role != "admin") {
          dbExecute(con,
            "UPDATE users SET login_count = login_count + 1 WHERE id = $1",
            params = list(user_row$id)
          )
        }

        # 設定用戶資訊
        user_info(user_row)

        success_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$messages$login_success)) {
          lang_texts$messages$login_success
        } else {
          "✅ 登入成功！"
        }
        output$login_msg <- renderText(success_msg)

        # 🔥 登入成功時，傳送語言設定到主應用
        current_lang <- current_language()
        if (!is.null(current_lang)) {
          cat("📤 登入成功，傳送語言設定到主應用:", current_lang, "\n")

          # 使用 sendCustomMessage 傳送語言變更到主應用
          session$sendCustomMessage("global_language_change", list(
            language = current_lang,
            content = current_lang
          ))

          cat("🌍 已傳送語言設定到主應用\n")
        }

        # 隱藏登入頁面
        shinyjs::hide("login_page")
        shinyjs::hide("register_page")
      } else {
        error_msg <- if (!is.null(lang_texts) && !is.null(lang_texts$messages$login_failed)) {
          lang_texts$messages$login_failed
        } else {
          "❌ 密碼錯誤"
        }
        output$login_msg <- renderText(error_msg)
      }
    })

    # ── 登出函數 ────────────────────────────────────────────────────────────
    logout <- function() {
      user_info(NULL)

      # 清空輸入欄位
      updateTextInput(session, "login_user", value = "")
      updateTextInput(session, "login_pw", value = "")

      # 顯示登入頁面
      shinyjs::show("login_page")
      shinyjs::hide("register_page")

      # 清除訊息
      output$login_msg <- renderUI(NULL)
      output$register_msg <- renderUI(NULL)
    }

    # ── 語言切換 ────────────────────────────────────────────────────────────
    current_language <- reactiveVal("en_US")
    lang_content_reactive <- reactiveVal(NULL)

    observeEvent(input$language_selector, {
      selected_lang <- input$language_selector
      cat("🌍 語言切換至:", selected_lang, "\n")

      # 實作語言切換邏輯 - 使用完整語言載入系統
      tryCatch({
        # 載入完整的語言內容結構
        if (exists("load_language_content")) {
          full_lang_content <- load_language_content(app_config, selected_lang)
          if (!is.null(full_lang_content)) {
            # 從完整結構中提取 login 模組內容
            new_lang_content <- full_lang_content$content$modules$login
            # 保存完整內容供後續使用（用於獲取 app title）
            full_lang_content_saved <- full_lang_content
            cat("📝 成功載入", selected_lang, "完整語言內容\n")

            # global_lang_content 會由 load_language_content 自動同步，不需要手動設置
            # 更新全局語言狀態（使用完整結構）
            # if (exists("global_lang_content", envir = .GlobalEnv)) {
            #   tryCatch({
            #     global_lang_func <- get("global_lang_content", envir = .GlobalEnv)
            #     if (is.function(global_lang_func)) {
            #       global_lang_func(full_lang_content)  # 使用完整結構
            #       cat("🌍 已更新全局語言狀態至:", selected_lang, "\n")
            #     }
            #   }, error = function(e) {
            #     cat("⚠️ 全局語言狀態更新失敗:", e$message, "\n")
            #   })
            # }
          } else {
            # 降級到直接載入文件
            lang_file <- if(selected_lang == "en_US") {
              "database/content/english/module/login.yaml"
            } else {
              "database/content/chinese/module/login.yaml"
            }
            if (file.exists(lang_file)) {
              new_lang_content <- yaml::read_yaml(lang_file)
              cat("📝 降級載入", selected_lang, "語言文件\n")
            }
          }
        } else {
          # 降級到直接載入文件
          lang_file <- if(selected_lang == "en_US") {
            "database/content/english/module/login.yaml"
          } else {
            "database/content/chinese/module/login.yaml"
          }
          if (file.exists(lang_file)) {
            new_lang_content <- yaml::read_yaml(lang_file)
            cat("📝 降級載入", selected_lang, "語言文件\n")
          }
        }

        if (!is.null(new_lang_content)) {

          # 更新響應式值
          current_language(selected_lang)
          lang_content_reactive(new_lang_content)

          # 處理語言內容結構
          # Get app title from common content (not login module content)
          # CORRECT PATH: result$content$common$app$title (NOT general!)
          app_title_from_common <- tryCatch({
            if (exists("full_lang_content_saved") && !is.null(full_lang_content_saved)) {
              full_lang_content_saved$content$common$app$title
            } else {
              NULL
            }
          }, error = function(e) {
            cat("⚠️ Error getting app title from common:", e$message, "\n")
            NULL
          })

          login_content <- list(
            username = new_lang_content$fields$username %||% "帳號",
            password = new_lang_content$fields$password %||% "密碼",
            password_confirm = new_lang_content$fields$password_confirm %||% "確認密碼",
            login_btn = new_lang_content$buttons$login %||% "登入",
            register_btn = new_lang_content$buttons$register_btn %||% "註冊",
            register_link = new_lang_content$buttons$register %||% "沒有帳號？點此註冊",
            to_login = new_lang_content$buttons$to_login %||% "已有帳號？回登入",
            app_title = app_title_from_common %||% "精準行銷平台"
          )

          # 更新UI文字 - 使用JavaScript更新所有元素
          session$sendCustomMessage("update_login_ui", list(
            language = selected_lang,
            moduleId = ns(""),
            username_label = login_content$username,
            password_label = login_content$password,
            password_confirm_label = login_content$password_confirm,
            login_btn = login_content$login_btn,
            register_btn = login_content$register_btn,
            register_link = login_content$register_link,
            to_login = login_content$to_login,
            app_title = login_content$app_title
          ))

          # 🔥 重要：更新全局語言狀態，讓主應用程式知道語言變更
          # 使用更直接的方法：通過parent session發送全局消息
          tryCatch({
            # 嘗試找到parent session
            if (exists("session", envir = parent.frame(5))) {
              parent_session <- get("session", envir = parent.frame(5))
              if (!is.null(parent_session)) {
                parent_session$sendCustomMessage("global_language_change", list(
                  language = selected_lang,
                  content = new_lang_content
                ))
                cat("🌍 已發送全局語言變更消息:", selected_lang, "\n")
              }
            }

          }, error = function(e) {
            cat("⚠️ 語言狀態更新失敗:", e$message, "\n")
          })

          # 顯示切換成功通知
          showNotification(
            if(selected_lang == "en_US") "Switched to English" else "已切換至中文",
            type = "message",
            duration = 2
          )
        }
      }, error = function(e) {
        cat("❌ 語言切換失敗:", e$message, "\n")
        showNotification(
          if(current_language() == "en_US") "Language switch failed" else "語言切換失敗",
          type = "error", duration = 3)
      })
    })

    # ── 返回值 ──────────────────────────────────────────────────────────────
    list(
      user_info = reactive(user_info()),
      logged_in = reactive(!is.null(user_info())),
      logout = logout,
      db_con = reactive(db_con()),  # 提供資料庫連接給主應用
      selected_language = reactive({
        # 如果使用者有選擇語言，使用使用者的選擇
        # 否則根據當前應用程式的語言設定決定預設值
        if (!is.null(input$language_selector)) {
          cat("🔔 登入模組：使用者選擇語言 =", input$language_selector, "\n")
          return(input$language_selector)
        } else {
          # 根據目前的 global_lang_content 決定預設語言
          current_lang <- if (exists("global_lang_content")) {
            isolate(global_lang_content())$language %||% "zh_TW"
          } else {
            "zh_TW"
          }
          cat("🔔 登入模組：使用預設語言 =", current_lang, "\n")
          return(current_lang)
        }
      })  # 提供語言選擇
    )
  })
}
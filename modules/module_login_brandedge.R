################################################################################
# BrandEdge 登入模組 - Framework Version
################################################################################

#' Login & Register Module – UI
#' @param id  module id
#' @param app_title Application title to display
#' @param app_icon Path to app icon
#' @param lang_texts Static language texts (NOT reactive)
#' @param contacts_md Path to contacts markdown (not used)
#' @param background_color Background color (not used)
#' @param primary_color Primary color (not used)
loginModuleUI <- function(id, app_title = "BrandEdge 旗艦版", app_icon = NULL,
                          lang_texts = NULL, contacts_md = NULL,
                          background_color = NULL, primary_color = NULL) {
  ns <- NS(id)

  # Extract texts with fallback
  # DO NOT call reactive() here - this runs during UI construction, not in a reactive context
  texts <- lang_texts
  ui_texts <- if (!is.null(texts) && !is.null(texts$ui)) texts$ui else list()
  login_texts <- if (!is.null(ui_texts$login)) ui_texts$login else list()
  register_texts <- if (!is.null(ui_texts$register)) ui_texts$register else list()

  tagList(
    tags$style(HTML(paste0(
      ".login-mobile-bg {",
      "  min-height: 100vh; display: flex; align-items: center; justify-content: center; background: #f5f6fa; }",
      ".login-mobile-card {",
      "  background: #fff; border-radius: 20px; box-shadow: 0 4px 24px rgba(0,0,0,0.08); padding: 2.5rem 1.5rem 2rem 1.5rem; max-width: 350px; width: 100%; margin: 0 auto; text-align: center; }",
      ".login-mobile-icon { margin-bottom: 1.5rem; }",
      ".login-mobile-title { font-size: 1.5rem; font-weight: 600; color: #222; margin-bottom: 1.5rem; }",
      ".login-mobile-btn { width: 100%; font-size: 1.1rem; padding: 0.75rem; border-radius: 8px; }",
      ".login-mobile-link { color: #007bff; margin-top: 1rem; display: block; }",
      ".login-mobile-form .form-group { text-align: left; }",
      ".login-mobile-form label { font-weight: 500; }"
    ))),
    div(class = "login-mobile-bg",
      div(id = ns("login_page"), class = "login-mobile-card",
        div(class = "login-mobile-icon",
            img(src = "icons/icon.png", height = "90px",
                alt = login_texts$logo_alt %||% "BrandEdge Logo")
        ),
        div(class = "login-mobile-title",
            app_title),
        div(class = "login-mobile-form",
          textInput(ns("login_user"),
                   login_texts$username_label %||% "帳號",
                   placeholder = login_texts$username_placeholder %||% "請輸入帳號"),
          passwordInput(ns("login_pw"),
                       login_texts$password_label %||% "密碼",
                       placeholder = login_texts$password_placeholder %||% "請輸入密碼"),
          actionButton(ns("login_btn"),
                      login_texts$login_button %||% "登入",
                      class = "btn-primary login-mobile-btn"),
          br(),
          actionLink(ns("to_register"),
                    login_texts$register_link %||% "沒有帳號？點此註冊",
                    class = "login-mobile-link"),
          p(login_texts$contact_info %||% "聯絡資訊: mr.no.one01@gmail.com",
            style = "margin-top: 1rem; font-size: 0.875rem; color: #6c757d;"),
          verbatimTextOutput(ns("login_msg"))
        )
      ),
      hidden(
        div(id = ns("register_page"), class = "login-mobile-card",
            h2(register_texts$title %||% "📝 註冊新帳號",
               style = "font-size:1.2rem; margin-bottom:1.5rem;"),
            textInput(ns("reg_user"),
                     register_texts$username_label %||% "帳號",
                     placeholder = register_texts$username_placeholder %||% "請輸入帳號"),
            passwordInput(ns("reg_pw"),
                         register_texts$password_label %||% "密碼",
                         placeholder = register_texts$password_placeholder %||% "請輸入密碼"),
            passwordInput(ns("reg_pw2"),
                         register_texts$password_confirm_label %||% "確認密碼",
                         placeholder = register_texts$password_confirm_placeholder %||% "請再次輸入密碼"),
            actionButton(ns("register_btn"),
                        register_texts$register_button %||% "註冊",
                        class = "btn-success login-mobile-btn"),
            br(),
            actionLink(ns("to_login"),
                      register_texts$login_link %||% "已有帳號？回登入",
                      class = "login-mobile-link"),
            p(register_texts$contact_info %||% "聯絡資訊: mr.no.one01@gmail.com",
              style = "margin-top: 0.5rem; font-size: 0.875rem; color: #6c757d;"),
            verbatimTextOutput(ns("register_msg"))
        )
      )
    )
  )
}

#' Login & Register Module – Server
#'
#' @param lang_texts Reactive language texts from unified_language_manager
#' @return reactiveValues list(user_info = reactive, logged_in = reactive, logout = function())
loginModuleServer <- function(id, lang_texts = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    # ── helpers --------------------------------------------------------------
    show  <- shinyjs::show
    hide  <- shinyjs::hide
    ns    <- session$ns

    user_info <- reactiveVal(NULL)

    # Helper function to get message text
    get_msg <- function(path, fallback = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) {
        NULL
      })
      if (is.null(texts)) return(fallback)

      # Navigate path like "messages.login.success"
      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          return(fallback)
        }
      }
      return(result)
    }

    # ---- UI page switches --------------------------------------------------
    observeEvent(input$to_register, { hide("login_page"); show("register_page") })
    observeEvent(input$to_login,    { hide("register_page"); show("login_page") })

    # ---- 註冊 --------------------------------------------------------------
    observeEvent(input$register_btn, {
      req(input$reg_user, input$reg_pw, input$reg_pw2)

      # Validate empty fields
      if (nchar(trimws(input$reg_user)) == 0 || nchar(trimws(input$reg_pw)) == 0 || nchar(trimws(input$reg_pw2)) == 0) {
        return(output$register_msg <- renderText(
          get_msg("messages.register.empty_fields", "❌ 請填寫所有欄位")
        ))
      }

      # Validate password match
      if (input$reg_pw != input$reg_pw2) {
        return(output$register_msg <- renderText(
          get_msg("messages.register.password_mismatch", "❌ 兩次密碼不一致")
        ))
      }

      # Validate password length
      if (nchar(input$reg_pw) < 6) {
        return(output$register_msg <- renderText(
          get_msg("messages.register.password_too_short", "❌ 密碼至少 6 個字元")
        ))
      }

      # Check if username exists
      exists <- db_query("SELECT 1 FROM users WHERE username=?", params = list(input$reg_user))
      if (nrow(exists)) {
        return(output$register_msg <- renderText(
          get_msg("messages.register.username_exists", "❌ 帳號已存在")
        ))
      }

      # Register user
      db_execute("INSERT INTO users (username,hash,role,login_count) VALUES (?,?, 'user',0)",
                params = list(input$reg_user, bcrypt::hashpw(input$reg_pw)))

      output$register_msg <- renderText(
        get_msg("messages.register.success", "✅ 註冊成功！請登入")
      )
      updateTextInput(session, "login_user", value = input$reg_user)
      hide("register_page"); show("login_page")
    })

    # ---- 登入 --------------------------------------------------------------
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pw)

      # Validate empty fields
      if (nchar(trimws(input$login_user)) == 0) {
        return(output$login_msg <- renderText(
          get_msg("messages.login.empty_username", "❌ 請輸入帳號")
        ))
      }
      if (nchar(trimws(input$login_pw)) == 0) {
        return(output$login_msg <- renderText(
          get_msg("messages.login.empty_password", "❌ 請輸入密碼")
        ))
      }

      # Query user
      row <- db_query("SELECT * FROM users WHERE username=?", params = list(input$login_user))
      if (!nrow(row)) {
        return(output$login_msg <- renderText(
          get_msg("messages.login.account_not_found", "❌ 帳號不存在")
        ))
      }

      # Check login limit
      used <- row$login_count %||% 0
      if (row$role != "admin" && used >= 100) {
        return(output$login_msg <- renderText(
          get_msg("messages.login.limit_reached", "⛔ 已達登入次數上限")
        ))
      }

      # Verify password
      if (bcrypt::checkpw(input$login_pw, row$password_hash)) {
        if (row$role != "admin")
          db_execute("UPDATE users SET login_count=login_count+1 WHERE id=?", params = list(row$id))

        # Set user info and show welcome message
        user_info(row)
        welcome_msg <- get_msg("messages.login.welcome", "歡迎回來，{username}！")
        welcome_msg <- gsub("\\{username\\}", row$username, welcome_msg)
        output$login_msg <- renderText(
          get_msg("messages.login.success", "✅ 登入成功！歡迎使用")
        )
      } else {
        output$login_msg <- renderText(
          get_msg("messages.login.password_incorrect", "❌ 密碼錯誤")
        )
      }
    })

    # ---- public reactive & logout ----------------------------------------
    observeEvent(user_info(), {
      if (!is.null(user_info())) {
        hide("login_page"); hide("register_page")
      }
    })

    # expose a logout trigger so parent can hide/show pages
    list(
      user_info = reactive(user_info()),
      logged_in = reactive(!is.null(user_info())),
      logout = function() {
        user_info(NULL)
        show("login_page")
        hide("register_page")
      }
    )
  })
}
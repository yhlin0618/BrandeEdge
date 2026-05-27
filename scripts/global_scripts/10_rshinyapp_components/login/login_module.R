################################################################################
# 1⃣  module_login.R -----------------------------------------------------------
################################################################################

#' @include ../../02_db_utils/tbl2/fn_tbl2.R
#'
#' @details
#' 本模組依賴資料存取函式 `tbl2()`（定義於
#' `scripts/global_scripts/02_db_utils/tbl2/fn_tbl2.R`）。
#' 該函式會在專案初始化（`autoinit()`）階段自動載入，因此本檔不再於程式碼中
#' 以 `source()` 方式載入，改以 roxygen2 `@include` 標籤宣告編譯期依賴。

#' Login & Register Module – UI
#' @param id  module id
#' @param app_title  應用程式名稱 (預設: "精準行銷平台")
#' @param app_icon   圖示路徑 (預設: "icons/icon.png")
#' @param contacts_md  聯絡資訊 markdown 檔案路徑 (預設: "md/contacts.md")
#' @param background_color  背景顏色 (預設: "#f5f6fa")
#' @param primary_color  主要色彩 (預設: "#007bff")
loginModuleUI <- function(id, 
                         app_title = "精準行銷平台",
                         app_icon = "icons/icon.png",
                         contacts_md = "md/contacts.md",
                         background_color = "#f5f6fa",
                         primary_color = "#007bff") {
  ns <- NS(id)
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
      ".login-mobile-form label { font-weight: 500; }"
    ))),
    div(class = "login-mobile-bg",
      div(id = ns("login_page"), class = "login-mobile-card",
        div(class = "login-mobile-icon",
            img(src = app_icon, height = "90px", id = "icon_main")
        ),
        div(class = "login-mobile-title", app_title),
        div(class = "login-mobile-form",
          textInput(ns("login_user"), "帳號"),
          passwordInput(ns("login_pw"), "密碼"),
          actionButton(ns("login_btn"), "登入", class = "btn-primary login-mobile-btn"),
          br(),
          actionLink(ns("to_register"), "沒有帳號？點此註冊", class = "login-mobile-link"),
          if (file.exists(contacts_md)) includeMarkdown(contacts_md),
          verbatimTextOutput(ns("login_msg"))
        )
      ),
      hidden(
        div(id = ns("register_page"), class = "login-mobile-card",
            h2("📝 註冊新帳號", style = "font-size:1.2rem; margin-bottom:1.5rem;"),
            textInput(ns("reg_user"), "帳號"),
            passwordInput(ns("reg_pw"), "密碼"),
            passwordInput(ns("reg_pw2"), "確認密碼"),
            actionButton(ns("register_btn"), "註冊", class = "btn-success login-mobile-btn"), br(),
            actionLink(ns("to_login"), "已有帳號？回登入", class = "login-mobile-link"),
            if (file.exists(contacts_md)) includeMarkdown(contacts_md),
            verbatimTextOutput(ns("register_msg"))
        )
      )
    )
  )
}

#' Login & Register Module – Server
#'
#' @return reactiveValues list(user_info = reactive, logged_in = reactive, logout = function())
loginModuleServer <- function(id, con) {
  moduleServer(id, function(input, output, session) {
    # ── helpers --------------------------------------------------------------
    show  <- shinyjs::show
    hide  <- shinyjs::hide
    ns    <- session$ns
    
    user_info <- reactiveVal(NULL)
    
    # ---- UI page switches --------------------------------------------------
    observeEvent(input$to_register, { hide("login_page"); show("register_page") })
    observeEvent(input$to_login,    { hide("register_page"); show("login_page") })
    
    # ---- 註冊 --------------------------------------------------------------
    observeEvent(input$register_btn, {
      req(input$reg_user, input$reg_pw, input$reg_pw2)
      if (input$reg_pw != input$reg_pw2)
        return(output$register_msg <- renderText("❌ 兩次密碼不一致"))
      if (nchar(input$reg_pw) < 6)
        return(output$register_msg <- renderText("❌ 密碼至少 6 個字元"))
      
      # 檢查帳號是否存在
      exists <- tbl2(con, "users") %>%
        filter(username == !!input$reg_user) %>%
        collect()
      
      if (nrow(exists) > 0)
        return(output$register_msg <- renderText("❌ 帳號已存在"))
      
      # 新增使用者記錄
      new_user <- data.frame(
        username = input$reg_user,
        hash = bcrypt::hashpw(input$reg_pw),
        role = 'user',
        login_count = 0L
      )
      
      # 使用 DBI 直接插入新記錄
      DBI::dbWriteTable(con, "users", new_user, append = TRUE)
      output$register_msg <- renderText("✅ 註冊成功！請登入")
      updateTextInput(session, "login_user", value = input$reg_user)
      hide("register_page"); show("login_page")
    })
    
    # ---- 登入 --------------------------------------------------------------
    observeEvent(input$login_btn, {
      req(input$login_user, input$login_pw)
      # 查詢使用者資料
      user_data <- tbl2(con, "users") %>%
        filter(username == !!input$login_user) %>%
        collect()
      
      if (nrow(user_data) == 0) 
        return(output$login_msg <- renderText("❌ 帳號不存在"))
      
      row <- user_data[1, ]  # 取第一筆記錄
      used <- row$login_count %||% 0
      
      if (row$role != "admin" && used >= 5)
        return(output$login_msg <- renderText("⛔ 已達登入次數上限"))
      
      if (bcrypt::checkpw(input$login_pw, row$password_hash)) {
        if (row$role != "admin") {
          # 更新登入次數
          current_count <- tbl2(con, "users") %>%
            filter(id == !!row$id) %>%
            select(login_count) %>%
            collect() %>%
            pull(login_count)
          
          # 更新記錄 (使用正確的參數語法)
          if (inherits(con, "PqConnection")) {
            # PostgreSQL syntax
            DBI::dbExecute(con, "UPDATE users SET login_count = $1 WHERE id = $2", 
                          params = list(current_count + 1L, row$id))
          } else {
            # SQLite syntax
            DBI::dbExecute(con, "UPDATE users SET login_count = ? WHERE id = ?", 
                          params = list(current_count + 1L, row$id))
          }
          
          # 更新 row 資料以反映新的登入次數
          row$login_count <- current_count + 1L
        }
        user_info(row)
      } else {
        output$login_msg <- renderText("❌ 帳號或密碼錯誤")
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
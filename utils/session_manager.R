# ==========================================
# Session 管理模組
# ==========================================
# 處理 session 中斷和恢復

# Session 管理器 R6 類別
SessionManager <- R6::R6Class(
  "SessionManager",

  public = list(
    session = NULL,
    user_info = NULL,
    current_page = NULL,
    module_states = NULL,

    initialize = function(session) {
      self$session <- session
      self$module_states <- list()

      # 設定 session 結束處理
      session$onSessionEnded(function() {
        self$save_session_state()
      })

      # 設定 session 恢復檢查
      self$restore_session_state()

      message("📌 Session 管理器已初始化")
    },

    # 儲存 session 狀態
    save_session_state = function() {
      if (!is.null(self$user_info)) {
        session_data <- list(
          user_info = self$user_info,
          current_page = self$current_page,
          module_states = self$module_states,
          timestamp = Sys.time()
        )

        # 儲存到暫存檔
        session_file <- file.path(
          tempdir(),
          paste0("session_", self$user_info$id, ".rds")
        )

        tryCatch({
          saveRDS(session_data, session_file)
          message("💾 Session 狀態已儲存")
        }, error = function(e) {
          message("⚠️ 無法儲存 session 狀態: ", e$message)
        })
      }
    },

    # 恢復 session 狀態
    restore_session_state = function() {
      # 嘗試從 URL 參數或 cookie 取得 user_id
      query <- parseQueryString(self$session$clientData$url_search)

      if (!is.null(query$user_id)) {
        session_file <- file.path(
          tempdir(),
          paste0("session_", query$user_id, ".rds")
        )

        if (file.exists(session_file)) {
          tryCatch({
            session_data <- readRDS(session_file)

            # 檢查 session 是否過期（超過 30 分鐘）
            if (difftime(Sys.time(), session_data$timestamp, units = "mins") < 30) {
              self$user_info <- session_data$user_info
              self$current_page <- session_data$current_page
              self$module_states <- session_data$module_states

              message("✅ Session 狀態已恢復")
              return(TRUE)
            } else {
              message("⏱️ Session 已過期")
              unlink(session_file)
            }
          }, error = function(e) {
            message("⚠️ 無法恢復 session 狀態: ", e$message)
          })
        }
      }

      return(FALSE)
    },

    # 更新使用者資訊
    set_user_info = function(user_info) {
      self$user_info <- user_info
      self$save_session_state()
    },

    # 更新當前頁面
    set_current_page = function(page) {
      self$current_page <- page
      self$save_session_state()
    },

    # 儲存模組狀態
    save_module_state = function(module_id, state) {
      self$module_states[[module_id]] <- state
      self$save_session_state()
    },

    # 取得模組狀態
    get_module_state = function(module_id) {
      return(self$module_states[[module_id]])
    },

    # 清除 session
    clear_session = function() {
      if (!is.null(self$user_info)) {
        session_file <- file.path(
          tempdir(),
          paste0("session_", self$user_info$id, ".rds")
        )

        if (file.exists(session_file)) {
          unlink(session_file)
        }
      }

      self$user_info <- NULL
      self$current_page <- NULL
      self$module_states <- list()

      message("🗑️ Session 已清除")
    }
  )
)

# 輔助函數：建立 session 管理器
create_session_manager <- function(session) {
  SessionManager$new(session)
}

# 輔助函數：處理 session 中斷
handle_session_interruption <- function(session, callback = NULL) {
  # 監聽客戶端斷線事件
  observeEvent(session$clientData$url_search, {
    if (grepl("reconnect=true", session$clientData$url_search)) {
      message("🔄 偵測到 session 重新連接")

      if (!is.null(callback)) {
        callback()
      }
    }
  })

  # 定期檢查 session 狀態
  observe({
    invalidateLater(30000)  # 每 30 秒檢查一次

    if (session$isClosed()) {
      message("⚠️ Session 已關閉")
    } else {
      # Session 仍然活躍
      session$sendCustomMessage("heartbeat", list(
        timestamp = Sys.time(),
        status = "active"
      ))
    }
  })
}
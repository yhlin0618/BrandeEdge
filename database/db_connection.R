# ============================================================================
# InsightForge 資料庫連接模組 - Connection Pooling 版本
# ============================================================================
#
# 更新日期: 2025-01-07
# 更新原因: 修復並發用戶崩潰問題（4+ 用戶時連接耗盡）
# 解決方案: 使用 pool 套件實現連接池，所有 session 共享連接
#
# ============================================================================

# ── 載入必要套件 ─────────────────────────────────────────────────────────────
library(pool)
library(DBI)

# ── 全局連接池（所有 session 共享）─────────────────────────────────────────
db_pool <- NULL
db_pool_info <- NULL
pool_initialized <- FALSE

# ── 初始化連接池 ─────────────────────────────────────────────────────────────
init_pool <- function(force = FALSE) {
  # 如果已初始化且 pool 有效，不重複建立
  if (!force && pool_initialized && !is.null(db_pool)) {
    if (inherits(db_pool, "Pool") && pool::dbIsValid(db_pool)) {
      return(invisible(TRUE))
    }
  }

  # 載入 dotenv（如果尚未載入）
  if (!nzchar(Sys.getenv("SUPABASE_DB_HOST"))) {
    if (file.exists(".env")) {
      if (requireNamespace("dotenv", quietly = TRUE)) {
        dotenv::load_dot_env(file = ".env")
        cat("[Pool] 已載入 .env 檔案\n")
      }
    }
  }

  # 嘗試建立 PostgreSQL 連接池
  pg_success <- tryCatch({
    db_config <- get_config("db")

    # 診斷：檢查環境變數是否正確載入
    if (is.null(db_config$host) || !nzchar(db_config$host)) {
      cat("[Pool] ⚠️ SUPABASE_DB_HOST 未設定！\n")
      cat("[Pool]   檢查 .env 檔案或 Posit Connect Variable Set\n")
      cat("[Pool]   目前 SUPABASE_DB_HOST =", Sys.getenv("SUPABASE_DB_HOST", "(空)"), "\n")
      return(FALSE)
    }

    if (!nzchar(db_config$password)) {
      cat("[Pool] ⚠️ SUPABASE_DB_PASSWORD 未設定！\n")
      return(FALSE)
    }

    # 所有檢查通過，建立連接池
    cat("[Pool] 建立 PostgreSQL 連接池...\n")
    cat("[Pool] Host:", substr(db_config$host, 1, 30), "...\n")
    cat("[Pool] User:", db_config$user, "\n")

    # 使用 pool 套件建立連接池
    # gssencmode = "disable" 避免 Supabase Pooler GSSAPI 錯誤
    db_pool <<- pool::dbPool(
      drv = RPostgres::Postgres(),
      host     = db_config$host,
      port     = as.integer(db_config$port %||% 5432),
      user     = db_config$user,
      password = db_config$password,
      dbname   = db_config$dbname,
      sslmode  = db_config$sslmode %||% "require",
      gssencmode = "disable",  # 禁用 GSSAPI 加密（Supabase Pooler 需要）
      minSize  = 2,           # 最小連接數
      maxSize  = 15,          # 最大連接數（支援 4 workers × 多並發用戶）
      idleTimeout = 60000,    # 閒置 60 秒後回收
      validationInterval = 60 # 每 60 秒驗證連接
    )

    db_pool_info <<- list(
      type = "PostgreSQL",
      host = db_config$host,
      port = db_config$port,
      dbname = db_config$dbname,
      icon = "🐘",
      color = "#336791",
      status = "正式環境 (Pool)",
      pool_size = "2-10"
    )

    cat("[Pool] ✅ PostgreSQL 連接池建立成功 (minSize=2, maxSize=15)\n")
    TRUE
  }, error = function(e) {
    cat("[Pool] PostgreSQL 連接池建立失敗:", e$message, "\n")
    FALSE
  })

  # 如果 PostgreSQL 失敗，使用 SQLite（無法用 pool，用單一連接）
  if (!pg_success) {
    cat("[Pool] 切換到 SQLite 本地測試模式（單一連接）\n")

    writable_dir <- Sys.getenv("CONNECT_CONTENTS_DIR")
    if (nzchar(writable_dir)) {
      writable_dir <- file.path(writable_dir, "sqlite")
    } else {
      writable_dir <- file.path(tempdir(), "insightforge_sqlite")
    }

    if (!dir.exists(writable_dir)) {
      dir.create(writable_dir, recursive = TRUE, showWarnings = FALSE)
    }

    db_path <- file.path(writable_dir, "insightforge_test.db")

    db_pool <<- tryCatch({
      DBI::dbConnect(RSQLite::SQLite(), db_path)
    }, error = function(e) {
      stop("[Pool] SQLite 連接失敗: ", e$message)
    })

    db_pool_info <<- list(
      type = "SQLite",
      path = db_path,
      icon = "📁",
      color = "#FF8C00",
      status = "本地測試 (單一連接)",
      pool_size = "1"
    )

    cat("[Pool] SQLite 路徑:", db_path, "\n")
  }

  # 初始化表格
  init_tables()

  pool_initialized <<- TRUE
  invisible(TRUE)
}

# ── 初始化表格（只執行一次）────────────────────────────────────────────────
init_tables <- function() {
  if (is.null(db_pool)) return(invisible(FALSE))

  is_sqlite <- inherits(db_pool, "SQLiteConnection")

  tryCatch({
    if (is_sqlite) {
      # SQLite 語法
      DBI::dbExecute(db_pool, "
        CREATE TABLE IF NOT EXISTS users (
          id           INTEGER PRIMARY KEY AUTOINCREMENT,
          username     TEXT UNIQUE,
          password_hash TEXT,
          role         TEXT DEFAULT 'user',
          login_count  INTEGER DEFAULT 0
        );
      ")

      DBI::dbExecute(db_pool, "
        CREATE TABLE IF NOT EXISTS rawdata (
          id           INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id      TEXT,
          uploaded_at  DATETIME DEFAULT CURRENT_TIMESTAMP,
          json         TEXT
        );
      ")

      DBI::dbExecute(db_pool, "
        CREATE TABLE IF NOT EXISTS processed_data (
          id            INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id       TEXT,
          processed_at  DATETIME DEFAULT CURRENT_TIMESTAMP,
          json          TEXT
        );
      ")

      DBI::dbExecute(db_pool, "
        CREATE TABLE IF NOT EXISTS salesdata (
          id INTEGER PRIMARY KEY AUTOINCREMENT,
          user_id TEXT,
          uploaded_at TEXT,
          json TEXT
        );
      ")
    } else {
      # PostgreSQL 語法
      suppressMessages({
        DBI::dbExecute(db_pool, "
          CREATE TABLE IF NOT EXISTS users (
            id           SERIAL PRIMARY KEY,
            username     TEXT UNIQUE,
            password_hash TEXT,
            role         TEXT DEFAULT 'user',
            login_count  INTEGER DEFAULT 0
          );
        ")

        DBI::dbExecute(db_pool, "
          CREATE TABLE IF NOT EXISTS rawdata (
            id           SERIAL PRIMARY KEY,
            user_id      TEXT,
            uploaded_at  TIMESTAMPTZ DEFAULT now(),
            json         JSONB
          );
        ")

        DBI::dbExecute(db_pool, "
          CREATE TABLE IF NOT EXISTS processed_data (
            id            SERIAL PRIMARY KEY,
            user_id       TEXT,
            processed_at  TIMESTAMPTZ DEFAULT now(),
            json          JSONB
          );
        ")

        DBI::dbExecute(db_pool, "
          CREATE TABLE IF NOT EXISTS salesdata (
            id SERIAL PRIMARY KEY,
            user_id TEXT,
            uploaded_at TIMESTAMPTZ DEFAULT now(),
            json JSONB
          );
        ")
      })
    }

    # 檢查並創建測試用戶
    existing_users <- DBI::dbGetQuery(db_pool, "SELECT COUNT(*) as count FROM users")
    user_count <- as.integer(existing_users$count[1])  # 確保正確轉換為整數
    if (user_count == 0) {
      cat("[Pool] 創建測試用戶...\n")

      DBI::dbExecute(db_pool, "
        INSERT INTO users (username, password_hash, role, login_count)
        VALUES (?, ?, 'admin', 0)
      ", list("admin", bcrypt::hashpw("admin123")))

      DBI::dbExecute(db_pool, "
        INSERT INTO users (username, password_hash, role, login_count)
        VALUES (?, ?, 'user', 0)
      ", list("testuser", bcrypt::hashpw("user123")))

      cat("[Pool] 測試用戶創建完成\n")
    } else {
      cat("[Pool] 資料庫中有", user_count, "個用戶\n")
    }

    invisible(TRUE)
  }, error = function(e) {
    cat("[Pool] 表格初始化失敗:", e$message, "\n")
    invisible(FALSE)
  })
}

# ── 取得連接（向後相容）─────────────────────────────────────────────────────
#
# 重要：此函數現在回傳 pool 物件（PostgreSQL）或單一連接（SQLite）
# pool 物件可以直接用於大部分 DBI 操作，如：
#   - dbGetQuery(get_con(), "SELECT ...")
#   - dbExecute(get_con(), "INSERT ...")
#   - dbWriteTable(get_con(), "table", data)
#
# 不需要手動 dbDisconnect()，pool 會自動管理連接
#
get_con <- function() {
  # 確保 pool 已初始化
  if (!pool_initialized || is.null(db_pool)) {
    init_pool()
  }

  # 驗證 pool 仍然有效
  if (inherits(db_pool, "Pool")) {
    if (!pool::dbIsValid(db_pool)) {
      cat("[Pool] 連接池失效，重新建立...\n")
      init_pool(force = TRUE)
    }
  } else if (inherits(db_pool, "SQLiteConnection")) {
    if (!DBI::dbIsValid(db_pool)) {
      cat("[Pool] SQLite 連接失效，重新建立...\n")
      init_pool(force = TRUE)
    }
  }

  return(db_pool)
}

# ── 取得連接池資訊 ────────────────────────────────────────────────────────────
get_db_info <- function(con = NULL) {
  # 如果有傳入 con，嘗試從屬性取得
  if (!is.null(con)) {
    info <- attr(con, "db_info")
    if (!is.null(info)) return(info)
  }

  # 否則回傳全局 pool info
  if (!is.null(db_pool_info)) {
    return(db_pool_info)
  }

  # 嘗試判斷類型
  if (!is.null(db_pool)) {
    if (inherits(db_pool, "Pool")) {
      return(list(
        type = "PostgreSQL",
        icon = "🐘",
        color = "#336791",
        status = "正式環境 (Pool)"
      ))
    } else if (inherits(db_pool, "SQLiteConnection")) {
      return(list(
        type = "SQLite",
        icon = "📁",
        color = "#FF8C00",
        status = "本地測試"
      ))
    }
  }

  return(list(
    type = "未連接",
    icon = "❌",
    color = "#DC3545",
    status = "未連接"
  ))
}

# ── 測試連接 ─────────────────────────────────────────────────────────────────
test_db_connection <- function() {
  tryCatch({
    con <- get_con()

    # 測試查詢
    result <- DBI::dbGetQuery(con, "SELECT 1 as test")

    # 檢查表格
    is_sqlite <- inherits(con, "SQLiteConnection")
    if (is_sqlite) {
      tables <- DBI::dbGetQuery(con, "
        SELECT name as table_name
        FROM sqlite_master
        WHERE type='table'
        AND name IN ('users', 'rawdata', 'processed_data', 'salesdata')
      ")
    } else {
      tables <- DBI::dbGetQuery(con, "
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public'
        AND table_name IN ('users', 'rawdata', 'processed_data', 'salesdata')
      ")
    }

    user_count <- DBI::dbGetQuery(con, "SELECT COUNT(*) as count FROM users")

    # 注意：使用 pool 時不需要 dbDisconnect
    # 如果是 SQLite 單一連接，也不要關閉（會被其他 session 使用）

    list(
      success = TRUE,
      message = paste("[Pool] 連接成功，", nrow(tables), "個表格，", user_count$count, "個用戶"),
      tables = tables$table_name,
      pool_type = if (inherits(con, "Pool")) "PostgreSQL Pool" else "SQLite Single"
    )
  }, error = function(e) {
    list(
      success = FALSE,
      message = paste("[Pool] 連接失敗:", e$message)
    )
  })
}

# ── 檢查配置 ─────────────────────────────────────────────────────────────────
check_db_config <- function() {
  test_db_connection()
}

# ── 關閉連接池（應用程式結束時呼叫）────────────────────────────────────────
close_pool <- function() {
  if (!is.null(db_pool)) {
    tryCatch({
      if (inherits(db_pool, "Pool")) {
        pool::poolClose(db_pool)
        cat("[Pool] PostgreSQL 連接池已關閉\n")
      } else if (inherits(db_pool, "SQLiteConnection")) {
        DBI::dbDisconnect(db_pool)
        cat("[Pool] SQLite 連接已關閉\n")
      }
    }, error = function(e) {
      cat("[Pool] 關閉連接時發生錯誤:", e$message, "\n")
    })
    db_pool <<- NULL
    pool_initialized <<- FALSE
  }
}

# ── Null-coalescing operator ─────────────────────────────────────────────────
`%||%` <- function(x, y) if (is.null(x)) y else x

# ── 資料庫查詢輔助說明 ───────────────────────────────────────────────────────
#
# 使用 Connection Pool 的注意事項：
#
# 1. 取得連接：
#    con <- get_con()  # 回傳 pool 物件或 SQLite 連接
#
# 2. 執行查詢（與一般 DBI 相同）：
#    result <- dbGetQuery(con, "SELECT * FROM users")
#    dbExecute(con, "UPDATE users SET login_count = login_count + 1 WHERE id = ?", list(1))
#
# 3. 不需要手動關閉連接！
#    # 錯誤：dbDisconnect(con) - 不要這樣做！
#    # Pool 會自動管理連接的借出和歸還
#
# 4. 使用 dplyr 時：
#    tbl(con, "users") %>% filter(role == "admin") %>% collect()
#
# 5. 應用程式完全結束時才呼叫：
#    close_pool()
#

# ── 資料庫查詢輔助函數 ─────────────────────────────────────────────────────────
# 這些函數提供與 BrandEdge 相容的 API，讓 module 可以直接使用
# 更新日期: 2025-01-08 - 修復 TagPilot 上傳功能
# - 修復 1: 加入 db_execute 和 db_query 函數
# - 修復 2: 加入 convert_placeholders 將 ? 轉換為 PostgreSQL 格式 $1, $2, $3
#   參考: https://github.com/r-dbi/RPostgres/issues/201

#' 將 ? 佔位符轉換為 PostgreSQL 格式 $1, $2, $3
#' @param sql SQL 查詢字串
#' @param con 資料庫連接
#' @return 轉換後的 SQL 字串
convert_placeholders <- function(sql, con) {
  # 只有 PostgreSQL (Pool 或 PqConnection) 需要轉換
  # SQLite 支援 ? 佔位符，不需轉換
  if (inherits(con, "Pool") || inherits(con, "PqConnection")) {
    # 逐一將 ? 替換為 $1, $2, $3...
    count <- 0
    while (grepl("\\?", sql)) {
      count <- count + 1
      sql <- sub("\\?", paste0("$", count), sql)
    }
  }
  return(sql)
}

#' 執行 SELECT 查詢
#' @param sql SQL 查詢字串（支援 ? 或 $1 佔位符）
#' @param params 參數列表（用於參數化查詢）
#' @return 查詢結果 data.frame
db_query <- function(sql, params = list()) {
  con <- get_con()
  sql <- convert_placeholders(sql, con)
  if (length(params) > 0) {
    result <- DBI::dbGetQuery(con, sql, params = params)
  } else {
    result <- DBI::dbGetQuery(con, sql)
  }
  return(result)
}

#' 執行 INSERT/UPDATE/DELETE 語句
#' @param sql SQL 執行語句（支援 ? 或 $1 佔位符）
#' @param params 參數列表（用於參數化查詢）
#' @return 影響的列數
#' @note 為避免 RPostgres 類型推斷問題，所有字串參數會被強制轉換
db_execute <- function(sql, params = list()) {
  con <- get_con()
  sql <- convert_placeholders(sql, con)
  if (length(params) > 0) {
    # 修復：強制將所有參數轉為正確類型，避免 RPostgres 類型推斷錯誤
    # 特別是 UUID 字串可能被誤判為其他類型
    params <- lapply(params, function(x) {
      if (is.character(x)) {
        # 使用 I() 包裝字串，防止 RPostgres 進行類型推斷
        I(as.character(x))
      } else {
        x
      }
    })
    result <- DBI::dbExecute(con, sql, params = params)
  } else {
    result <- DBI::dbExecute(con, sql)
  }
  return(result)
}

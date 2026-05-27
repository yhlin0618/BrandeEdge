# ============================================================================
# InsightForge 應用程式配置檔
# ============================================================================

# 載入環境變數（如果 .env 檔案存在）
# 優先檢查根目錄的 .env，然後檢查 config/.env
# 注意：如果環境變數已由應用主檔案載入（如 env/brandedge/.env），則跳過
if (nzchar(Sys.getenv("OPENAI_API_KEY")) || nzchar(Sys.getenv("SUPABASE_DB_HOST"))) {
  # 環境變數已載入，跳過
  # cat("✅ 環境變數已載入（來自應用專用 .env）\n")
} else if (file.exists(".env")) {
  dotenv::load_dot_env(file = ".env")
  cat("📁 已載入 .env 配置檔\n")
} else if (file.exists("config/.env")) {
  dotenv::load_dot_env(file = "config/.env")
  cat("📁 已載入 config/.env 配置檔\n")
} else {
  cat("⚠️ 未找到 .env 檔案，使用預設測試配置\n")
}

# ── 應用程式設定 ──────────────────────────────────────────────────────────
APP_CONFIG <- list(
  # 應用程式基本資訊
  app_name = "InsightForge",
  app_version = "v17",
  app_title = "精準行銷平台",
  
  # 資料庫設定（Supabase PostgreSQL）
  # dbname：Supabase 託管預設資料庫名稱固定為 postgres（勿誤填專案 ref / postgresql）
  db = list(
    host = trimws(Sys.getenv("SUPABASE_DB_HOST")),
    port = as.integer(Sys.getenv("SUPABASE_DB_PORT", 5432)),
    user = trimws(Sys.getenv("SUPABASE_DB_USER")),
    password = Sys.getenv("SUPABASE_DB_PASSWORD"),
    dbname = local({
      host <- trimws(Sys.getenv("SUPABASE_DB_HOST"))
      url <- trimws(Sys.getenv("SUPABASE_URL"))
      n <- trimws(Sys.getenv("SUPABASE_DB_NAME"))
      if (!nzchar(n)) n <- "postgres"
      # 連到 Supabase（pooler 或專案網域）時一律用 postgres，避免 Variables 貼錯
      if (grepl("supabase", host, ignore.case = TRUE) ||
          (nzchar(url) && grepl("supabase", url, ignore.case = TRUE))) {
        "postgres"
      } else {
        n
      }
    }),
    sslmode = "require"
  ),
  
  # AI API 設定
  ai = list(
    api_key = Sys.getenv("OPENAI_API_KEY"),
    api_url = "https://api.openai.com/v1/chat/completions",
    model = "gpt-5-nano",
    timeout_sec = 60,
    temperature = 0.3,
    max_tokens = 1024
  ),
  
  # 分析設定
  analysis = list(
    default_facets = 6,
    max_rows = 100,
    default_rows = 50,
    score_range = c(1, 5)
  ),
  
  # UI 設定
  ui = list(
    theme = "cerulean",
    font_family = "Noto Sans TC",
    icon_height = "60px",
    spinner_type = 6,
    spinner_color = "#0d6efd"
  ),
  
  # 平行處理設定
  parallel = list(
    max_workers = if (Sys.getenv("SHINY_PORT") != "") 1 else min(2, parallel::detectCores() - 1),
    use_sequential = Sys.getenv("SHINY_PORT") != ""
  )
)

# ── 驗證設定 ──────────────────────────────────────────────────────────────
validate_config <- function() {
  # 檢查 AI API 金鑰（這是必須的）
  if (!nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    cat("⚠️ 警告: 未設定 OPENAI_API_KEY，AI 功能將無法使用\n")
    cat("   請設定環境變數或在 .env 檔案中配置\n")
  }
  
  # 檢查資料庫配置（可選，會自動切換到 SQLite）
  db_vars <- c("SUPABASE_DB_HOST", "SUPABASE_DB_USER", "SUPABASE_DB_PASSWORD", "SUPABASE_DB_NAME")
  missing_db_vars <- db_vars[!nzchar(Sys.getenv(db_vars))]
  
  if (length(missing_db_vars) > 0) {
    cat("⚠️ PostgreSQL 配置不完整，將使用 SQLite 測試模式\n")
    cat("   缺少環境變數:", paste(missing_db_vars, collapse = ", "), "\n")
  }
  
  cat("✅ 配置檢查通過\n")
  return(TRUE)
}

# ── 輔助函數 ──────────────────────────────────────────────────────────────
get_config <- function(key = NULL) {
  if (is.null(key)) {
    return(APP_CONFIG)
  }
  
  # 支援 dot notation，例如 "db.host"
  keys <- strsplit(key, "\\.")[[1]]
  result <- APP_CONFIG
  
  for (k in keys) {
    if (!k %in% names(result)) {
      return(NULL)
    }
    result <- result[[k]]
  }
  
  return(result)
} 
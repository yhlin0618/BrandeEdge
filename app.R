# ==========================================
# BrandEdge 品牌印記引擎 - 動態 YAML 驅動應用程式
# ==========================================
# 根據 YAML 配置動態生成應用程式
# Version: 3.2.1 Flagship Edition
# Last Updated: 2025-10-06
# Based on: InsightForge Dynamic Framework v4.0

# Shiny trace for debugging (disabled - error resolved)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)

# ==========================================
# 設定應用類型識別
# ==========================================
Sys.setenv(APP_TYPE = "brandedge")
Sys.setenv(APP_CONFIG_FILE = "config/yaml/brandedge_config/brandedge.yaml")

# ==========================================
# 載入 BrandEdge 環境變數
# ==========================================
message("🔧 載入 BrandEdge 環境變數...")

# Load BrandEdge-specific .env file
find_brandedge_env_file <- function() {
  relative_env_path <- file.path("env", "brandedge", ".env")
  candidate_paths <- unique(c(
    relative_env_path,
    file.path(getwd(), relative_env_path),
    file.path(getwd(), "l3_premium", "sandbox", "sandbox_test", relative_env_path),
    file.path(dirname(getwd()), relative_env_path),
    file.path(dirname(dirname(getwd())), relative_env_path)
  ))

  existing_paths <- candidate_paths[file.exists(candidate_paths)]
  if (length(existing_paths) > 0) {
    return(normalizePath(existing_paths[1], winslash = "/", mustWork = TRUE))
  }

  relative_env_path
}

brandedge_env_file <- find_brandedge_env_file()
if (file.exists(brandedge_env_file)) {
  # Manual loading (more reliable than dotenv for this use case)
  env_lines <- readLines(brandedge_env_file, warn = FALSE)
  env_lines <- env_lines[!grepl("^#", env_lines) & nzchar(trimws(env_lines))]

  for (line in env_lines) {
    if (grepl("=", line)) {
      parts <- strsplit(line, "=", fixed = TRUE)[[1]]
      if (length(parts) >= 2) {
        key <- trimws(parts[1])
        value <- trimws(paste(parts[-1], collapse = "="))
        value <- gsub('^["\']|["\']$', '', value)  # Remove quotes

        if (grepl("^SELLERSPRITE_", key)) {
          next
        }

        # Set environment variable using proper syntax
        do.call(Sys.setenv, setNames(list(value), key))
      }
    }
  }
  message("✅ 已載入 BrandEdge 環境變數: ", brandedge_env_file)

  # Verify key variables are loaded
  if (nzchar(Sys.getenv("OPENAI_API_KEY"))) {
    message("   - OPENAI_API_KEY: ", substr(Sys.getenv("OPENAI_API_KEY"), 1, 20), "...")
  }
  if (nzchar(Sys.getenv("PGHOST"))) {
    message("   - PGHOST: ", Sys.getenv("PGHOST"))
  }
} else {
  message("⚠️  找不到 BrandEdge .env 檔案，使用預設設定")
  message("   期望路徑: ", brandedge_env_file)
}

# ==========================================
# 載入必要套件
# ==========================================
message("📦 載入必要套件...")

suppressPackageStartupMessages({
  library(shiny)
  library(bs4Dash)
  library(shinyjs)
  library(yaml)
  library(dplyr)
  library(DT)
  library(DBI)
  library(RPostgres)
  library(RSQLite)
  library(stringr)
  library(readxl)
  library(jsonlite)
  library(httr)
  library(httr2)
  library(plotly)
  library(GGally)
  library(tidyverse)
  library(bcrypt)
  library(future)
  library(furrr)
  library(markdown)
  library(tibble)
})

# ==========================================
# 自動切換工作目錄
# ==========================================
tryCatch({
  if (!is.null(sys.frame(1)$ofile)) {
    this_file <- sys.frame(1)$ofile
  } else {
    ca <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("--file=", ca, value = TRUE)
    if (length(file_arg)) this_file <- sub("--file=", "", file_arg[1])
  }

  if (exists("this_file") && !is.null(this_file)) {
    app_dir <- dirname(normalizePath(this_file))
    setwd(app_dir)
    message("工作目錄設為: ", app_dir)
  }
}, error = function(e) {
  message("無法自動設定工作目錄")
})

# ==========================================
# 載入環境變數和配置
# ==========================================
# 載入 dotenv
if (!requireNamespace("dotenv", quietly = TRUE)) {
  install.packages("dotenv")
}
library(dotenv)

# 載入 .env 檔案
if (file.exists(".env")) {
  dotenv::load_dot_env(file = ".env")
  message("✅ 已載入 .env 檔案")
}

# 若當前 APP_TYPE 已被設定為 brandedge，避免 .env 覆寫 brandedge 專用設定
if (tolower(Sys.getenv("APP_TYPE", "")) == "brandedge") {
  Sys.setenv(APP_CONFIG_FILE = "config/yaml/brandedge_config/brandedge.yaml")
}

# 載入配置檔
if (file.exists("config/config.R")) {
  source("config/config.R")
  message("✅ 已載入 config/config.R")
}

# 載入套件檔
if (file.exists("config/packages.R")) {
  source("config/packages.R")
  if (exists("initialize_packages")) {
    initialize_packages()
  }
  message("✅ 已載入 config/packages.R")
}

# ==========================================
# 載入登入模組（必須在 UI 生成器之前）
# ==========================================
# Supabase 模組保留路徑供未來切換使用；目前部署使用本地 bcrypt 登入。
supabase_auth_path <- "scripts/global_scripts/11_rshinyapp_utils/supabase_auth"
login_component_path <- "scripts/global_scripts/10_rshinyapp_components/login"
USE_SUPABASE_AUTH <<- FALSE

if (file.exists("modules/module_login_brandedge.R")) {
  source("modules/module_login_brandedge.R")
  message("✅ 已載入 BrandEdge 本地 bcrypt 登入模組")
} else if (file.exists("modules/module_login.R")) {
  source("modules/module_login.R")
  message("✅ 已載入本地 bcrypt 登入模組")
} else {
  stop("❌ 找不到本地 bcrypt 登入模組")
}

# ==========================================
# 載入工具模組
# ==========================================
# 載入配置管理器
if (file.exists("utils/config_manager.R")) {
  source("utils/config_manager.R")
}

# 載入 UI 生成器
if (file.exists("utils/ui_generator.R")) {
  source("utils/ui_generator.R")
}

# 載入 Server 引擎
if (file.exists("utils/server_engine.R")) {
  source("utils/server_engine.R")
}

# 載入模組管理器
if (file.exists("utils/module_manager.R")) {
  source("utils/module_manager.R")
}

# 載入 Session 管理器
if (file.exists("utils/session_manager.R")) {
  source("utils/session_manager.R")
}

# 載入 OpenAI 工具函數
if (file.exists("utils/openai_utils.R")) {
  source("utils/openai_utils.R")
  message("✅ 已載入 OpenAI 工具函數")
}

# 載入 Token 追蹤工具
if (file.exists("utils/token_tracker.R")) {
  source("utils/token_tracker.R")
  message("✅ 已載入 Token 追蹤工具")
}

# 載入提示系統（Hint System）
if (file.exists("utils/hint_system.R")) {
  source("utils/hint_system.R")
  message("✅ 已載入提示系統")
}

# 載入 Prompt 管理系統
if (file.exists("utils/prompt_manager.R")) {
  source("utils/prompt_manager.R")
  message("✅ 已載入 Prompt 管理系統")
}

# 載入語言管理系統
if (file.exists("utils/language_manager.R")) {
  source("utils/language_manager.R")
  message("✅ 已載入語言管理系統")
}

# 載入統一內容管理系統
if (file.exists("utils/unified_content_manager.R")) {
  source("utils/unified_content_manager.R")
  message("✅ 已載入統一內容管理系統")
}

# 載入側邊欄更新器
if (file.exists("utils/sidebar_updater.R")) {
  source("utils/sidebar_updater.R")
  message("✅ 已載入側邊欄更新器")
}

# 載入統一語言同步架構
if (file.exists("utils/unified_language_sync.R")) {
  source("utils/unified_language_sync.R")
  message("✅ 已載入統一語言同步架構")
}

# 載入統一語言管理器
if (file.exists("utils/unified_language_manager.R")) {
  source("utils/unified_language_manager.R")
  message("✅ 已載入統一語言管理器")
}

# ==========================================
# 修復並行處理設定
# ==========================================
if (Sys.getenv("SHINY_PORT") != "") {
  plan(sequential)
} else {
  plan(multisession, workers = min(2, parallel::detectCores() - 1))
}
options(future.rng.onMisuse = "ignore")

# ==========================================
# 設定資源路徑
# ==========================================
register_resource_path <- function(prefix, directory_path) {
  if (dir.exists(directory_path)) {
    addResourcePath(prefix, directory_path)
    message("✅ 已設定資源路徑: ", prefix, " -> ", directory_path)
    return(invisible(TRUE))
  }

  message("ℹ️ 略過不存在的資源路徑: ", prefix, " -> ", directory_path)
  invisible(FALSE)
}

register_resource_path("assets", "scripts/global_scripts/24_assets")
register_resource_path("www", "www")

# ==========================================
# Database Connection Setup
# ==========================================
message("🔌 設定資料庫連接...")

# Database connection function
get_db_connection <- function() {
  # Try to get database configuration
  db_config <- if (exists("app_config") && !is.null(app_config$database)) {
    app_config$database
  } else {
    list(type = "sqlite", path = "database/brandedge.db")
  }

  # Connect based on type
  if (db_config$type == "postgresql" || db_config$type == "postgres") {
    con <- DBI::dbConnect(
      RPostgres::Postgres(),
      host = Sys.getenv("PGHOST", "localhost"),
      port = as.integer(Sys.getenv("PGPORT", "5432")),
      dbname = Sys.getenv("PGDATABASE", "brandedge"),
      user = Sys.getenv("PGUSER", "postgres"),
      password = Sys.getenv("PGPASSWORD", ""),
      sslmode = Sys.getenv("PGSSLMODE", "prefer")
    )
  } else {
    # Default to SQLite
    db_path <- db_config$path %||% "database/brandedge.db"
    if (!dir.exists(dirname(db_path))) {
      dir.create(dirname(db_path), recursive = TRUE)
    }
    con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  }

  # Initialize tables if they don't exist
  if (!DBI::dbExistsTable(con, "users")) {
    DBI::dbExecute(con, "
      CREATE TABLE users (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        username TEXT UNIQUE NOT NULL,
        hash TEXT NOT NULL,
        role TEXT DEFAULT 'user',
        login_count INTEGER DEFAULT 0,
        created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
      )
    ")
  }

  existing_users <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS count FROM users")
  if (as.integer(existing_users$count[1]) == 0) {
    DBI::dbExecute(
      con,
      "INSERT INTO users (username, hash, role, login_count) VALUES (?, ?, 'admin', 0)",
      params = list("admin", bcrypt::hashpw("admin123"))
    )
    message("✅ 已建立本地管理員帳號 admin / admin123")
  }

  if (!DBI::dbExistsTable(con, "rawdata")) {
    DBI::dbExecute(con, "
      CREATE TABLE rawdata (
        id INTEGER PRIMARY KEY AUTOINCREMENT,
        user_id TEXT,
        uploaded_at TEXT,
        json TEXT
      )
    ")
  }

  return(con)
}

# Database helper functions
db_query <- function(sql, params = list()) {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  if (length(params) > 0) {
    result <- DBI::dbGetQuery(con, sql, params = params)
  } else {
    result <- DBI::dbGetQuery(con, sql)
  }
  return(result)
}

db_execute <- function(sql, params = list()) {
  con <- get_db_connection()
  on.exit(DBI::dbDisconnect(con))

  if (length(params) > 0) {
    result <- DBI::dbExecute(con, sql, params = params)
  } else {
    result <- DBI::dbExecute(con, sql)
  }
  return(result)
}

message("✅ 資料庫連接設定完成")

# ==========================================
# OpenAI API Functions
# ==========================================
message("🤖 設定 OpenAI API 函數...")

# Safe value extraction for scores
safe_value <- function(txt) {
  txt <- trimws(txt)
  num <- stringr::str_extract(txt, "[1-5]")
  if (!is.na(num)) return(as.numeric(num))
  val <- suppressWarnings(as.numeric(txt))
  if (!is.na(val) && val >= 1 && val <= 5) return(val)
  NA_real_
}

# OpenAI API function
chat_api <- function(messages, max_retries = 5, retry_delay = 5, use_mock = FALSE) {
  if (use_mock) {
    return(jsonlite::toJSON(list(
      facets = c("品質", "價格", "功能", "外觀", "耐用性", "使用便利性",
                 "包裝", "客服", "配送速度", "性價比", "創新性", "安全性",
                 "環保", "品牌信譽", "售後服務")
    ), auto_unbox = TRUE))
  }

  api_key <- Sys.getenv("OPENAI_API_KEY")
  if (!nzchar(api_key)) {
    warning("OPENAI_API_KEY not found, using mock data")
    return(jsonlite::toJSON(list(
      facets = c("品質", "價格", "功能", "外觀", "耐用性", "使用便利性",
                 "包裝", "客服", "配送速度", "性價比")
    ), auto_unbox = TRUE))
  }

  for (attempt in 1:max_retries) {
    tryCatch({
      response <- httr::POST(
        url = "https://api.openai.com/v1/chat/completions",
        httr::add_headers(
          `Content-Type` = "application/json",
          Authorization = paste("Bearer", api_key)
        ),
        body = jsonlite::toJSON(list(
          model = "gpt-4o-mini",
          messages = messages,
          temperature = 0.7,
          max_tokens = 500
        ), auto_unbox = TRUE),
        encode = "json",
        httr::timeout(30)
      )

      if (httr::status_code(response) == 200) {
        content <- httr::content(response, "text", encoding = "UTF-8")
        parsed <- jsonlite::fromJSON(content)
        return(parsed$choices[[1]]$message$content)
      } else if (httr::status_code(response) == 429) {
        if (attempt < max_retries) {
          Sys.sleep(retry_delay * attempt)
          next
        }
      } else {
        warning("API returned status ", httr::status_code(response))
      }
    }, error = function(e) {
      warning("API call error: ", e$message)
      if (attempt == max_retries) {
        return(jsonlite::toJSON(list(
          facets = c("品質", "價格", "功能", "外觀", "耐用性")
        ), auto_unbox = TRUE))
      }
    })
  }

  # Final fallback
  return(jsonlite::toJSON(list(
    facets = c("品質", "價格", "功能", "外觀", "耐用性")
  ), auto_unbox = TRUE))
}

message("✅ OpenAI API 函數設定完成")

# ==========================================
# Hint and Prompt Manager Functions
# ==========================================
message("💬 載入提示系統...")

# Load hint system
hint_data <- NULL
if (file.exists("database/content/chinese/general/brandedge/hint.csv")) {
  hint_data <- read.csv("database/content/chinese/general/brandedge/hint.csv",
                       stringsAsFactors = FALSE)
  message("✅ 已載入 hint 資料 (", nrow(hint_data), " 筆)")
}

get_hint <- function(var_id) {
  if (is.null(hint_data)) return("")
  hint_row <- hint_data[hint_data$var_id == var_id, ]
  if (nrow(hint_row) > 0) {
    return(hint_row$description[1])
  }
  return("")
}

# Load prompt system
prompt_data <- NULL
if (file.exists("database/content/chinese/general/brandedge/prompt.csv")) {
  prompt_data <- read.csv("database/content/chinese/general/brandedge/prompt.csv",
                         stringsAsFactors = FALSE)
  message("✅ 已載入 prompt 資料 (", nrow(prompt_data), " 筆)")
}

get_prompt <- function(analysis_name) {
  if (is.null(prompt_data)) return("")
  prompt_row <- prompt_data[prompt_data$analysis_name == analysis_name, ]
  if (nrow(prompt_row) > 0) {
    return(prompt_row$prompt[1])
  }
  return("")
}

# Prompt replacement helper
replace_prompt_vars <- function(prompt_template, vars_list) {
  result <- prompt_template
  for (var_name in names(vars_list)) {
    placeholder <- paste0("{", var_name, "}")
    result <- gsub(placeholder, vars_list[[var_name]], result, fixed = TRUE)
  }
  return(result)
}

message("✅ 提示系統載入完成")

# ==========================================
# 載入應用程式配置
# ==========================================
config_file <- Sys.getenv("APP_CONFIG_FILE", "config/yaml/brandedge_config/brandedge.yaml")

# 載入配置
app_config <- if (exists("load_app_config")) {
  tryCatch({
    load_app_config(config_file)
  }, error = function(e) {
    message("配置載入失敗: ", e$message)
    # 使用最小配置
    list(
      app_info = list(
        name = "BrandEdge 品牌印記引擎",
        version = "3.2.1"
      ),
      modules = list(),
      pages = list()
    )
  })
} else {
  # 如果沒有 config_manager，直接讀取 YAML
  yaml::read_yaml(config_file)
}

# ==========================================
# 初始化統一語言管理系統
# ==========================================
# 確定初始語言
config_language <- if (!is.null(app_config$language$default)) {
  # 將 config 中的語言目錄名稱轉換為語言代碼
  if (app_config$language$default == "chinese") "zh_TW" else "en_US"
} else {
  "zh_TW"  # 預設中文
}

# 將 app_config 設置為全域變數，供語言管理器使用
assign("app_config", app_config, envir = .GlobalEnv)

# 初始化統一語言管理器
if (exists("initialize_unified_language_manager")) {
  initialize_unified_language_manager(config_language)
} else {
  stop("initialize_unified_language_manager() function not found. Please check utils/unified_language_manager.R is loaded.")
}

# DEBUG: Check global_lang_content type
if (exists("global_lang_content")) {
  glc_test <- get("global_lang_content", envir = .GlobalEnv)
  cat("🔍 [INIT DEBUG] global_lang_content type check:\n")
  cat("  - is.function:", is.function(glc_test), "\n")
  cat("  - class:", paste(class(glc_test), collapse = ", "), "\n")
  rm(glc_test)
}

default_language <- config_language

cat("🌍 === 語言初始化調試資訊 ===\n")
cat("🌍 app_config$language$default:", app_config$language$default, "\n")
cat("🌍 最終使用語言:", default_language, "\n")
cat("🌍 ================================\n")

message(paste("✅ 已載入初始語言內容:", default_language))

# 保持向後兼容
lang_content <- if (exists("get_unified_language_content") && is.function(get_unified_language_content)) {
  get_unified_language_content()
} else if (exists("global_lang_content") && is.function(global_lang_content)) {
  tryCatch(isolate(global_lang_content()), error = function(e) list(language = default_language, content = list()))
} else {
  list(language = default_language, content = list())
}

# ==========================================
# 語言輔助函數
# ==========================================
# 安全獲取語言文字的函數，提供備用文字
get_text <- function(path, fallback = "", ...) {
  # 使用動態語言內容 - 直接訪問 global_lang_content (through closure)
  current_lang_content <- tryCatch({
    # Directly try to call global_lang_content() - it should be available via closure
    # Don't use exists() as it may fail in observeEvent context
    isolate(global_lang_content())
  }, error = function(e) {
    # If global_lang_content not available, try global_language_state directly
    cat("⚠️ [get_text] global_lang_content not accessible, trying global_language_state\n")
    tryCatch({
      if (exists("global_language_state", inherits = TRUE) && is.reactivevalues(global_language_state)) {
        isolate(global_language_state$language_content)
      } else if (exists("lang_content", inherits = TRUE)) {
        lang_content
      } else {
        NULL
      }
    }, error = function(e2) {
      cat("⚠️ [get_text] All fallbacks failed, using static fallback\n")
      if (exists("lang_content", inherits = TRUE)) {
        lang_content
      } else {
        NULL
      }
    })
  })

  # Debug: Show current language
  if (!is.null(current_lang_content$language)) {
    cat("🌐 [get_text] Path:", path, "| Language:", current_lang_content$language, "\n")
  }

  # 從語言內容中尋找文字
  result <- NULL
  if (!is.null(current_lang_content)) {
    keys <- strsplit(path, "\\.")[[1]]

    # 處理簡單的 key
    if (length(keys) == 1 && grepl("_", keys[1])) {
      parts <- strsplit(keys[1], "_")[[1]]
      if (length(parts) >= 2) {
        # 嘗試在 common 中查找
        if (!is.null(current_lang_content$content$common)) {
          if (parts[1] == "app" && !is.null(current_lang_content$content$common$app)) {
            result <- current_lang_content$content$common$app[[paste(parts[-1], collapse = "_")]]
          } else if (parts[1] == "login" && !is.null(current_lang_content$content$common$login)) {
            result <- current_lang_content$content$common$login[[paste(parts[-1], collapse = "_")]]
          } else if (length(parts) >= 2 && parts[length(parts)] %in% c("button", "btn") && !is.null(current_lang_content$content$common$buttons)) {
            button_key <- paste(parts[-length(parts)], collapse = "_")
            result <- current_lang_content$content$common$buttons[[button_key]]
          } else if (!is.null(current_lang_content$content$common[[parts[1]]])) {
            if (length(parts) == 2) {
              result <- current_lang_content$content$common[[parts[1]]][[parts[2]]]
            } else {
              result <- current_lang_content$content$common[[parts[1]]][[paste(parts[-1], collapse = "_")]]
            }
          }
        }

        # 如果沒找到，嘗試在模組中查找
        if (is.null(result) && !is.null(current_lang_content$content$modules)) {
          if (!is.null(current_lang_content$content$modules[[parts[1]]])) {
            if (length(parts) == 2) {
              result <- current_lang_content$content$modules[[parts[1]]][[parts[2]]]
            } else {
              result <- current_lang_content$content$modules[[parts[1]]][[paste(parts[-1], collapse = "_")]]
            }
          }
        }
      }
    }

    # 如果簡單 key 沒找到，嘗試標準路徑查找
    if (is.null(result)) {
      # 從 content.common 開始尋找
      if (!is.null(current_lang_content$content$common)) {
        temp_result <- current_lang_content$content$common
        for (key in keys) {
          if (is.list(temp_result) && key %in% names(temp_result)) {
            temp_result <- temp_result[[key]]
          } else {
            temp_result <- NULL
            break
          }
        }
        if (!is.null(temp_result)) {
          result <- temp_result
        }
      }
    }

    # 如果沒找到，從 content 直接尋找
    if (is.null(result) && !is.null(current_lang_content$content)) {
      temp_result <- current_lang_content$content
      for (key in keys) {
        if (is.list(temp_result) && key %in% names(temp_result)) {
          temp_result <- temp_result[[key]]
        } else {
          temp_result <- NULL
          break
        }
      }
      if (!is.null(temp_result)) {
        result <- temp_result
      }
    }

    # 如果還是沒找到，直接從語言內容尋找
    if (is.null(result)) {
      temp_result <- current_lang_content
      for (key in keys) {
        if (is.list(temp_result) && key %in% names(temp_result)) {
          temp_result <- temp_result[[key]]
        } else {
          temp_result <- NULL
          break
        }
      }
      if (!is.null(temp_result) && is.character(temp_result)) {
        result <- temp_result
      }
    }
  }

  # 如果沒找到結果，使用 fallback
  if (is.null(result)) {
    cat("⚠️ [get_text] Path not found:", path, "- Using fallback:", fallback, "\n")
    result <- fallback
  } else {
    cat("✅ [get_text] Found:", path, "=", as.character(result), "\n")
  }

  # 支持字串模板替換
  if (length(list(...)) > 0 && is.character(result)) {
    args <- list(...)
    for (name in names(args)) {
      result <- gsub(paste0("\\{", name, "\\}"), args[[name]], result)
    }
  }

  return(if (is.character(result)) result else fallback)
}

# ==========================================
# 模組語言文字獲取函數
# ==========================================
get_module_text <- function(module_id, path, fallback = "", ...) {
  current_lang_content <- if (exists("global_lang_content") && is.function(global_lang_content)) {
    tryCatch(isolate(global_lang_content()), error = function(e) NULL)
  } else {
    NULL
  }

  if (is.null(current_lang_content) || is.null(current_lang_content$content$modules)) {
    return(fallback)
  }

  module_content <- current_lang_content$content$modules[[module_id]]
  if (is.null(module_content)) {
    return(fallback)
  }

  keys <- strsplit(path, "\\.")[[1]]
  result <- module_content

  for (key in keys) {
    if (is.list(result) && key %in% names(result)) {
      result <- result[[key]]
    } else {
      return(fallback)
    }
  }

  # 支持字串模板替換
  if (length(list(...)) > 0 && is.character(result)) {
    args <- list(...)
    for (name in names(args)) {
      result <- gsub(paste0("\\{", name, "\\}"), args[[name]], result)
    }
  }

  return(if (is.character(result)) result else fallback)
}

# ==========================================
# NULL 合併運算子
# ==========================================
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ==========================================
# 模組語言文字輔助函數
# ==========================================
get_module_texts <- function(lang_content, module_name) {
  language_code <- if (!is.null(lang_content$language)) {
    lang_content$language
  } else {
    "zh_TW"
  }

  module_texts <- get_module_language_content(module_name, language_code)

  if (!is.null(module_texts)) {
    module_texts$language <- language_code
  }

  return(module_texts)
}

# ==========================================
# 顯示配置資訊
# ==========================================
message("========================================")
message("應用程式: ", app_config$app_info$name %||% "未命名")
message("版本: ", app_config$app_info$version %||% "1.0")
message("環境: ", app_config$environment$mode %||% "development")
message("========================================")

# ==========================================
# 載入 BrandEdge 模組
# ==========================================
message("載入 BrandEdge 模組...")

# 載入登入模組（本地 bcrypt）
if (!exists("USE_SUPABASE_AUTH") || !USE_SUPABASE_AUTH) {
  if (file.exists("modules/module_login_brandedge.R")) {
    source("modules/module_login_brandedge.R")
    USE_SUPABASE_AUTH <<- FALSE
    message("✅ 已載入 BrandEdge 登入模組（bcrypt）")
  } else if (file.exists("modules/module_login.R")) {
    source("modules/module_login.R")
    USE_SUPABASE_AUTH <<- FALSE
    message("✅ 已載入通用登入模組（bcrypt）")
  } else {
    USE_SUPABASE_AUTH <<- FALSE
    message("⚠️ 未找到任何登入模組")
  }
} else {
  message("✅ Supabase 登入模組已在前面載入，跳過此區塊")
}

# 載入 BrandEdge 專用資料上傳模組
if (file.exists("modules/module_upload_brandedge.R")) {
  source("modules/module_upload_brandedge.R")
  message("✅ 已載入 BrandEdge 資料上傳模組")
} else if (file.exists("modules/module_upload.R")) {
  # 備用方案：如果找不到 brandedge 版本，使用通用版本
  source("modules/module_upload.R")
  message("✅ 已載入通用資料上傳模組")
}

# 載入 BrandEdge 共用函數
if (file.exists("modules/module_brandedge_shared.R")) {
  source("modules/module_brandedge_shared.R")
  message("✅ 已載入 BrandEdge 共用函數")
}

# 動態從 YAML 載入 BrandEdge 各個子模組
if (!is.null(app_config$pages)) {
  # 收集所有啟用的模組名稱 (排除 login，它已經單獨載入)
  module_names <- unique(sapply(app_config$pages, function(page) {
    if (!is.null(page$module) && page$enabled &&
        !page$module %in% c("login")) {
      return(page$module)
    }
    return(NULL)
  }))

  # 移除 NULL 值
  module_names <- module_names[!sapply(module_names, is.null)]

  # 載入每個模組
  for (module_name in module_names) {
    # 檢查是否已有 "brandedge_" 前綴，避免重複
    if (grepl("^brandedge_", module_name)) {
      module_file <- paste0("modules/module_", module_name, ".R")
    } else {
      module_file <- paste0("modules/module_brandedge_", module_name, ".R")
    }

    if (file.exists(module_file)) {
      source(module_file)
      message(paste("✅ 已從 YAML 配置載入:", basename(module_file)))
    } else {
      # Try without brandedge prefix as fallback
      alt_module_file <- paste0("modules/module_", module_name, ".R")
      if (file.exists(alt_module_file)) {
        source(alt_module_file)
        message(paste("✅ 已從 YAML 配置載入:", basename(alt_module_file)))
      } else {
        warning(paste("⚠️ YAML 配置中的模組檔案不存在:", module_file))
      }
    }
  }

  message(paste("📦 共載入", length(module_names), "個 BrandEdge 子模組"))
}

message("========================================")

# ==========================================
# 載入資料庫連接
# ==========================================
if (file.exists("database/db_connection.R")) {
  source("database/db_connection.R")
  message("✅ 已載入資料庫連接模組")
}

# ==========================================
# 定義 UI
# ==========================================
ui <- function() {
  # 檢查是否有動態 UI 生成器
  if (exists("generate_app_ui")) {
    ui_content <- generate_app_ui(app_config)
    tagList(
      ui_content,
      # 註冊側邊欄更新器 JavaScript
      if (exists("register_sidebar_updater_js")) {
        register_sidebar_updater_js()
      } else {
        NULL
      },
      # BrandEdge 特定 JavaScript 和 CSS
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "market_profile_enhanced.css"),
        tags$style(HTML("
          /* BrandEdge 特定樣式 */
          .step-indicator {
            display: flex;
            justify-content: space-between;
            margin-bottom: 2rem;
            padding: 1rem;
            background: #f8f9fa;
            border-radius: 8px;
          }
          .step-item {
            flex: 1;
            text-align: center;
            padding: 0.5rem;
            border-radius: 5px;
            transition: all 0.3s;
            margin: 0 5px;
          }
          .step-item.active {
            background: #007bff;
            color: white;
          }
          .step-item.completed {
            background: #28a745;
            color: white;
          }
          .welcome-banner {
            background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
            color: white;
            padding: 1.5rem;
            border-radius: 10px;
            margin-bottom: 2rem;
            text-align: center;
          }
          .attr-selector {
            background: #f8f9fa;
            padding: 1rem;
            border-radius: 8px;
            margin-bottom: 1rem;
          }
          .attr-badge {
            display: inline-block;
            padding: 0.25rem 0.5rem;
            margin: 0.25rem;
            background: #007bff;
            color: white;
            border-radius: 0.25rem;
            font-size: 0.875rem;
          }
          .dna-insights {
            padding: 15px;
            background: #f8f9fa;
            border-radius: 8px;
            margin-top: 10px;
          }
          .dna-insights h5 {
            color: #2c3e50;
            border-bottom: 2px solid #3498db;
            padding-bottom: 10px;
            margin-bottom: 15px;
          }
          .ai-insights-section {
            background: #ffffff;
            border: 1px solid #dee2e6;
            border-radius: 5px;
            padding: 15px;
            margin-top: 20px;
          }
          .ai-insights-section li:before {
            content: '▸ ';
            color: #3498db;
            font-weight: bold;
            margin-right: 5px;
          }
        "))
      ),
      tags$script(HTML("
        // 處理全局語言變更消息
        Shiny.addCustomMessageHandler('global_language_change', function(data) {
          console.log('收到全局語言變更:', data.language);
          Shiny.setInputValue('global_language_change', data, {priority: 'event'});
        });

        // 強制模組語言同步處理器
        Shiny.addCustomMessageHandler('force_module_language_sync', function(data) {
          console.log('🔄 強制模組語言同步:', data.language);
          setTimeout(function() {
            Shiny.setInputValue('force_language_reload', {
              language: data.language,
              timestamp: data.timestamp,
              force: true
            }, {priority: 'event'});

            if (window.Shiny && window.Shiny.unbindAll && window.Shiny.bindAll) {
              try {
                window.Shiny.unbindAll();
                window.Shiny.bindAll();
                console.log('✅ Shiny 元素重新綁定完成');
              } catch(e) {
                console.log('⚠️ Shiny 重新綁定警告:', e);
              }
            }
          }, 50);
        });

        // 監聽模組語言更新消息
        Shiny.addCustomMessageHandler('module_language_update', function(data) {
          console.log('模組語言更新:', data.language);
          if (data.language) {
            sessionStorage.setItem('selected_language', data.language);
          }
        });

        // 處理語言按鈕樣式更新
        Shiny.addCustomMessageHandler('updateLanguageButtonClass', function(data) {
          console.log('更新語言按鈕樣式:', data);
          if (data.activeBtn) {
            $('#' + data.activeBtn).addClass('active').removeClass('btn-outline-primary').addClass('btn-primary');
          }
          if (data.inactiveBtn) {
            $('#' + data.inactiveBtn).removeClass('active').removeClass('btn-primary').addClass('btn-outline-primary');
          }
        });

        // 處理側邊欄文字更新
        Shiny.addCustomMessageHandler('updateSidebarText', function(data) {
          console.log('🌐 更新側邊欄文字:', data.id, '->', data.text);
          var element = document.getElementById(data.id);
          if (element) {
            element.textContent = data.text;
            console.log('✅ 側邊欄文字更新成功');
          } else {
            console.log('⚠️ 找不到側邊欄元素:', data.id);
          }
        });

        // 頁面載入時檢查 sessionStorage 中的語言設定
        $(document).ready(function() {
          var savedLanguage = sessionStorage.getItem('selected_language');
          if (savedLanguage) {
            console.log('從 sessionStorage 載入語言設定:', savedLanguage);
            Shiny.setInputValue('initial_language', savedLanguage, {priority: 'event'});
          }
        });
      "))
    )
  } else {
    # 備用簡單 UI
    fluidPage(
      titlePanel(app_config$app_info$name %||% "BrandEdge 品牌印記引擎"),
      sidebarLayout(
        sidebarPanel(
          h4("功能選單"),
          p("請檢查配置檔案")
        ),
        mainPanel(
          h3("系統初始化中"),
          p("請檢查配置檔案")
        )
      )
    )
  }
}

# ==========================================
# 定義 Server
# ==========================================
server <- function(input, output, session) {

  # ==========================================
  # 初始化反應式變數
  # ==========================================

  # 使用者狀態
  user_logged_in <- reactiveVal(FALSE)
  current_user <- reactiveVal(NULL)

  # Output for conditionalPanel (needed by UI generator)
  output$user_logged_in <- reactive({
    user_logged_in()
  })
  outputOptions(output, "user_logged_in", suspendWhenHidden = FALSE)

  # 資料狀態
  upload_data <- reactiveVal(NULL)
  processed_data <- reactiveVal(NULL)
  facets_rv <- reactiveVal(NULL)
  num_attributes_rv <- reactiveVal(15)  # 預設15個屬性
  scoring_data <- reactiveVal(NULL)
  analysis_data <- reactiveVal(NULL)
  amazon_rank_results <- reactiveVal(NULL)
  amazon_rank_status <- reactiveVal(NULL)
  amazon_rank_error <- reactiveVal(NULL)
  competitor_asins <- reactiveVal(NULL)
  amazon_review_preview <- reactiveVal(NULL)
  amazon_review_summary <- reactiveVal(NULL)
  amazon_review_status <- reactiveVal(NULL)
  amazon_review_error <- reactiveVal(NULL)
  amazon_review_timing <- reactiveVal(NULL)
  AMAZON_REVIEW_MAX_ASINS <- 5L

  # 進度資訊
  progress_info <- reactiveVal(list(start=NULL, done=0, total=0))

  # UI 更新觸發器
  module_ui_update_trigger <- reactiveVal(0)

  # ==========================================
  # Token 使用量追蹤
  # ==========================================

  # 初始化 Token 狀態
  token_state <- reactiveVal(create_token_state(
    model = app_config$api$openai$default_model %||% "gpt-5-nano"
  ))

  # 全域更新函數（供模組呼叫）
  update_token_usage <- function(usage_info) {
    if (!is.null(usage_info)) {
      current <- token_state()
      updated <- update_token_state(current, usage_info)
      token_state(updated)
      message(sprintf("📊 Token 更新: +%d tokens, 累計: %d, 費用: %s",
                     usage_info$total_tokens %||% 0,
                     updated$total_tokens,
                     format_cost(updated$total_cost)))
    }
  }
  assign("update_token_usage", update_token_usage, envir = .GlobalEnv)

  run_amazon_bsr_scraper <- function(asins) {
    script_path <- file.path(getwd(), "scripts", "python", "amazon_bsr_scraper.py")
    if (!file.exists(script_path)) {
      stop("找不到 Amazon 爬蟲腳本: ", script_path)
    }
    if (!requireNamespace("processx", quietly = TRUE)) {
      stop("缺少 R 套件 processx，無法呼叫 Python 爬蟲。")
    }

    payload <- jsonlite::toJSON(
      list(asins = as.character(asins)),
      auto_unbox = TRUE
    )
    payload_file <- tempfile(fileext = ".json")
    writeLines(payload, payload_file, useBytes = TRUE)
    on.exit(unlink(payload_file), add = TRUE)

    process_result <- processx::run(
      command = "python3",
      args = c(script_path, payload_file),
      error_on_status = FALSE,
      timeout = 90
    )

    stdout <- trimws(process_result$stdout %||% "")
    stderr <- trimws(process_result$stderr %||% "")

    if (!identical(process_result$status, 0L)) {
      stop("Amazon 爬蟲執行失敗: ", stderr %||% paste("exit status", process_result$status))
    }
    if (!nzchar(stdout)) {
      stop("Amazon 爬蟲沒有回傳資料。", if (nzchar(stderr)) paste0(" ", stderr) else "")
    }

    jsonlite::fromJSON(stdout, flatten = TRUE)
  }

  merge_user_competitors <- function(scrape_results, competitor_df, input_asin) {
    result_columns <- c(
      "input_asin", "source_category", "source_category_rank", "source_segment",
      "top_rank", "top_asin", "product_url", "scrape_status", "error_message"
    )

    if (is.null(scrape_results) || !is.data.frame(scrape_results)) {
      scrape_results <- data.frame()
    }

    for (column_name in setdiff(result_columns, names(scrape_results))) {
      scrape_results[[column_name]] <- NA
    }
    scrape_results <- scrape_results[, result_columns, drop = FALSE]

    competitor_asins_df <- competitor_df
    if (is.null(competitor_asins_df) || !is.data.frame(competitor_asins_df) || nrow(competitor_asins_df) == 0) {
      return(scrape_results)
    }

    competitor_values <- unique(trimws(competitor_asins_df$asin %||% character()))
    competitor_values <- competitor_values[nzchar(competitor_values)]
    if (length(competitor_values) == 0) {
      return(scrape_results)
    }

    competitor_rows <- data.frame(
      input_asin = rep(as.character(input_asin)[1], length(competitor_values)),
      source_category = rep("客戶指定", length(competitor_values)),
      source_category_rank = rep(NA_integer_, length(competitor_values)),
      source_segment = rep("客戶指定", length(competitor_values)),
      top_rank = rep(NA_integer_, length(competitor_values)),
      top_asin = competitor_values,
      product_url = paste0("https://www.amazon.com/dp/", competitor_values),
      scrape_status = rep("user_competitor", length(competitor_values)),
      error_message = rep("", length(competitor_values)),
      stringsAsFactors = FALSE
    )

    combined_results <- rbind(scrape_results, competitor_rows)
    combined_results <- combined_results[!duplicated(paste(combined_results$top_asin, combined_results$source_segment)), , drop = FALSE]
    cap_competitor_results(combined_results, limit = 40)
  }

  cap_competitor_results <- function(results, limit = 40) {
    if (is.null(results) || !is.data.frame(results) || nrow(results) <= limit) {
      return(results)
    }

    capped_results <- results
    for (segment_name in c("New Releases Top 100", "對手", "中段")) {
      if (nrow(capped_results) <= limit) {
        break
      }

      segment_indices <- which(capped_results$source_segment == segment_name)
      removable_count <- min(nrow(capped_results) - limit, length(segment_indices))
      if (removable_count <= 0) {
        next
      }

      segment_ranks <- capped_results$top_rank[segment_indices]
      segment_ranks[is.na(segment_ranks)] <- Inf
      remove_indices <- segment_indices[order(segment_ranks, decreasing = TRUE)][seq_len(removable_count)]
      capped_results <- capped_results[-remove_indices, , drop = FALSE]
    }

    if (nrow(capped_results) > limit) {
      capped_results <- capped_results[seq_len(limit), , drop = FALSE]
    }

    capped_results
  }

  normalize_amazon_reviews_for_scoring <- function(reviews) {
    if (is.null(reviews) || !is.data.frame(reviews) || nrow(reviews) == 0) {
      return(NULL)
    }

    get_column <- function(column_name, fallback = NA) {
      if (column_name %in% names(reviews)) {
        reviews[[column_name]]
      } else {
        rep(fallback, nrow(reviews))
      }
    }

    asin <- trimws(as.character(get_column("asin", "")))
    title <- as.character(get_column("title", ""))
    content <- as.character(get_column("content", ""))
    title[is.na(title)] <- ""
    content[is.na(content)] <- ""

    valid_rows <- !is.na(asin) &
      nzchar(asin) &
      (nzchar(trimws(content)) | nzchar(trimws(title)))

    if (!any(valid_rows)) {
      return(NULL)
    }

    normalized_reviews <- data.frame(
      Variation = asin[valid_rows],
      Title = title[valid_rows],
      Body = content[valid_rows],
      Rating = suppressWarnings(as.numeric(get_column("star", NA_real_)))[valid_rows],
      Date = as.character(get_column("review_date", ""))[valid_rows],
      source = as.character(get_column("source", ""))[valid_rows],
      author = as.character(get_column("author", ""))[valid_rows],
      verified = as.logical(get_column("verified", NA))[valid_rows],
      stringsAsFactors = FALSE
    )

    if (nrow(normalized_reviews) == 0) {
      return(NULL)
    }

    normalized_reviews
  }

  build_amazon_review_preview_asins <- function(primary_asin, rank_results, max_asins = 41L) {
    candidate_asins <- c(as.character(primary_asin)[1])

    if (!is.null(rank_results) && is.data.frame(rank_results) && "top_asin" %in% names(rank_results)) {
      candidate_asins <- c(candidate_asins, as.character(rank_results$top_asin))
    }

    candidate_asins <- unique(trimws(candidate_asins))
    candidate_asins <- candidate_asins[nzchar(candidate_asins)]

    if (length(candidate_asins) > max_asins) {
      candidate_asins <- candidate_asins[seq_len(max_asins)]
    }

    candidate_asins
  }

  append_amazon_review_timing_log <- function(timing, asin_count, reviews_per_asin) {
    if (is.null(timing) || length(timing) == 0) {
      return(invisible(FALSE))
    }

    tryCatch({
      log_dir <- "logs"
      if (!dir.exists(log_dir)) {
        dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
      }

      log_row <- list(
        started_at = timing$started_at %||% as.character(Sys.time()),
        ended_at = timing$ended_at %||% as.character(Sys.time()),
        elapsed_seconds = timing$elapsed_seconds %||% NA_real_,
        asin_count = asin_count,
        reviews_per_asin = reviews_per_asin,
        total_reviews_found = timing$total_reviews_found %||% NA_integer_,
        reviews_per_second = timing$reviews_per_second %||% NA_real_
      )

      write(
        jsonlite::toJSON(log_row, auto_unbox = TRUE),
        file = file.path(log_dir, "amazon_review_scraper_timing.log"),
        append = TRUE
      )
      invisible(TRUE)
    }, error = function(e) {
      message("⚠️ Amazon review timing log 寫入失敗: ", e$message)
      invisible(FALSE)
    })
  }

  run_amazon_review_scraper <- function(asins,
                                        reviews_per_asin,
                                        marketplace = "US",
                                        max_pages = 10L) {
    script_dir <- file.path(getwd(), "scripts", "python")
    script_path <- file.path(script_dir, "amazon_review_selenium_scraper.py")
    scraper_engine <- "selenium"
    python_command <- trimws(Sys.getenv("AMAZON_REVIEW_PYTHON", ""))
    if (!nzchar(python_command)) {
      python_candidates <- c(
        file.path(getwd(), ".venv-selenium", "bin", "python"),
        file.path(dirname(getwd()), "ai_martech2", ".venv-selenium", "bin", "python"),
        "/opt/venv/bin/python"
      )
      existing_python <- python_candidates[file.exists(python_candidates)]
      python_command <- if (length(existing_python) > 0) {
        existing_python[1]
      } else {
        "python3"
      }
    }

    if (!file.exists(script_path)) {
      stop("找不到 Amazon Selenium 評論爬蟲腳本: ", script_path)
    }
    if (!requireNamespace("processx", quietly = TRUE)) {
      stop("缺少 R 套件 processx，無法呼叫 Python 爬蟲。")
    }

    asins <- unique(trimws(as.character(asins)))
    asins <- asins[nzchar(asins)]
    reviews_per_asin <- max(1L, min(100L, as.integer(reviews_per_asin %||% 40L)))

    payload <- jsonlite::toJSON(
      list(
        asins = asins,
        reviews_per_asin = reviews_per_asin,
        marketplace = marketplace,
        random_sample = TRUE,
        max_pages = max_pages
      ),
      auto_unbox = TRUE
    )
    payload_file <- tempfile(fileext = ".json")
    writeLines(payload, payload_file, useBytes = TRUE)
    on.exit(unlink(payload_file), add = TRUE)

    started_at <- Sys.time()
    message(sprintf(
      "Amazon 評論爬蟲開始: engine=%s, python=%s, script=%s, ASIN=%d, 每個 ASIN 目標評論數=%d, started_at=%s",
      scraper_engine,
      python_command,
      basename(script_path),
      length(asins),
      reviews_per_asin,
      format(started_at, "%Y-%m-%d %H:%M:%S")
    ))

    process_result <- processx::run(
      command = python_command,
      args = c(script_path, payload_file),
      error_on_status = FALSE,
      timeout = 900
    )

    ended_at <- Sys.time()
    stdout <- trimws(process_result$stdout %||% "")
    stderr <- trimws(process_result$stderr %||% "")

    if (!identical(process_result$status, 0L)) {
      stop("Amazon 評論爬蟲執行失敗: ", stderr %||% paste("exit status", process_result$status))
    }
    if (!nzchar(stdout)) {
      stop("Amazon 評論爬蟲沒有回傳資料。", if (nzchar(stderr)) paste0(" ", stderr) else "")
    }

    parsed_result <- jsonlite::fromJSON(stdout, flatten = TRUE)
    timing <- parsed_result$timing %||% list()
    elapsed_seconds <- timing$elapsed_seconds %||% as.numeric(difftime(ended_at, started_at, units = "secs"))
    total_reviews_found <- timing$total_reviews_found %||% if (is.data.frame(parsed_result$results)) nrow(parsed_result$results) else 0

    if (isTRUE(as.integer(total_reviews_found) == 0L)) {
      summary_df <- parsed_result$summary
      summary_parts <- c(parsed_result$error_message %||% "")
      if (!is.null(summary_df) && is.data.frame(summary_df) && nrow(summary_df) > 0) {
        status_values <- unique(as.character(summary_df$scrape_status %||% ""))
        error_values <- unique(as.character(summary_df$error_message %||% ""))
        summary_parts <- c(
          summary_parts,
          paste0("status=", paste(status_values[nzchar(status_values)], collapse = ",")),
          paste0("message=", paste(error_values[nzchar(error_values)], collapse = "; "))
        )
      }
      summary_message <- paste(summary_parts[nzchar(summary_parts)], collapse = ", ")
      stop("Amazon Selenium 評論爬蟲沒有取得任何評論。", if (nzchar(summary_message)) paste0(" ", summary_message) else "")
    }

    message(sprintf(
      "Amazon 評論爬蟲完成: ended_at=%s, elapsed=%.2fs, total_reviews=%d, avg_seconds_per_asin=%s, reviews_per_second=%s",
      format(ended_at, "%Y-%m-%d %H:%M:%S"),
      as.numeric(elapsed_seconds),
      as.integer(total_reviews_found),
      as.character(timing$avg_seconds_per_asin %||% NA),
      as.character(timing$reviews_per_second %||% NA)
    ))

    append_amazon_review_timing_log(
      timing = timing,
      asin_count = length(asins),
      reviews_per_asin = reviews_per_asin
    )

    parsed_result
  }

  run_amazon_review_preview <- function(primary_asin, rank_results, reviews_per_asin) {
    target_asins <- build_amazon_review_preview_asins(
      primary_asin = primary_asin,
      rank_results = rank_results,
      max_asins = AMAZON_REVIEW_MAX_ASINS
    )

    if (length(target_asins) == 0) {
      stop("沒有可查詢的 ASIN。")
    }

    amazon_review_status(sprintf(
      "Amazon 評論抓取中... 本次最多 %d 個 ASIN，實際查詢 %d 個 ASIN，每個 ASIN 約 %d 則評論",
      AMAZON_REVIEW_MAX_ASINS,
      length(target_asins),
      reviews_per_asin
    ))
    amazon_review_error(NULL)

    scraper_result <- run_amazon_review_scraper(
      asins = target_asins,
      reviews_per_asin = reviews_per_asin,
      marketplace = "US",
      max_pages = 10L
    )

    reviews <- scraper_result$results
    if (is.null(reviews) || !is.data.frame(reviews)) {
      reviews <- data.frame()
    }

    summary_df <- scraper_result$summary
    if (is.null(summary_df) || !is.data.frame(summary_df)) {
      summary_df <- data.frame()
    }

    if (nrow(summary_df) > 0 && all(c("asin", "scrape_status", "error_message") %in% names(summary_df))) {
      apply(summary_df, 1, function(row) {
        if (nzchar(row[["error_message"]])) {
          message(sprintf(
            "Amazon 評論爬蟲狀態: ASIN=%s, status=%s, message=%s",
            row[["asin"]],
            row[["scrape_status"]],
            row[["error_message"]]
          ))
        }
      })
    }

    amazon_review_preview(reviews)
    amazon_review_summary(summary_df)
    amazon_review_timing(scraper_result$timing %||% NULL)

    failed_rows <- if (nrow(summary_df) > 0 && "scrape_status" %in% names(summary_df)) {
      summary_df[summary_df$scrape_status %in% c("error", "partial", "no_reviews"), , drop = FALSE]
    } else {
      data.frame()
    }

    if (nrow(failed_rows) > 0) {
      amazon_review_error("部分 ASIN 評論抓取失敗或沒有評論，請查看摘要表。")
    } else if (!is.null(scraper_result$error_message) && nzchar(scraper_result$error_message)) {
      message("Amazon 評論爬蟲錯誤: ", scraper_result$error_message)
      amazon_review_error("評論抓取失敗，請稍後再試。")
    }

    amazon_review_status(sprintf(
      "Amazon 評論抓取完成：%d 個 ASIN，共 %d 則評論。",
      length(target_asins),
      nrow(reviews)
    ))
  }

  # 模型顯示
  output$token_model_display <- renderUI({
    state <- token_state()
    tags$span(state$model)
  })

  # Token 數量顯示
  output$token_count_display <- renderUI({
    state <- token_state()
    tags$span(format_tokens(state$total_tokens))
  })

  # 費用顯示
  output$token_cost_display <- renderUI({
    state <- token_state()
    tags$span(format_cost(state$total_cost))
  })


  # ==========================================
  # 初始化語言系統（向後兼容）
  # ==========================================

  # 創建 global_lang_content reactive（從 global_language_state 派生）
  if (exists("global_language_state") && is.reactivevalues(global_language_state)) {
    global_lang_content <- reactive({
      # 觸發更新
      global_language_state$update_trigger

      # 返回語言內容
      if (!is.null(global_language_state$language_content)) {
        return(global_language_state$language_content)
      } else {
        return(list())
      }
    })

    message("✅ [Server] global_lang_content reactive 已創建")
  } else {
    # 如果沒有統一語言管理器，創建空的 reactive
    global_lang_content <- reactive({ list() })
    warning("⚠️ [Server] global_language_state 不存在，使用空的 global_lang_content")
  }

  # ==========================================
  # 語言切換處理
  # ==========================================

  # 從導航欄切換語言
  observeEvent(input$navbar_language_selector, {
    req(input$navbar_language_selector)

    new_language <- input$navbar_language_selector
    current_language <- isolate(global_language_state$language_content$language)

    cat("🌍 導航欄語言切換:", new_language, "(當前:", current_language, ")\n")

    # ⚠️ 只有在語言真的改變時才執行切換
    if (new_language != current_language) {
      if (exists("unified_language_switch") && is.function(unified_language_switch)) {
        unified_language_switch(
          session = session,
          new_language = new_language,
          app_config = app_config,
          module_ui_update_trigger = module_ui_update_trigger
        )
        message("✅ 已切換語言:", new_language)

        # ✅ 顯示通知但不重載（避免登出）
        lang_name <- switch(new_language,
          "zh_TW" = "中文",
          "en_US" = "English",
          "ja_JP" = "日本語",
          new_language
        )

        # 根據新語言顯示不同的提示訊息
        notification_msg <- switch(new_language,
          "zh_TW" = paste0("語言已切換為", lang_name, "。側邊欄已更新。請點擊其他頁面以查看完整語言變更。"),
          "en_US" = paste0("Language switched to ", lang_name, ". Sidebar updated. Click another page to see full language change."),
          "ja_JP" = paste0("言語を", lang_name, "に切り替えました。サイドバーが更新されました。完全な言語変更を確認するには、別のページをクリックしてください。"),
          paste0("Language updated to ", lang_name, ". Sidebar menu updated. Click another page to see full change.")
        )

        showNotification(
          notification_msg,
          type = "message",
          duration = 5
        )
        # ⚠️ 不使用 session$reload() - 會導致登出
      }
    } else {
      cat("⏭️  語言未改變，跳過切換\n")
    }
  }, ignoreInit = TRUE)

  # 從首頁切換語言
  observeEvent(input$home_language_change, {
    req(input$home_language_change)

    new_language <- input$home_language_change$language
    cat("🌍 首頁語言切換:", new_language, "\n")

    if (exists("unified_language_switch") && is.function(unified_language_switch)) {
      unified_language_switch(
        session = session,
        new_language = new_language,
        app_config = app_config,
        module_ui_update_trigger = module_ui_update_trigger
      )
      message("✅ 已切換語言:", new_language)
    }
  })

  # 從模組切換語言
  observeEvent(input$global_language_change, {
    req(input$global_language_change)

    new_language <- input$global_language_change$language
    cat("🌍 模組語言切換:", new_language, "\n")

    if (exists("unified_language_switch") && is.function(unified_language_switch)) {
      unified_language_switch(
        session = session,
        new_language = new_language,
        app_config = app_config,
        module_ui_update_trigger = module_ui_update_trigger
      )

      # 發送強制同步消息
      session$sendCustomMessage("force_module_language_sync", list(
        language = new_language,
        timestamp = as.numeric(Sys.time())
      ))

      message("✅ 已切換語言並同步:", new_language)
    }
  })

  # ==========================================
  # 初始化語言從 sessionStorage
  # ==========================================
  observeEvent(input$initial_language, {
    req(input$initial_language)

    saved_language <- input$initial_language
    cat("🌍 從 sessionStorage 載入語言:", saved_language, "\n")

    if (exists("unified_language_switch") && is.function(unified_language_switch)) {
      unified_language_switch(
        session = session,
        new_language = saved_language,
        app_config = app_config,
        module_ui_update_trigger = module_ui_update_trigger
      )
      message("✅ 已恢復語言設定:", saved_language)
    }
  })

  # ==========================================
  # 登入模組處理
  # ==========================================
  login_result <- NULL

  # 優先使用 Supabase 登入模組
  if (exists("USE_SUPABASE_AUTH") && USE_SUPABASE_AUTH && exists("loginSupabaseServer")) {
    message("🔐 使用 Supabase 登入模組")
    login_result <- loginSupabaseServer("login1", app_name = "brandedge")
  } else if (exists("loginModuleServer")) {
    message("🔐 使用傳統 bcrypt 登入模組")
    # Get language texts for login module (use generic "login", not "login_brandedge")
    # Use lang_content (static variable from initialization), not global_lang_content() reactive!
    login_lang_texts <- if (!is.null(lang_content)) {
      get_module_texts(lang_content, "login")
    } else {
      NULL
    }
    login_result <- loginModuleServer("login1", lang_texts = login_lang_texts)
  } else {
    message("⚠️ 未找到任何登入模組 Server 函數")
  }

  if (!is.null(login_result)) {

    observe({
      if (!is.null(login_result$user_info())) {
        user_logged_in(TRUE)
        current_user(login_result$user_info())
        message("✅ 使用者已登入:", login_result$user_info()$username)
      }
    })

    # Handle logout - monitor logged_in reactive value
    observe({
      if (!is.null(login_result$logged_in) && is.function(login_result$logged_in)) {
        logged_in_status <- tryCatch({
          login_result$logged_in()
        }, error = function(e) {
          TRUE  # If error, assume still logged in
        })

        # Detect logout: was logged in (current_user exists), now logged out
        if (!logged_in_status && !is.null(current_user())) {
          user_logged_in(FALSE)
          current_user(NULL)
          message("👋 使用者已登出")
        }
      }
    })
  }

  # ==========================================
  # Logout button handler
  # ==========================================
  observeEvent(input$logout_btn, {
    message("🚪 Logout button clicked")

    # Update state FIRST to trigger UI change
    user_logged_in(FALSE)
    current_user(NULL)

    # Clear upload result if it exists
    if (exists("upload_result") && !is.null(upload_result)) {
      upload_result <- NULL
    }

    # Call login module logout function to clear its internal state
    if (!is.null(login_result) && !is.null(login_result$logout)) {
      tryCatch({
        login_result$logout()
        message("✅ Login module logout called successfully")
      }, error = function(e) {
        message("⚠️ Login module logout error:", e$message)
      })
    }

    # Force session reload to return to login page
    session$reload()

    message("👋 User logged out via button - UI reloading to show login page")
  })

  # ==========================================
  # 上傳模組處理
  # ==========================================
  upload_result <- NULL
  if (exists("uploadModuleServer") && !is.null(login_result)) {
    # Get language texts for upload module (REACTIVE)
    upload_lang_texts <- reactive({
      # Use global_lang_content() reactive to get current language content
      if (exists("global_lang_content") && is.function(global_lang_content)) {
        current_content <- global_lang_content()
        if (!is.null(current_content)) {
          get_module_texts(current_content, "upload_brandedge")
        } else {
          NULL
        }
      } else {
        NULL
      }
    })

    upload_result <- uploadModuleServer(
      "upload_brandedge",
      con = get_db_connection(),
      user_info = login_result$user_info,
      lang_texts = upload_lang_texts,
      module_config = app_config$module_configs[["upload_brandedge"]],
      amazon_rank_results = reactive(amazon_rank_results()),
      amazon_rank_status = reactive(amazon_rank_status()),
      amazon_rank_error = reactive(amazon_rank_error()),
      amazon_review_preview = reactive(amazon_review_preview()),
      amazon_review_summary = reactive(amazon_review_summary()),
      amazon_review_status = reactive(amazon_review_status()),
      amazon_review_error = reactive(amazon_review_error())
    )

    # Monitor Amazon review preview data (with safety check)
    observe({
      data <- amazon_review_preview()
      if (!is.null(data) && is.data.frame(data) && nrow(data) > 0) {
        upload_data(data)
        message("✅ Amazon 評論資料已更新: ", nrow(data), " 筆")
      }
    })

    observeEvent(upload_result$asin_data(), {
      asin_df <- upload_result$asin_data()
      req(!is.null(asin_df), nrow(asin_df) > 0)

      competitor_asins(upload_result$competitor_asin_data())
      amazon_rank_results(NULL)
      amazon_rank_error(NULL)
      amazon_rank_status("Amazon 資料查詢中...")
      amazon_review_preview(NULL)
      amazon_review_summary(NULL)
      amazon_review_error(NULL)
      amazon_review_status(NULL)
      amazon_review_timing(NULL)

      tryCatch({
        withProgress(message = "Amazon 資料查詢中", value = 0, {
          incProgress(0.1, detail = "檢查 ASIN 輸入...")
          amazon_rank_status("Amazon 同產品類別 ASIN 查詢中...")

          incProgress(0.25, detail = "查詢 Amazon 同產品類別排行...")
          scrape_response <- run_amazon_bsr_scraper(asin_df$asin)
          scrape_results <- scrape_response$results

          incProgress(0.15, detail = "整理競品 ASIN 清單...")
          if (is.null(scrape_results) ||
              (is.data.frame(scrape_results) && nrow(scrape_results) == 0) ||
              (!is.data.frame(scrape_results) && length(scrape_results) == 0)) {
            error_message <- scrape_response$error_message %||% "找不到該類別的 Top 10 商品 ASIN。"
            combined_results <- merge_user_competitors(
              scrape_results = data.frame(),
              competitor_df = upload_result$competitor_asin_data(),
              input_asin = asin_df$asin
            )
            if (nrow(combined_results) > 0) {
              amazon_rank_results(combined_results)
            }

            incProgress(0.35, detail = "抓取 Amazon 評論資料...")
            tryCatch({
              run_amazon_review_preview(asin_df$asin, combined_results, upload_result$reviews_per_asin())
            }, error = function(review_error_obj) {
              amazon_review_status(NULL)
              amazon_review_error(review_error_obj$message)
              showNotification(review_error_obj$message, type = "warning", duration = 8)
            })
            amazon_rank_error(error_message)
            amazon_rank_status(NULL)
            incProgress(0.15, detail = "完成")
            return()
          }

          if (!is.data.frame(scrape_results)) {
            scrape_results <- as.data.frame(scrape_results, stringsAsFactors = FALSE)
          }

          combined_results <- merge_user_competitors(
            scrape_results = scrape_results,
            competitor_df = upload_result$competitor_asin_data(),
            input_asin = asin_df$asin
          )

          amazon_rank_results(combined_results)
          amazon_rank_status("Amazon 同產品類別競品查詢完成")

          incProgress(0.35, detail = "抓取 Amazon 評論資料...")
          tryCatch({
            run_amazon_review_preview(asin_df$asin, combined_results, upload_result$reviews_per_asin())
          }, error = function(review_error_obj) {
            amazon_review_status(NULL)
            amazon_review_error(review_error_obj$message)
            showNotification(review_error_obj$message, type = "warning", duration = 8)
          })

          incProgress(0.15, detail = "建立分析資料...")
          if (!is.null(scrape_response$error_message) && nzchar(scrape_response$error_message)) {
            showNotification(scrape_response$error_message, type = "warning", duration = 8)
          }
        })
      }, error = function(e) {
        amazon_rank_results(NULL)
        amazon_rank_status(NULL)
        amazon_rank_error(e$message)
        tryCatch({
          run_amazon_review_preview(asin_df$asin, data.frame(), upload_result$reviews_per_asin())
        }, error = function(review_error_obj) {
          amazon_review_status(NULL)
          amazon_review_error(review_error_obj$message)
        })
        showNotification(e$message, type = "error", duration = 8)
      })
    }, ignoreInit = TRUE)
  }

  # ==========================================
  # 🆕 屬性評分模組 (NEW)
  # ==========================================
  scoring_result <- reactiveValues(
    scored_data = reactive({ NULL }),
    attributes = reactive({ NULL }),
    proceed_trigger = reactive({ 0 })
  )
  scoring_module_result <- NULL

  if (exists("brandedgeScoringModuleServer") && !is.null(upload_result)) {
    message("📊 初始化 BrandEdge 屬性評分模組...")

    # Get language texts for scoring module (REACTIVE)
    scoring_lang_texts <- reactive({
      # Use global_lang_content() reactive to get current language content
      if (exists("global_lang_content") && is.function(global_lang_content)) {
        current_content <- global_lang_content()
        if (!is.null(current_content)) {
          get_module_texts(current_content, "brandedge_scoring")
        } else {
          NULL
        }
      } else {
        NULL
      }
    })

    # Initialize scoring module with config
    scoring_module_result <- brandedgeScoringModuleServer(
      "brandedge_scoring",
      review_data = reactive({
        amazon_reviews <- normalize_amazon_reviews_for_scoring(amazon_review_preview())
        if (!is.null(amazon_reviews) && nrow(amazon_reviews) > 0) {
          return(amazon_reviews)
        }

        NULL
      }),
      lang_texts = scoring_lang_texts,
      con = get_db_connection(),
      user_info = if (!is.null(login_result)) login_result$user_info else NULL,
      module_config = app_config$module_configs[["brandedge_scoring"]],
      api_config = app_config$api$openai  # 共用 API 設定（模型從這裡讀取）
    )

    # Store scoring results
    if (!is.null(scoring_module_result)) {
      scoring_result$scored_data <- scoring_module_result$scored_data
      scoring_result$attributes <- scoring_module_result$attributes
      scoring_result$proceed_trigger <- scoring_module_result$proceed_trigger

      # Monitor scoring data
      observe({
        if (!is.null(scoring_result$scored_data()) && is.data.frame(scoring_result$scored_data())) {
          message("✅ 評分資料已更新: ", nrow(scoring_result$scored_data()), " 筆, ",
                  ncol(scoring_result$scored_data()) - 1, " 個屬性")
        }
      })

      message("✅ BrandEdge 屬性評分模組初始化完成")
    }
  } else {
    message("⚠️  BrandEdge 屬性評分模組未載入")
  }

  # ==========================================
  # 🆕 導航邏輯 - 自動頁面切換 (NEW)
  # ==========================================
  # 監聽 Amazon 評論資料更新 → 切換到評分頁面
  if (!is.null(upload_result)) {
    tryCatch({
      observeEvent(amazon_review_preview(), {
        amazon_reviews_for_scoring <- normalize_amazon_reviews_for_scoring(amazon_review_preview())
        has_amazon_reviews <- !is.null(amazon_reviews_for_scoring) && nrow(amazon_reviews_for_scoring) > 0

        if (has_amazon_reviews) {
          message("🚀 [Navigation] 從上傳頁面切換至評分頁面")
          updateTabItems(session, "sidebar_menu", "brandedge_scoring")
          showNotification(
            get_text("system.notifications.switch_to_scoring", "✅ 切換至評分頁面"),
            type = "message",
            duration = 3
          )
        } else {
          showNotification(
            "無法獲取 Amazon 評論，請確認 ASIN、Amazon cookie 或稍後再試。",
            type = "error",
            duration = 8
          )
        }
      }, ignoreInit = TRUE)
    }, error = function(e) {
      message("⚠️ [Navigation] 上傳導航設定失敗: ", e$message)
    })
  }

  # 監聽評分模組的「下一步」按鈕 → 切換到第一個分析模組
  if (!is.null(scoring_module_result)) {
    tryCatch({
      observeEvent(scoring_result$proceed_trigger(), {
        if (!is.null(scoring_result$scored_data()) && is.data.frame(scoring_result$scored_data())) {
          # 找到第一個啟用的分析模組
          first_analysis_page <- NULL
          for (page in app_config$pages) {
            if (page$enabled &&
                !page$id %in% c("login", "upload_brandedge", "brandedge_scoring")) {
              first_analysis_page <- page$id
              break
            }
          }

          if (!is.null(first_analysis_page)) {
            message("🚀 [Navigation] 從評分頁面切換至分析頁面: ", first_analysis_page)
            updateTabItems(session, "sidebar_menu", first_analysis_page)
            showNotification(
              get_text("system.notifications.enter_analysis", "✅ 進入分析模組"),
              type = "message",
              duration = 3
            )
          }
        }
      }, ignoreInit = TRUE)
    }, error = function(e) {
      message("⚠️ [Navigation] 評分導航設定失敗: ", e$message)
    })
  }

  # ==========================================
  # 動態模組初始化 - BrandEdge Submodules
  # ==========================================
  # 為每個啟用的頁面初始化模組 Server
  for (page in app_config$pages) {
    # Skip login and brandedge_scoring (already initialized above)
    if (!is.null(page$module) && page$enabled &&
        !page$id %in% c("login", "brandedge_scoring")) {
      local({
        page_id <- page$id
        module_name <- page$module

        cat("📦 初始化模組 Server:", module_name, "(ID:", page_id, ")\n")

        # 處理命名映射
        name_mappings <- list(
          "market_profile" = "marketProfile",
          "market_track" = "marketTrack",
          "advanced_attribute" = "advancedAttribute",
          "key_factor" = "keyFactor",  # 🆕 NEW: 關鍵因素分析
          "advanced_dna" = "advancedDNA",
          "brand_identity" = "brandIdentity",
          "ideal_point" = "idealPoint",
          "positioning_strategy" = "positioningStrategy",
          "brandedge_scoring" = "brandedgeScoring"
        )

        base_name <- name_mappings[[module_name]]
        if (is.null(base_name)) base_name <- module_name

        server_func_name <- paste0(base_name, "ModuleServer")

        if (exists(server_func_name)) {
          server_func <- get(server_func_name)

          # 建立動態語言內容的 reactive
          module_lang_texts <- reactive({
            current_lang <- global_lang_content()
            cat("📦 [Module Lang Texts] Module:", module_name, "\n")
            if (!is.null(current_lang)) {
              cat("  - global_lang_content() language:", current_lang$language %||% "NULL", "\n")
              module_texts <- get_module_texts(current_lang, module_name)
              if (!is.null(module_texts)) {
                cat("  - module_texts language:", module_texts$language %||% "NULL", "\n")
              } else {
                cat("  - module_texts is NULL\n")
              }
              return(module_texts)
            } else {
              cat("  - global_lang_content() is NULL\n")
              NULL
            }
          })

          tryCatch({
            # 準備數據 - 只使用評分後的數據（與原始 BrandEdge 一致）
            # ⚠️ 分析模組只能在完成評分後使用
            analysis_data <- reactive({
              cat("🔍 [analysis_data] Checking scored data...\n")

              scored <- scoring_result$scored_data()
              req(scored)  # 要求必須有評分數據

              cat("  ✅ scored_data:", nrow(scored), "rows,", ncol(scored), "cols\n")
              cat("  - Columns:", paste(head(names(scored), 5), collapse=", "), "...\n")

              scored
            })

            # 🆕 計算關鍵屬性 (Key Attributes)
            # 基於變異度選擇最重要的5個屬性
            key_attributes <- reactive({
              df <- analysis_data()
              req(!is.null(df) && nrow(df) > 0)

              # 獲取所有數值欄位（排除 Variation）
              attrs <- setdiff(names(df), "Variation")
              attrs <- attrs[sapply(df[attrs], is.numeric)]

              if (length(attrs) == 0) return(character(0))

              # 計算每個屬性的變異度
              variances <- df %>%
                select(all_of(attrs)) %>%
                summarise(across(everything(), ~var(., na.rm = TRUE))) %>%
                pivot_longer(everything(), names_to = "attr", values_to = "variance") %>%
                arrange(desc(variance))

              # 返回前5個變異度最高的屬性
              head(variances$attr, 5)
            })

            # 🆕 獲取模組配置
            module_config <- app_config$module_configs[[module_name]]

            # 🆕 注意: attributes 參數將在未來版本添加到各模組
            # 目前先使用原有參數以保持兼容性
            if (module_name == "market_profile") {
              server_func(page_id,
                         data = analysis_data,
                         brand_data = analysis_data,
                         lang_texts = module_lang_texts,
                         module_config = module_config,
                         api_config = app_config$api$openai)  # 共用 API 設定
            } else if (module_name == "market_track") {
              server_func(page_id,
                         data = analysis_data,
                         key_vars = reactive(NULL),
                         lang_texts = module_lang_texts,
                         module_config = module_config)
            } else if (module_name == "advanced_attribute") {
              server_func(page_id,
                         review_data = analysis_data,
                         lang_texts = module_lang_texts,
                         module_config = module_config,
                         api_config = app_config$api$openai)  # 共用 API 設定
            } else if (module_name == "advanced_dna") {
              server_func(page_id,
                         data_full = analysis_data,
                         lang_texts = module_lang_texts,
                         module_config = module_config,
                         api_config = app_config$api$openai)  # 共用 API 設定
            } else if (module_name == "brand_identity") {
              server_func(page_id,
                         data = analysis_data,
                         key_vars = key_attributes,  # 🆕 使用 key_attributes
                         lang_texts = module_lang_texts,
                         module_config = module_config,
                         api_config = app_config$api$openai)  # 共用 API 設定
            } else if (module_name == "ideal_point") {
              server_func(page_id,
                         data = analysis_data,
                         raw = upload_data,
                         indicator = analysis_data,
                         key_vars = key_attributes,  # 🆕 使用 key_attributes
                         lang_texts = module_lang_texts,
                         module_config = module_config)
            } else if (module_name == "positioning_strategy") {
              server_func(page_id,
                         data = analysis_data,
                         key_vars = key_attributes,  # 🆕 使用 key_attributes
                         lang_texts = module_lang_texts,
                         module_config = module_config,
                         api_config = app_config$api$openai)  # 共用 API 設定
            } else if (module_name == "key_factor") {
              server_func(page_id,
                         data = analysis_data,
                         key_vars = key_attributes,
                         lang_texts = module_lang_texts,
                         module_config = module_config)
            } else {
              # 默認調用
              server_func(page_id, lang_texts = module_lang_texts, module_config = module_config)
            }
            cat("✅ 模組 Server 初始化成功:", module_name, "\n")
          }, error = function(e) {
            cat("❌ 模組 Server 初始化失敗:", module_name, "-", e$message, "\n")
          })
        } else {
          cat("⚠️ 找不到模組函數:", server_func_name, "\n")
        }
      })
    }
  }

  # ==========================================
  # 動態頁面標題渲染器 (Following InsightForge Pattern)
  # ==========================================
  # 為每個頁面渲染動態標題
  for (page in app_config$pages) {
    local({
      page_id <- page$id

      output[[paste0("page_title_", page_id)]] <- renderUI({
        # 監聽語言更新觸發器
        module_ui_update_trigger()

        # 取得當前語言內容
        current_lang_content <- global_lang_content()

        # 獲取頁面標題翻譯
        page_title <- if (!is.null(current_lang_content) &&
                         !is.null(current_lang_content$content$common$pages) &&
                         !is.null(current_lang_content$content$common$pages[[page_id]])) {
          current_lang_content$content$common$pages[[page_id]]
        } else {
          page$title  # 備用原始標題
        }

        h3(page_title)
      })
    })
  }

  # ==========================================
  # 動態 UI 渲染器 - 為每個模組頁面生成 UI (Following InsightForge Pattern)
  # ==========================================
  # 為每個啟用的模組生成 renderUI (不使用 observe 包裝)
  for (page in app_config$pages) {
    if (!is.null(page$module) && page$enabled && !page$id %in% c("login")) {
      local({
        page_id <- page$id
        module_name <- page$module
        output_id <- paste0(page_id, "_", module_name, "_ui_container")

        output[[output_id]] <- renderUI({
          # 監聽語言更新觸發器 (INSIDE renderUI like InsightForge)
          module_ui_update_trigger()

          tryCatch({
            # 在 renderUI 內部獲取 lang_content
            lang_content <- global_lang_content()

            # 獲取模組語言文本
            module_texts <- if (!is.null(lang_content)) {
              get_module_texts(lang_content, module_name)
            } else {
              NULL
            }

            # 處理命名映射
            name_mappings <- list(
              "market_profile" = "marketProfile",
              "market_track" = "marketTrack",
              "advanced_attribute" = "advancedAttribute",
              "key_factor" = "keyFactor",  # 🆕 NEW: 關鍵因素分析
              "advanced_dna" = "advancedDNA",
              "brand_identity" = "brandIdentity",
              "ideal_point" = "idealPoint",
              "positioning_strategy" = "positioningStrategy",
              "brandedge_scoring" = "brandedgeScoring",
              "upload_brandedge" = "upload"
            )

            base_name <- name_mappings[[module_name]]
            if (is.null(base_name)) base_name <- module_name

            ui_func_name <- paste0(base_name, "ModuleUI")

            if (exists(ui_func_name)) {
              ui_func <- get(ui_func_name)

              # 🔍 Get module config for this module
              module_config <- app_config$module_configs[[module_name]]

              # 🔍 Debug: Log config loading
              if (!is.null(module_config)) {
                cat("✅ [UI Render] Passing config to", ui_func_name, "\n")
              } else {
                cat("⚠️  [UI Render] No config found for module:", module_name, "\n")
              }

              # Pass module_config and lang_texts to UI function
              ui_func(page_id, module_config = module_config, lang_texts = module_texts)
            } else {
              div(
                class = "alert alert-warning",
                paste("找不到模組 UI 函數:", ui_func_name)
              )
            }
          }, error = function(e) {
            div(
              class = "alert alert-danger",
              h4("模組載入失敗"),
              p(paste("模組:", module_name)),
              p(paste("錯誤:", e$message))
            )
          })
        })
      })
    }
  }

  # ==========================================
  # Session 結束處理
  # ==========================================
  session$onSessionEnded(function() {
    message("Session 結束")
  })
}

# ==========================================
# 執行應用程式
# ==========================================

# 正確關閉連接池：App 停止時關閉 Pool
onStop(function() {
  if (exists("close_pool") && is.function(close_pool)) {
    close_pool()
  }
})

shinyApp(ui = ui, server = server)

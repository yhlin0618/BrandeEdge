# ============================================================================
# InsightForge 套件管理
# ============================================================================

# ── 必要套件列表 ──────────────────────────────────────────────────────────
# 初始化函數 - 載入所有必要套件
initialize_packages <- function() {
  # 先載入關鍵套件
  library(DBI)
  library(RPostgres)
  library(RSQLite)
  library(dotenv)

  message("✅ 已載入資料庫套件")
}

REQUIRED_PACKAGES <- c(
  # Shiny 核心
  "shiny",
  "shinyjs",
  
  # 資料庫
  "DBI",
  "RSQLite",
  "RPostgres",
  "RMariaDB",
  
  # 安全性
  "bcrypt",
  
  # 資料處理
  "readxl",
  "jsonlite",
  "dplyr",
  "tidyverse",
  "stringr",
  
  # HTTP 和 API
  "httr",
  "httr2",
  
  # 視覺化
  "DT",
  "GGally",
  "plotly",
  
  # UI 和主題
  "bslib",
  "bs4Dash",
  
  # 環境設定
  "dotenv",
  
  # 資料庫相關
  "duckdb",
  
  # 平行處理
  "future",
  "furrr",
  
  # 文檔處理
  "markdown"
)

# ── 套件安裝函數 ──────────────────────────────────────────────────────────
install_required_packages <- function(force = FALSE) {
  cat("🔍 檢查必要套件...\n")
  
  installed <- installed.packages()[, "Package"]
  missing <- REQUIRED_PACKAGES[!REQUIRED_PACKAGES %in% installed]
  
  if (length(missing) == 0 && !force) {
    cat("✅ 所有必要套件都已安裝\n")
    return(invisible(TRUE))
  }
  
  if (force) {
    to_install <- REQUIRED_PACKAGES
    cat("🔄 強制重新安裝所有套件...\n")
  } else {
    to_install <- missing
    cat("📦 發現", length(missing), "個缺少的套件:", paste(missing, collapse = ", "), "\n")
  }
  
  cat("⏳ 開始安裝套件...\n")
  
  for (pkg in to_install) {
    cat("  安裝", pkg, "...")
    tryCatch({
      install.packages(pkg, quiet = TRUE)
      cat(" ✅\n")
    }, error = function(e) {
      cat(" ❌ 錯誤:", e$message, "\n")
    })
  }
  
  cat("🎉 套件安裝完成！\n")
}

# ── 載入套件函數 ──────────────────────────────────────────────────────────
load_packages <- function() {
  cat("📚 載入必要套件...\n")
  
  # 載入套件並處理衝突
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(DBI)
    library(RSQLite)
    library(RPostgres)
    library(RMariaDB)
    library(bcrypt)
    library(readxl)
    library(jsonlite)
    library(httr)
    library(DT)
    library(dplyr)
    library(GGally)
    library(tidyverse)
    library(stringr)
    library(bslib)
    library(bs4Dash)
    library(dotenv)
    library(plotly)
    library(duckdb)
    library(httr2)
    library(future)
    library(furrr)
    library(markdown)
  })
  
  # ── 處理套件衝突 ──────────────────────────────────────────────────────
  # 明確指定要使用的函數
  show <- shinyjs::show
  hide <- shinyjs::hide
  filter <- dplyr::filter
  lag <- dplyr::lag
  validate <- shiny::validate
  
  # 將函數指派到全域環境
  assign("show", show, envir = .GlobalEnv)
  assign("hide", hide, envir = .GlobalEnv)
  assign("filter", filter, envir = .GlobalEnv)
  assign("lag", lag, envir = .GlobalEnv)
  assign("validate", validate, envir = .GlobalEnv)
  
  # 確保 shiny HTML tags 可用
  tags <- shiny::tags
  assign("tags", tags, envir = .GlobalEnv)
  
  # 定義 null coalescing operator
  `%||%` <- function(x, y) if (is.null(x)) y else x
  assign("%||%", `%||%`, envir = .GlobalEnv)
  
  cat("✅ 套件載入完成\n")
}

# ── 檢查套件版本 ──────────────────────────────────────────────────────────
check_package_versions <- function() {
  cat("📋 套件版本資訊:\n")
  
  for (pkg in REQUIRED_PACKAGES) {
    if (pkg %in% installed.packages()[, "Package"]) {
      version <- packageVersion(pkg)
      cat("  ", pkg, ":", as.character(version), "\n")
    } else {
      cat("  ", pkg, ": ❌ 未安裝\n")
    }
  }
}

# ── 平行處理設定 ──────────────────────────────────────────────────────────
setup_parallel_processing <- function() {
  # 根據環境設定平行處理
  if (Sys.getenv("SHINY_PORT") != "") {
    # 在 shinyapps.io 上運行
    plan(sequential)
    cat("🔄 設定為循序處理模式 (shinyapps.io)\n")
  } else {
    # 本地運行
    workers <- min(2, parallel::detectCores() - 1)
    plan(multisession, workers = workers)
    cat("🚀 設定平行處理模式，工作程序數:", workers, "\n")
  }
  
  # 關閉隨機種子警告
  options(future.rng.onMisuse = "ignore")
}

# ── 初始化函數 ────────────────────────────────────────────────────────────
initialize_packages <- function(install_missing = TRUE, check_versions = FALSE) {
  cat("🚀 初始化 InsightForge 套件環境\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  if (install_missing) {
    install_required_packages()
  }
  
  load_packages()
  setup_parallel_processing()
  
  if (check_versions) {
    check_package_versions()
  }
  
  cat(paste(rep("=", 50), collapse = ""), "\n")
  cat("🎉 初始化完成！\n")
} 
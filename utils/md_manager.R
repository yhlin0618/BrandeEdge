# =============================================================================
# Markdown Manager - 動態多語言 Markdown 內容管理系統
# =============================================================================
# 基於 MAMBA 架構原則，提供統一的 Markdown 內容切換和管理機制
# Version: 1.0
# Last Updated: 2025-09-29

library(shiny)
library(yaml)

# ── 語言代碼轉目錄名稱 (動態從 YAML 讀取) ───────────────────────────────────
get_language_dir_from_config <- function(language_code) {
  tryCatch({
    lang_config <- yaml::read_yaml("config/languages.yaml")
    for (lang in lang_config$supported_languages) {
      if (lang$code == language_code && lang$enabled) {
        return(lang$dir)
      }
    }
    fallback_code <- lang_config$fallback$fallback_language
    for (lang in lang_config$supported_languages) {
      if (lang$code == fallback_code) {
        return(lang$dir)
      }
    }
    return("chinese")
  }, error = function(e) {
    warning("Failed to load language config: ", e$message)
    return("chinese")
  })
}

# =============================================================================
# 核心功能：動態語言化 Markdown 載入
# =============================================================================

#' 載入語言化的 Markdown 檔案
#' @param filename 檔案名稱 (如 "help.md")
#' @param lang_content 當前語言內容物件 (reactive 或靜態)
#' @param fallback_path 備用檔案路徑
#' @param cache_enabled 是否啟用快取
#' @return Markdown 內容的 HTML 標籤
load_language_markdown <- function(filename, lang_content = NULL, fallback_path = NULL, cache_enabled = TRUE) {

  # 決定語言目錄
  language_dir <- determine_language_dir(lang_content)

  # 建立語言化檔案路徑
  markdown_paths <- build_markdown_paths(filename, language_dir)

  # 嘗試載入檔案
  content <- try_load_markdown_file(markdown_paths, fallback_path)

  return(content)
}

#' Reactive 版本的 Markdown 載入器
#' @param filename 檔案名稱
#' @param global_lang_content 全域 reactive 語言內容
#' @param fallback_path 備用路徑
#' @return reactive UI 元素
reactive_language_markdown <- function(filename, global_lang_content = NULL, fallback_path = NULL) {

  if (is.null(global_lang_content)) {
    # 非 reactive 模式
    return(load_language_markdown(filename, fallback_path = fallback_path))
  }

  # Reactive 模式
  renderUI({
    current_lang <- global_lang_content()
    load_language_markdown(filename, current_lang, fallback_path)
  })
}

# =============================================================================
# 輔助函數
# =============================================================================

#' 決定語言目錄名稱
determine_language_dir <- function(lang_content) {
  language_code <- "zh_TW"  # 預設語言代碼

  if (!is.null(lang_content)) {
    # 處理 reactive 和非 reactive 內容
    actual_content <- if (is.function(lang_content)) {
      tryCatch(lang_content(), error = function(e) NULL)
    } else {
      lang_content
    }

    if (!is.null(actual_content) && !is.null(actual_content$language)) {
      language_code <- actual_content$language
    }
  }

  # 動態從 YAML 配置獲取目錄名稱
  return(get_language_dir_from_config(language_code))
}

#' 建立 Markdown 檔案路徑清單
build_markdown_paths <- function(filename, language_dir) {
  base_content_path <- "database/content"

  paths <- list(
    # 主要語言化路徑
    primary = file.path(base_content_path, language_dir, "markdown", filename),

    # 次要語言化路徑（直接在語言目錄下）
    secondary = file.path(base_content_path, language_dir, filename),

    # 備用中文路徑
    fallback_cn = file.path(base_content_path, "chinese", "markdown", filename),

    # 原始 md 目錄路徑
    original = file.path("md", filename)
  )

  return(paths)
}

#' 嘗試載入 Markdown 檔案
try_load_markdown_file <- function(paths, fallback_path = NULL) {

  # 按優先順序嘗試載入
  for (path_name in names(paths)) {
    path <- paths[[path_name]]

    if (file.exists(path)) {
      tryCatch({
        cat("✅ [MD Manager] 成功載入:", path, "\n")
        return(includeMarkdown(path))
      }, error = function(e) {
        cat("❌ [MD Manager] 載入失敗:", path, "-", e$message, "\n")
        next
      })
    }
  }

  # 嘗試使用自訂備用路徑
  if (!is.null(fallback_path) && file.exists(fallback_path)) {
    tryCatch({
      cat("🔄 [MD Manager] 使用備用路徑:", fallback_path, "\n")
      return(includeMarkdown(fallback_path))
    }, error = function(e) {
      cat("❌ [MD Manager] 備用路徑載入失敗:", e$message, "\n")
    })
  }

  # 都無法載入時返回錯誤訊息
  error_filename <- basename(paths$primary)
  cat("⚠️ [MD Manager] 無法找到檔案:", error_filename, "\n")

  return(div(
    class = "alert alert-warning",
    style = "margin: 10px 0; padding: 15px;",
    icon("exclamation-triangle"),
    tags$strong(" 檔案載入失敗: "),
    error_filename,
    br(),
    tags$small("已搜尋的路徑: ", paste(unlist(paths), collapse = ", "))
  ))
}

# =============================================================================
# 進階功能：批次 Markdown 載入
# =============================================================================

#' 批次載入多個 Markdown 檔案
#' @param filenames 檔案名稱向量
#' @param lang_content 語言內容
#' @param as_tabs 是否作為分頁顯示
#' @return HTML 內容清單或分頁 UI
batch_load_markdown <- function(filenames, lang_content = NULL, as_tabs = FALSE) {

  contents <- lapply(filenames, function(filename) {
    load_language_markdown(filename, lang_content)
  })
  names(contents) <- tools::file_path_sans_ext(filenames)

  if (as_tabs) {
    # 建立分頁 UI
    tabs <- mapply(function(name, content) {
      tabPanel(title = name, content)
    }, names(contents), contents, SIMPLIFY = FALSE)

    return(do.call(tabsetPanel, tabs))
  }

  return(contents)
}

# =============================================================================
# 特殊用途：通知和說明內容管理
# =============================================================================

#' 載入通知內容的語言化版本
load_notification_content <- function(notification_type = "general", lang_content = NULL) {
  filename <- paste0("notifications_", notification_type, ".md")
  return(load_language_markdown(filename, lang_content))
}

#' 載入說明文件的語言化版本
load_help_content <- function(help_section = "general", lang_content = NULL) {
  filename <- paste0("help_", help_section, ".md")
  return(load_language_markdown(filename, lang_content))
}

# =============================================================================
# 模組整合：與現有系統整合
# =============================================================================

#' 整合到現有模組的輔助函數
#' @param module_name 模組名稱
#' @param content_type 內容類型 (help, notification, etc.)
#' @param global_lang_content 全域語言內容
integrate_with_module <- function(module_name, content_type = "help", global_lang_content = NULL) {
  filename <- paste0(module_name, "_", content_type, ".md")

  if (is.function(global_lang_content)) {
    # Reactive 模式
    return(reactive_language_markdown(filename, global_lang_content))
  } else {
    # 靜態模式
    return(load_language_markdown(filename, global_lang_content))
  }
}

# =============================================================================
# 匯出函數列表
# =============================================================================
md_manager_exports <- list(
  load_language_markdown = load_language_markdown,
  reactive_language_markdown = reactive_language_markdown,
  batch_load_markdown = batch_load_markdown,
  load_notification_content = load_notification_content,
  load_help_content = load_help_content,
  integrate_with_module = integrate_with_module
)

# =============================================================================
# 向後兼容性
# =============================================================================

# 為了保持與現有 language_manager.R 的兼容性
if (!exists("includeLanguageMarkdown")) {
  includeLanguageMarkdown <- load_language_markdown
}

if (!exists("reactiveLanguageMarkdown")) {
  reactiveLanguageMarkdown <- reactive_language_markdown
}

cat("✅ [MD Manager] Markdown 管理系統載入完成\n")
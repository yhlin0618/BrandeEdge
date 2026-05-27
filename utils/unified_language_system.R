# =============================================================================
# 統一語言系統管理器 - Unified Language System Manager
# =============================================================================
# 基於 MAMBA 架構原則，統一管理跨模組的語言切換、Hint 系統和 Prompt 管理
# 確保語言切換時所有組件同步更新
# Version: 1.0
# Last Updated: 2025-09-29

library(shiny)

# =============================================================================
# 核心類別：統一語言管理器
# =============================================================================

#' 建立統一語言系統管理器
#' @param initial_language 初始語言代碼
#' @param config 語言配置
#' @return 語言系統管理器物件
create_unified_language_system <- function(initial_language = "zh_TW", config = NULL) {

  # 載入配置
  if (is.null(config) && exists("load_language_config")) {
    config <- load_language_config()
  }

  # 建立 reactive values
  values <- reactiveValues(
    current_language = initial_language,
    language_content = NULL,
    hints_cache = list(),
    prompts_cache = list(),
    last_update = Sys.time()
  )

  # 初始載入語言內容
  if (exists("load_language_content")) {
    initial_content <- load_language_content(config, initial_language)
    values$language_content <- initial_content
  }

  # 返回管理器物件
  list(
    values = values,
    config = config,

    # 核心方法
    get_current_language = function() tryCatch(values$current_language, error = function(e) "unknown"),
    get_language_content = function() values$language_content,
    switch_language = function(new_language) switch_language_impl(values, config, new_language),

    # Hint 系統方法
    get_hints = function(language = NULL) get_hints_impl(values, language),
    reload_hints = function(language = NULL) reload_hints_impl(values, language),

    # Prompt 系統方法
    get_prompts = function(language = NULL) get_prompts_impl(values, language),
    reload_prompts = function(language = NULL) reload_prompts_impl(values, language),

    # 模組整合方法
    create_module_lang_texts = function() create_reactive_lang_texts(values),
    create_module_hints = function() create_reactive_hints(values),

    # 工具方法
    validate_system = function() validate_system_impl(values, config),
    get_system_status = function() get_system_status_impl(values, config)
  )
}

# =============================================================================
# 核心實作函數
# =============================================================================

#' 切換語言實作
switch_language_impl <- function(values, config, new_language) {

  current <- tryCatch(values$current_language, error = function(e) "unknown")
  cat("🌐 [統一語言系統] 切換語言:", current, "->", new_language, "\n")

  tryCatch({
    # 載入新語言內容
    if (exists("load_language_content")) {
      new_content <- load_language_content(config, new_language)
      values$language_content <- new_content
      values$current_language <- new_language
      values$last_update <- Sys.time()

      # 清除快取強制重新載入
      values$hints_cache <- list()
      values$prompts_cache <- list()

      cat("✅ [統一語言系統] 語言切換成功:", new_language, "\n")
      return(TRUE)
    } else {
      cat("❌ [統一語言系統] load_language_content 函數不存在\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("❌ [統一語言系統] 語言切換失敗:", e$message, "\n")
    return(FALSE)
  })
}

#' 獲取 Hints 實作
get_hints_impl <- function(values, language = NULL) {

  target_language <- language %||% tryCatch(values$current_language, error = function(e) "zh_TW")

  # 檢查快取
  if (!is.null(values$hints_cache[[target_language]])) {
    cat("🔄 [統一語言系統] 使用 hints 快取:", target_language, "\n")
    return(values$hints_cache[[target_language]])
  }

  # 載入 hints
  if (exists("load_hints")) {
    tryCatch({
      hints <- load_hints(language = target_language)
      values$hints_cache[[target_language]] <- hints
      cat("✅ [統一語言系統] 載入 hints:", target_language, "- 數量:", if(!is.null(hints)) nrow(hints) else 0, "\n")
      return(hints)
    }, error = function(e) {
      cat("❌ [統一語言系統] hints 載入失敗:", e$message, "\n")
      return(NULL)
    })
  }

  return(NULL)
}

#' 重新載入 Hints 實作
reload_hints_impl <- function(values, language = NULL) {
  target_language <- language %||% tryCatch(values$current_language, error = function(e) "zh_TW")
  values$hints_cache[[target_language]] <- NULL
  return(get_hints_impl(values, target_language))
}

#' 獲取 Prompts 實作
get_prompts_impl <- function(values, language = NULL) {

  target_language <- language %||% tryCatch(values$current_language, error = function(e) "zh_TW")

  # 檢查快取
  if (!is.null(values$prompts_cache[[target_language]])) {
    cat("🔄 [統一語言系統] 使用 prompts 快取:", target_language, "\n")
    return(values$prompts_cache[[target_language]])
  }

  # 載入 prompts
  if (exists("load_prompts")) {
    tryCatch({
      # Get app_name from environment variable or use default
      app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
      prompts <- load_prompts(language = target_language, app_name = app_name)
      values$prompts_cache[[target_language]] <- prompts
      cat("✅ [統一語言系統] 載入 prompts:", target_language, "app:", app_name, "\n")
      return(prompts)
    }, error = function(e) {
      cat("❌ [統一語言系統] prompts 載入失敗:", e$message, "\n")
      return(NULL)
    })
  }

  return(NULL)
}

#' 重新載入 Prompts 實作
reload_prompts_impl <- function(values, language = NULL) {
  target_language <- language %||% tryCatch(values$current_language, error = function(e) "zh_TW")
  values$prompts_cache[[target_language]] <- NULL
  return(get_prompts_impl(values, target_language))
}

# =============================================================================
# 模組整合功能
# =============================================================================

#' 建立模組用的 reactive 語言文本
create_reactive_lang_texts <- function(values) {
  reactive({
    list(
      language = values$current_language,
      content = if (!is.null(values$language_content)) values$language_content$content else NULL,
      last_update = values$last_update
    )
  })
}

#' 建立模組用的 reactive hints
create_reactive_hints <- function(values) {
  reactive({
    current_lang <- tryCatch(values$current_language, error = function(e) "zh_TW")
    get_hints_impl(values, current_lang)
  })
}

# =============================================================================
# 系統驗證和狀態檢查
# =============================================================================

#' 驗證系統完整性
validate_system_impl <- function(values, config) {

  issues <- list()

  # 檢查語言內容
  if (is.null(values$language_content)) {
    issues <- append(issues, "語言內容未載入")
  }

  # 檢查必要函數
  required_functions <- c("load_language_content", "load_hints", "load_prompts")
  for (func in required_functions) {
    if (!exists(func)) {
      issues <- append(issues, paste("缺少必要函數:", func))
    }
  }

  # 檢查語言檔案
  if (!is.null(config)) {
    for (lang in config$supported_languages) {
      if (lang$enabled) {
        lang_dir <- file.path("database/content", lang$dir)
        if (!dir.exists(lang_dir)) {
          issues <- append(issues, paste("語言目錄不存在:", lang_dir))
        }
      }
    }
  }

  result <- list(
    valid = length(issues) == 0,
    issues = issues,
    timestamp = Sys.time()
  )

  if (result$valid) {
    cat("✅ [統一語言系統] 系統驗證通過\n")
  } else {
    cat("❌ [統一語言系統] 系統驗證失敗，發現", length(issues), "個問題\n")
    for (issue in issues) {
      cat("  -", issue, "\n")
    }
  }

  return(result)
}

#' 獲取系統狀態
get_system_status_impl <- function(values, config) {

  status <- list(
    current_language = tryCatch(values$current_language, error = function(e) "unknown"),
    content_loaded = !is.null(values$language_content),
    hints_cached = length(values$hints_cache),
    prompts_cached = length(values$prompts_cache),
    last_update = values$last_update,
    supported_languages = if (!is.null(config)) {
      sapply(config$supported_languages, function(x) x$code)
    } else {
      NULL
    }
  )

  return(status)
}

# =============================================================================
# 便利函數
# =============================================================================

#' NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' 模組輔助函數：安全獲取語言值
safe_get_lang_value <- function(lang_texts, path, default = NULL) {

  if (is.null(lang_texts)) return(default)

  # 處理 reactive
  texts <- if (is.function(lang_texts)) {
    tryCatch(lang_texts(), error = function(e) NULL)
  } else {
    lang_texts
  }

  if (is.null(texts) || is.null(texts$content)) return(default)

  # 解析路徑
  paths <- strsplit(path, "\\$|\\.", fixed = FALSE)[[1]]
  value <- texts$content

  for (p in paths) {
    if (!is.null(value) && is.list(value) && p %in% names(value)) {
      value <- value[[p]]
    } else {
      return(default)
    }
  }

  return(if (!is.null(value)) value else default)
}

# =============================================================================
# 匯出和初始化
# =============================================================================

# 全域統一語言系統實例（單例模式）
.global_unified_language_system <- NULL

#' 獲取全域統一語言系統
get_unified_language_system <- function() {
  if (is.null(.global_unified_language_system)) {
    .global_unified_language_system <<- create_unified_language_system()
  }
  return(.global_unified_language_system)
}

#' 初始化全域統一語言系統
init_unified_language_system <- function(initial_language = "zh_TW", config = NULL) {
  .global_unified_language_system <<- create_unified_language_system(initial_language, config)
  cat("🚀 [統一語言系統] 全域系統初始化完成\n")
  return(.global_unified_language_system)
}

# 匯出函數列表
unified_language_system_exports <- list(
  create_unified_language_system = create_unified_language_system,
  get_unified_language_system = get_unified_language_system,
  init_unified_language_system = init_unified_language_system,
  safe_get_lang_value = safe_get_lang_value
)

cat("✅ [統一語言系統] 系統載入完成\n")
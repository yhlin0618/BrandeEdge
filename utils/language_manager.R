# ==========================================
# 語言管理器 - Language Manager
# ==========================================
# 負責載入和管理多語言內容
# Version: 1.0
# Last Updated: 2025-09-28

library(yaml)

# ==========================================
# 載入語言配置
# ==========================================
load_language_config <- function(config_path = "config/languages.yaml") {
  if (file.exists(config_path)) {
    return(yaml::read_yaml(config_path))
  } else {
    # 使用預設配置
    warning(paste("語言配置文件不存在：", config_path, "，使用預設配置"))
    return(list(
      default_language = "zh_TW",
      supported_languages = list(
        list(code = "zh_TW", dir = "chinese", enabled = TRUE),
        list(code = "en_US", dir = "english", enabled = TRUE)
      ),
      fallback = list(fallback_language = "zh_TW")
    ))
  }
}

# ==========================================
# 載入語言內容
# ==========================================
load_language_content <- function(config = NULL, language = NULL) {
  # 載入語言配置（如果沒有提供）
  if (is.null(config)) {
    config <- load_language_config()
  }

  # 決定使用的語言
  if (is.null(language)) {
    language <- config$default_language %||% "zh_TW"
  }

  # 獲取語言信息
  lang_info <- get_language_info_from_config(config, language)
  if (is.null(lang_info)) {
    message(paste("語言", language, "不可用，使用預設語言"))
    language <- config$language$fallback %||% config$language$default
    lang_info <- get_language_info_from_config(config, language)
  }

  # 建立語言內容路徑
  content_path <- "database/content"
  language_dir <- if (!is.null(lang_info)) lang_info$dir else "chinese"
  language_path <- file.path(content_path, language_dir)

  # 檢查語言目錄是否存在
  if (!dir.exists(language_path)) {
    # 嘗試使用備用語言
    fallback_language <- config$language$fallback %||% config$language$default
    fallback_info <- get_language_info_from_config(config, fallback_language)
    fallback_dir <- if (!is.null(fallback_info)) fallback_info$dir else "chinese"
    language_path <- file.path(content_path, fallback_dir)

    if (!dir.exists(language_path)) {
      # 返回空的語言內容
      warning(paste("找不到語言內容目錄：", language_path))
      return(list(
        language = language,
        content = list()
      ))
    }

    message(paste("語言", language, "內容不存在，使用備用語言：", fallback_language))
    language <- fallback_language
    language_dir <- fallback_dir
  }

  # 載入語言檔案
  lang_content <- list()
  lang_content$language <- language
  lang_content$content <- list()

  # 載入通用內容 (支援 app-specific 目錄結構)
  # 從 config 獲取 app_name，如果未設定則從環境變數取得，最後預設為 insightforge
  app_name <- config$app_info$app_id %||% Sys.getenv("APP_TYPE", unset = "insightforge")
  common_file <- file.path(language_path, "general", app_name, "common.yaml")

  # 如果 app-specific 路徑不存在，回退到舊路徑
  if (!file.exists(common_file)) {
    common_file <- file.path(language_path, "general", "common.yaml")
  }

  if (file.exists(common_file)) {
    tryCatch({
      lang_content$content$common <- yaml::read_yaml(common_file)
      message(paste("✅ 已載入通用語言內容：", language, " (", basename(dirname(common_file)), ")"))
    }, error = function(e) {
      warning(paste("載入 common.yaml 失敗：", e$message))
    })
  }

  # 載入模組內容（從 module 資料夾載入各個模組檔案）
  module_path <- file.path(language_path, "module")
  if (dir.exists(module_path)) {
    lang_content$content$modules <- list()

    # 取得當前應用的模組列表（如果有 config）
    enabled_modules <- NULL
    if (!is.null(config) && !is.null(config$modules)) {
      enabled_modules <- sapply(config$modules, function(m) m$id %||% "")
      enabled_modules <- enabled_modules[nzchar(enabled_modules)]
    }

    # 載入模組 YAML 檔案（僅載入已啟用的模組）
    module_files <- list.files(module_path, pattern = "\\.yaml$", full.names = TRUE)
    loaded_count <- 0

    for (file in module_files) {
      module_name <- tools::file_path_sans_ext(basename(file))

      # 如果有啟用模組列表，只載入列表中的模組
      if (!is.null(enabled_modules) && !module_name %in% enabled_modules) {
        # 跳過未啟用的模組
        next
      }

      tryCatch({
        lang_content$content$modules[[module_name]] <- yaml::read_yaml(file)
        message(paste("✅ 已載入模組內容：", module_name))
        loaded_count <- loaded_count + 1
      }, error = function(e) {
        warning(paste("載入模組", module_name, ".yaml 失敗：", e$message))
      })
    }

    message(paste("✅ 已載入", loaded_count, "個模組語言內容：", language))
  } else {
    # 嘗試載入舊版 modules.yaml 作為備用
    modules_file <- file.path(language_path, "modules.yaml")
    if (file.exists(modules_file)) {
      tryCatch({
        lang_content$content$modules <- yaml::read_yaml(modules_file)
        message(paste("✅ 已載入模組語言內容（舊版）：", language))
      }, error = function(e) {
        warning(paste("載入 modules.yaml 失敗：", e$message))
      })
    }
  }

  # 載入其他語言檔案（如果有，排除 general 和 module 資料夾中的檔案）
  yaml_files <- list.files(language_path, pattern = "\\.yaml$", full.names = TRUE)
  for (file in yaml_files) {
    file_name <- tools::file_path_sans_ext(basename(file))
    # 跳過已載入的 common、modules 檔案，以及位於子資料夾中的檔案
    if (!file_name %in% c("common", "modules") &&
        !grepl("general|module", dirname(file))) {
      tryCatch({
        lang_content$content[[file_name]] <- yaml::read_yaml(file)
        message(paste("✅ 已載入額外語言檔案：", file_name))
      }, error = function(e) {
        warning(paste("載入", file_name, ".yaml 失敗：", e$message))
      })
    }
  }

  # 設定類別
  class(lang_content) <- c("language_content", class(lang_content))

  # 同步到全局狀態（向後兼容）
  # 1. 同步到 global_language_state
  if (exists("global_language_state", envir = .GlobalEnv)) {
    tryCatch({
      gls <- get("global_language_state", envir = .GlobalEnv)
      shiny::isolate({
        gls$current_language <- language
        gls$language_content <- lang_content
        gls$last_update_time <- Sys.time()
      })
      cat("✅ [向后兼容] 已同步到 global_language_state\n")
    }, error = function(e) {
      cat("⚠️ [向后兼容] 同步到 global_language_state 失败:", e$message, "\n")
    })
  }

  # 2. 同步到 global_lang_content
  # NOTE: global_lang_content is a reactive() expression, NOT a reactiveVal()
  # It derives its value from global_language_state automatically
  # So we don't need to (and can't) set it directly
  # Modules should use global_language_state or get_module_language_content() instead
  cat("✅ [向后兼容] global_lang_content 由 global_language_state 自动派生，无需手动同步\n")

  return(lang_content)
}

# ==========================================
# 取得語言文字
# ==========================================
get_text <- function(lang_content, path, default = NULL) {
  # path 格式: "modules.upload.title" 或 "common.buttons.submit"

  if (is.null(lang_content) || is.null(lang_content$content)) {
    return(default %||% path)
  }

  # 分割路徑
  keys <- strsplit(path, "\\.")[[1]]

  # 遞迴取得值
  current <- lang_content$content
  for (key in keys) {
    if (is.list(current) && key %in% names(current)) {
      current <- current[[key]]
    } else {
      # 找不到時返回預設值
      return(default %||% path)
    }
  }

  return(current)
}

# ==========================================
# 批次取得語言文字
# ==========================================
get_texts <- function(lang_content, paths, defaults = NULL) {
  # paths: 字串向量，包含多個路徑
  # defaults: 與 paths 長度相同的預設值向量（可選）

  results <- list()

  for (i in seq_along(paths)) {
    path <- paths[i]
    default <- if (!is.null(defaults) && length(defaults) >= i) defaults[i] else NULL
    results[[path]] <- get_text(lang_content, path, default)
  }

  return(results)
}

# ==========================================
# 取得模組語言內容
# ==========================================
get_module_texts <- function(lang_content, module_id) {
  cat("\n🔍 [get_module_texts in language_manager.R] ===\n")
  cat("   Module ID:", module_id, "\n")
  cat("   lang_content is NULL:", is.null(lang_content), "\n")

  # 取得特定模組的所有語言內容
  # 並附加 metadata 包含語言代碼，供模組使用
  # 🆕 同時包含 common 內容，讓模組可以訪問 system.messages 等全局內容

  if (is.null(lang_content)) {
    cat("   ❌ lang_content is NULL, returning default\n")
    # Return empty structure with metadata showing fallback language
    return(list(
      metadata = list(language = "zh_TW")
    ))
  }

  # Extract language code from lang_content
  language_code <- if (!is.null(lang_content$language)) {
    lang_content$language
  } else if (!is.null(lang_content$metadata$language)) {
    lang_content$metadata$language
  } else {
    "zh_TW"
  }

  cat("   Language code:", language_code, "\n")

  # ✅ FIX: Call get_module_language_content to read fresh from YAML
  cat("   Calling get_module_language_content(", module_id, ",", language_code, ")\n")
  module_content <- get_module_language_content(module_id, language_code)

  cat("   get_module_language_content returned NULL:", is.null(module_content), "\n")
  if (!is.null(module_content)) {
    cat("   Keys returned:", paste(head(names(module_content), 5), collapse = ", "), "\n")
  }

  # 🆕 Merge with common content (common content at root level for easy access)
  # This allows modules to access system.messages.* paths
  if (!is.null(lang_content$content$common)) {
    cat("   Merging with common content\n")
    # Merge common content with module content
    # Module-specific content takes precedence over common content
    for (key in names(lang_content$content$common)) {
      if (!(key %in% names(module_content))) {
        module_content[[key]] <- lang_content$content$common[[key]]
      }
    }
  }

  # Add metadata with current language code
  module_content$metadata <- list(
    language = if (!is.null(lang_content$language)) lang_content$language else "zh_TW"
  )

  return(module_content)
}

# ==========================================
# 格式化文字（支援變數替換）
# ==========================================
format_text <- function(text, ...) {
  # 支援 {variable} 格式的變數替換
  # 例如: format_text("共 {count} 筆資料", count = 100)

  args <- list(...)

  if (length(args) == 0) {
    return(text)
  }

  # 替換所有 {variable} 格式的變數
  for (name in names(args)) {
    pattern <- paste0("\\{", name, "\\}")
    text <- gsub(pattern, as.character(args[[name]]), text)
  }

  return(text)
}

# ==========================================
# 從配置獲取語言信息
# ==========================================
get_language_info_from_config <- function(config, language_code) {
  # 語言代碼到目錄名稱的映射
  code_to_dir <- list(
    "zh_TW" = "chinese",
    "chinese" = "chinese",
    "zh" = "chinese",
    "en_US" = "english",
    "english" = "english",
    "en" = "english",
    "ja_JP" = "japanese",
    "japanese" = "japanese",
    "ja" = "japanese"
  )
  
  # 檢查映射
  if (language_code %in% names(code_to_dir)) {
    dir_name <- code_to_dir[[language_code]]
    return(list(code = language_code, dir = dir_name, name = dir_name))
  }

  return(NULL)
}


# ==========================================
# 建立語言切換器 UI
# ==========================================
language_selector_ui <- function(id, lang_content = NULL, config = NULL) {
  ns <- NS(id)

  # 載入語言配置（如果沒有提供）
  if (is.null(config)) {
    config <- load_language_config()
  }

  # 取得可用語言清單
  available_languages <- list()
  for (lang in config$supported_languages) {
    if (lang$enabled) {
      label <- lang$display_name
      if (config$ui$show_flags && !is.null(lang$flag)) {
        label <- paste(lang$flag, label)
      }
      available_languages[[lang$code]] <- label
    }
  }

  # 取得當前語言
  current_language <- if (!is.null(lang_content)) {
    lang_content$language
  } else {
    config$default_language
  }

  # 檢查是否啟用語言選擇
  enable_selection <- config$ui$show_language_selector %||% TRUE

  if (enable_selection && length(available_languages) > 1) {
    selectInput(
      ns("language"),
      label = NULL,
      choices = available_languages,
      selected = current_language,
      width = "150px"
    )
  } else {
    # 不顯示選擇器
    NULL
  }
}

# ==========================================
# 語言切換器 Server
# ==========================================
language_selector_server <- function(id, config = NULL) {
  moduleServer(id, function(input, output, session) {
    # 載入語言配置（如果沒有提供）
    if (is.null(config)) {
      config <- load_language_config()
    }

    # 儲存當前語言內容
    lang_content <- reactiveVal(NULL)

    # 初始載入
    observe({
      content <- load_language_content(config)
      lang_content(content)
    })

    # 語言切換
    observeEvent(input$language, {
      req(input$language)

      # 重新載入語言內容
      content <- load_language_content(config, input$language)
      lang_content(content)

      # 獲取語言信息
      lang_info <- get_language_info_from_config(config, input$language)
      lang_name <- if (!is.null(lang_info)) lang_info$name else input$language

      # 發送語言變更通知
      showNotification(
        paste("語言已切換至：", lang_name),
        type = "message",
        duration = 2
      )
    })

    return(lang_content)
  })
}

# ==========================================
# 輔助函數：檢查語言內容完整性
# ==========================================
validate_language_content <- function(lang_content, required_paths = NULL) {
  # 檢查語言內容是否包含必要的路徑

  if (is.null(lang_content) || is.null(lang_content$content)) {
    return(FALSE)
  }

  if (is.null(required_paths)) {
    # 預設檢查基本路徑
    required_paths <- c(
      "common.app.title",
      "common.buttons.submit",
      "modules.upload.title"
    )
  }

  missing_paths <- character(0)

  for (path in required_paths) {
    if (is.null(get_text(lang_content, path))) {
      missing_paths <- c(missing_paths, path)
    }
  }

  if (length(missing_paths) > 0) {
    warning(paste("缺少以下語言內容：", paste(missing_paths, collapse = ", ")))
    return(FALSE)
  }

  return(TRUE)
}

# ==========================================
# Print method for language_content
# ==========================================
print.language_content <- function(x, ...) {
  cat("========== 語言內容資訊 ==========\n")
  cat("當前語言:", x$language, "\n")
  cat("已載入內容:\n")

  if (!is.null(x$content)) {
    for (name in names(x$content)) {
      item_count <- if (is.list(x$content[[name]])) {
        length(x$content[[name]])
      } else {
        1
      }
      cat("  -", name, "(", item_count, "項)\n")
    }
  }

  cat("==================================\n")
}

# ==========================================
# 語言化的 Markdown 載入函數
# ==========================================
includeLanguageMarkdown <- function(filename, lang_content = NULL, fallback_path = NULL) {
  # 根據當前語言載入對應的 markdown 檔案
  # filename: 檔案名稱 (不含路徑，如 "notification.md")
  # lang_content: 當前語言內容物件
  # fallback_path: 備用路徑 (如果語言化檔案不存在)

  # 決定語言目錄 - 使用 YAML 配置
  language_code <- "zh_TW"  # 預設語言代碼
  if (!is.null(lang_content) && !is.null(lang_content$language)) {
    language_code <- lang_content$language
  }

  # 動態從 YAML 配置獲取目錄名稱
  language_dir <- if (exists("get_language_dir_from_config")) {
    get_language_dir_from_config(language_code)
  } else {
    # 備用方案
    if (language_code == "en_US") "english"
    else if (language_code == "ja_JP") "japanese"
    else "chinese"
  }

  # 建立語言化檔案路徑
  language_path <- file.path("database", "content", language_dir, "markdown", filename)

  # 檢查語言化檔案是否存在
  if (file.exists(language_path)) {
    return(includeMarkdown(language_path))
  }

  # 如果語言化檔案不存在，嘗試使用備用路徑
  if (!is.null(fallback_path) && file.exists(fallback_path)) {
    warning(paste("語言化檔案不存在:", language_path, "使用備用路徑:", fallback_path))
    return(includeMarkdown(fallback_path))
  }

  # 如果都不存在，嘗試使用原始 md/ 目錄
  original_path <- file.path("md", filename)
  if (file.exists(original_path)) {
    warning(paste("語言化檔案不存在:", language_path, "使用原始檔案:", original_path))
    return(includeMarkdown(original_path))
  }

  # 都找不到檔案時返回錯誤訊息
  error_msg <- paste("找不到檔案:", filename, "已搜尋路徑:", language_path, original_path)
  warning(error_msg)
  return(div(
    class = "alert alert-warning",
    style = "margin: 10px 0;",
    tags$strong("檔案載入失敗: "), filename
  ))
}

# ==========================================
# 反應式語言化 Markdown 載入函數
# ==========================================
reactiveLanguageMarkdown <- function(filename, global_lang_content = NULL, fallback_path = NULL) {
  # 用於 Server 端的反應式 Markdown 載入
  # global_lang_content: 反應式的語言內容

  if (is.null(global_lang_content)) {
    # 非反應式模式
    return(includeLanguageMarkdown(filename, fallback_path = fallback_path))
  }

  # 反應式模式
  renderUI({
    current_lang <- global_lang_content()
    includeLanguageMarkdown(filename, current_lang, fallback_path)
  })
}

# ==========================================
# NULL 合併運算子（如果不存在）
# ==========================================
# Fixed: Use isTRUE() to safely handle vector comparisons
if (!exists("%||%")) {
  `%||%` <- function(x, y) {
    if (is.null(x) || isTRUE(length(x) == 0)) y else x
  }
}

# ============================================================================
# InsightForge Hint System - 增強版提示系統功能
# 支援動態語言切換
# ============================================================================

library(shiny)
library(bs4Dash)

# ── 載入提示資料 (Reactive版本) ────────────────────────────────────────────
load_hints_reactive <- function(language) {
  reactive({
    cat("🔄 [Hints] Reactive載入, 語言:", language(), "\n")
    load_hints(language = language())
  })
}

# ── 載入提示資料 (原始版本) ────────────────────────────────────────────────
load_hints <- function(hint_file = NULL, language = "zh_TW") {
  # 根據語言決定檔案路徑
  if (is.null(hint_file)) {
    # 根據語言代碼決定目錄
    lang_dir <- if (language == "en_US" || language == "english") {
      "english"
    } else if (language == "zh_TW" || language == "chinese") {
      "chinese"
    } else {
      "chinese"  # 預設使用中文
    }
    hint_file <- file.path("database/content", lang_dir, "general/hint.csv")
  }

  if (file.exists(hint_file)) {
    hints <- read.csv(hint_file, stringsAsFactors = FALSE, encoding = "UTF-8")
    cat("✅ [Hints] 載入成功:", nrow(hints), "條 (", language, ")\n")
    return(hints)
  } else {
    warning("Hint file not found: ", hint_file)
    return(data.frame(
      concept_name = character(),
      var_id = character(),
      description = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ── 取得特定提示 ────────────────────────────────────────────────────────────
get_hint <- function(var_id, hints_df = NULL, language = "zh_TW") {
  if (is.null(hints_df)) {
    hints_df <- load_hints(language = language)
  }

  # 如果 hints_df 是 reactive，取其值
  if (is.reactive(hints_df)) {
    hints_df <- hints_df()
  }

  hint_row <- hints_df[hints_df$var_id == var_id, ]

  if (nrow(hint_row) > 0) {
    return(hint_row$description[1])
  } else {
    return(NULL)
  }
}

# ── 為 UI 元素添加提示 (支援 reactive) ──────────────────────────────────────
add_hint <- function(ui_element, var_id, hints_df = NULL, enable_hints = TRUE, language = "zh_TW") {
  if (!enable_hints) {
    return(ui_element)
  }

  # 如果 hints_df 是 reactive，取其值
  if (is.reactive(hints_df)) {
    hints_df <- hints_df()
  }

  hint_text <- get_hint(var_id, hints_df, language = language)

  if (!is.null(hint_text)) {
    # 使用 bs4Dash 的 tooltip 功能
    return(
      bs4Dash::tooltip(
        tag = ui_element,
        title = hint_text,
        placement = "top"
      )
    )
  } else {
    return(ui_element)
  }
}

# 其他函數保持不變...
# (複製原有的其他函數)


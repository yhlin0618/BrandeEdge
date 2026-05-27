
# ==============================================================================
# 模組語言切換修復補丁
# ==============================================================================
# 在模組的 Server 函數開頭添加以下代碼：

# ── 語言響應式變數 ──────────────────────────────────────────────────────────
current_language <- reactive({
  # 優先從 lang_texts 獲取語言
  if (!is.null(lang_texts)) {
    texts <- if (is.function(lang_texts)) lang_texts() else lang_texts
    if (!is.null(texts$language)) {
      return(texts$language)
    }
  }

  # 從全局環境獲取
  if (exists("global_lang_content") && is.reactive(global_lang_content)) {
    content <- global_lang_content()
    if (!is.null(content$language)) {
      return(content$language)
    }
  }

  # 預設值
  return("zh_TW")
})

# ── Reactive Hints ──────────────────────────────────────────────────────────
hints_reactive <- reactive({
  lang <- current_language()
  cat("📚 [Module] 載入 Hints, 語言:", lang, "\n")
  if (exists("load_hints") && is.function(load_hints)) {
    load_hints(language = lang)
  } else {
    NULL
  }
})

# ── Reactive Prompts ────────────────────────────────────────────────────────
prompts_reactive <- reactive({
  lang <- current_language()
  cat("📝 [Module] 載入 Prompts, 語言:", lang, "\n")
  if (exists("load_prompts") && is.function(load_prompts)) {
    load_prompts(language = lang)
  } else {
    NULL
  }
})

# ── Markdown 路徑 Reactive ──────────────────────────────────────────────────
markdown_path_reactive <- reactive({
  lang <- current_language()
  lang_dir <- if (lang == "en_US") "english" else "chinese"
  file.path("database/content", lang_dir, "markdown")
})

# ==============================================================================
# UI 函數修改範例
# ==============================================================================
# 在 renderUI 中使用 reactive hints:
output$dynamic_ui <- renderUI({
  hints <- hints_reactive()  # 獲取當前語言的 hints

  # 使用 hints 創建 UI 元素
  tagList(
    add_hint(
      textInput(ns("input1"), "輸入框"),
      "input_hint_id",
      hints
    )
  )
})


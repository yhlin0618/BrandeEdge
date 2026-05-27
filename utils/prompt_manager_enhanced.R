
# ============================================================================
# InsightForge Prompt Management System - 增強版 GPT Prompt 集中管理
# 支援動態語言切換
# ============================================================================

library(stringr)
library(jsonlite)

# ── 載入 Prompt 資料 (Reactive版本) ────────────────────────────────────────
load_prompts_reactive <- function(language) {
  reactive({
    cat("🔄 [Prompts] Reactive載入, 語言:", language(), "\n")
    load_prompts(language = language())
  })
}

# ── 載入 Prompt 資料 (原始版本) ────────────────────────────────────────────
load_prompts <- function(prompt_file = NULL, language = "zh_TW") {
  # 根據語言決定檔案路徑
  if (is.null(prompt_file)) {
    # 根據語言代碼決定目錄
    lang_dir <- if (language == "en_US" || language == "english") {
      "english"
    } else if (language == "zh_TW" || language == "chinese") {
      "chinese"
    } else {
      "chinese"  # 預設使用中文
    }
    prompt_file <- file.path("database/content", lang_dir, "general/prompt.csv")
  }

  if (file.exists(prompt_file)) {
    prompts <- read.csv(prompt_file, stringsAsFactors = FALSE, encoding = "UTF-8")
    cat("✅ [Prompts] 載入成功:", nrow(prompts), "個 (", language, ")\n")
    return(prompts)
  } else {
    warning("Prompt file not found: ", prompt_file)
    return(data.frame(
      analysis_name = character(),
      var_id = character(),
      prompt = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ── 取得特定 Prompt (支援 reactive) ─────────────────────────────────────────
get_prompt <- function(var_id, prompts_df = NULL, language = "zh_TW") {
  if (is.null(prompts_df)) {
    prompts_df <- load_prompts(language = language)
  }

  # 如果 prompts_df 是 reactive，取其值
  if (is.reactive(prompts_df)) {
    prompts_df <- prompts_df()
  }

  prompt_row <- prompts_df[prompts_df$var_id == var_id, ]

  if (nrow(prompt_row) > 0) {
    return(prompt_row$prompt[1])
  } else {
    warning(paste("Prompt not found for var_id:", var_id))
    return(NULL)
  }
}

# 其他函數保持不變...
# (複製原有的其他函數)


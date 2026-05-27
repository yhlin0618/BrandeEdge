# ============================================================================
# InsightForge Prompt Management System - GPT Prompt 集中管理
# 基於 BrandEdge 架構優化
# ============================================================================

library(stringr)
library(jsonlite)
library(yaml)

# ── 語言代碼轉目錄名稱 (動態從 YAML 讀取) ───────────────────────────────────
get_language_dir <- function(language_code) {
  cat("🔍 [Language Detection] Input code:", language_code, "\n")

  tryCatch({
    lang_config <- yaml::read_yaml("config/languages.yaml")
    cat("📋 [Language Config] Loaded", length(lang_config$supported_languages), "languages\n")

    # Loop through all supported languages
    for (lang in lang_config$supported_languages) {
      cat("  - Checking:", lang$code, "→", lang$dir, "(enabled:", lang$enabled, ")\n")

      if (lang$code == language_code && lang$enabled) {
        cat("✅ [Language Match] Using directory:", lang$dir, "\n")
        return(lang$dir)
      }
    }

    # If no match found, use fallback
    cat("⚠️ [Language Fallback] No match for", language_code, "\n")
    fallback_code <- lang_config$fallback$fallback_language
    cat("   Fallback code:", fallback_code, "\n")

    for (lang in lang_config$supported_languages) {
      if (lang$code == fallback_code) {
        cat("✅ [Fallback Match] Using fallback directory:", lang$dir, "\n")
        return(lang$dir)
      }
    }

    # Last resort fallback
    cat("⚠️ [Last Resort] Returning 'chinese'\n")
    return("chinese")
  }, error = function(e) {
    cat("❌ [Language Error]:", e$message, "\n")
    warning("Failed to load language config: ", e$message)
    return("chinese")
  })
}

# ── 載入 Prompt 資料 (支援 Reactive) ────────────────────────────────────────
# Reactive 版本 - 用於動態語言切換
load_prompts_reactive <- function(language) {
  reactive({
    lang <- if (is.reactive(language)) language() else language
    cat("🔄 [Prompts Reactive] 載入語言:", lang, "\n")
    load_prompts(language = lang)
  })
}

# ── 載入 Prompt 資料 (基礎版本) ──────────────────────────────────────────────
load_prompts <- function(prompt_file = NULL, language = "zh_TW", app_name = "insightforge") {
  cat("\n📂 [load_prompts] Called with language:", language, "app_name:", app_name, "\n")

  # 根據語言決定檔案路徑
  if (is.null(prompt_file)) {
    # 動態從 YAML 配置獲取目錄名稱
    lang_dir <- get_language_dir(language)
    prompt_file <- file.path("database/content", lang_dir, "general", app_name, "prompt.csv")
    cat("📁 [Prompt Path] Constructed:", prompt_file, "\n")
  } else {
    cat("📁 [Prompt Path] Using provided path:", prompt_file, "\n")
  }

  if (file.exists(prompt_file)) {
    prompts <- read.csv(prompt_file, stringsAsFactors = FALSE, encoding = "UTF-8")
    cat("✅ [Prompts] 載入", nrow(prompts), "個（", language, "）from", prompt_file, "\n")
    # Show first prompt for verification
    if (nrow(prompts) > 0) {
      cat("   First prompt sample:", substr(prompts$prompt[1], 1, 50), "...\n")
    }
    return(prompts)
  } else {
    cat("❌ [Prompts] File not found:", prompt_file, "\n")
    warning("Prompt file not found: ", prompt_file)
    return(data.frame(
      analysis_name = character(),
      var_id = character(),
      prompt = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ── 取得特定 Prompt ─────────────────────────────────────────────────────────
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

# ── 解析 Prompt 為 System 和 User 部分 ──────────────────────────────────────
parse_prompt <- function(prompt_text, language = "zh_TW") {
  if (is.null(prompt_text)) {
    return(list(system = "", user = ""))
  }
  
  # 分離 system 和 user 部分
  parts <- strsplit(prompt_text, "\nuser: ")[[1]]
  
  if (length(parts) >= 2) {
    system_part <- gsub("^system: ", "", parts[1])
    user_part <- parts[2]
  } else {
    # 如果沒有明確分離，整個作為 user prompt
    # 根據語言設定預設 system prompt
    if (language == "en_US" || language == "english") {
      system_part <- "You are a professional marketing data analyst. Please respond in English."
    } else {
      system_part <- "你是一位行銷專業的數據分析師，請用繁體中文回答。"
    }
    user_part <- prompt_text
  }
  
  return(list(
    system = trimws(system_part),
    user = trimws(user_part)
  ))
}

# ── 替換 Prompt 中的變數 ────────────────────────────────────────────────────
replace_prompt_variables <- function(prompt_text, variables = list()) {
  if (is.null(prompt_text)) {
    return(prompt_text)
  }
  
  # 替換所有 {variable_name} 格式的變數
  for (var_name in names(variables)) {
    pattern <- paste0("\\{", var_name, "\\}")
    
    # 處理不同類型的變數
    replacement <- variables[[var_name]]
    
    if (is.data.frame(replacement) || is.list(replacement)) {
      # 如果是資料框或列表，轉換為 JSON
      replacement <- jsonlite::toJSON(replacement, dataframe = "rows", auto_unbox = TRUE)
    } else if (is.numeric(replacement)) {
      # 數值保持原樣
      replacement <- as.character(replacement)
    } else {
      # 其他類型轉為字符串
      replacement <- as.character(replacement)
    }
    
    prompt_text <- gsub(pattern, replacement, prompt_text, fixed = FALSE)
  }
  
  return(prompt_text)
}

# ── 準備 GPT API 請求消息 ───────────────────────────────────────────────────
prepare_gpt_messages <- function(var_id, variables = list(), prompts_df = NULL, language = "zh_TW") {
  # 如果 prompts_df 是 reactive，取其值
  if (is.reactive(prompts_df)) {
    prompts_df <- prompts_df()
  }

  # 取得原始 prompt
  raw_prompt <- get_prompt(var_id, prompts_df, language = language)

  if (is.null(raw_prompt)) {
    stop(paste("Cannot find prompt for var_id:", var_id))
  }

  # 解析 system 和 user 部分
  parsed <- parse_prompt(raw_prompt, language = language)

  # 替換變數
  system_content <- replace_prompt_variables(parsed$system, variables)
  user_content <- replace_prompt_variables(parsed$user, variables)

  # 建立消息列表
  messages <- list(
    list(role = "system", content = system_content),
    list(role = "user", content = user_content)
  )

  return(messages)
}

# ── 快速建立單一 Prompt ─────────────────────────────────────────────────────
quick_prompt <- function(var_id, ..., prompts_df = NULL, language = "zh_TW") {
  variables <- list(...)
  prepare_gpt_messages(var_id, variables, prompts_df, language = language)
}

# ── 批次準備多個 Prompts ────────────────────────────────────────────────────
prepare_batch_prompts <- function(var_ids, variables_list, prompts_df = NULL, language = "zh_TW") {
  if (length(var_ids) != length(variables_list)) {
    stop("var_ids and variables_list must have the same length")
  }

  Map(function(id, vars) {
    prepare_gpt_messages(id, vars, prompts_df, language = language)
  }, var_ids, variables_list)
}

# ── 驗證 Prompt 變數 ────────────────────────────────────────────────────────
validate_prompt_variables <- function(var_id, prompts_df = NULL, language = "zh_TW") {
  raw_prompt <- get_prompt(var_id, prompts_df, language = language)
  
  if (is.null(raw_prompt)) {
    return(list(valid = FALSE, message = "Prompt not found"))
  }
  
  # 提取所有 {variable} 格式的變數
  variables <- stringr::str_extract_all(raw_prompt, "\\{[^}]+\\}")[[1]]
  variables <- gsub("[{}]", "", variables)
  
  return(list(
    valid = TRUE,
    variables = variables,
    count = length(variables)
  ))
}

# ── 列出所有可用的 Prompts ──────────────────────────────────────────────────
list_available_prompts <- function(prompts_df = NULL, language = "zh_TW") {
  if (is.null(prompts_df)) {
    prompts_df <- load_prompts(language = language)
  }

  prompts_df[, c("analysis_name", "var_id")]
}

# ── 創建自定義 Prompt ───────────────────────────────────────────────────────
create_custom_prompt <- function(system_content, user_content) {
  list(
    list(role = "system", content = system_content),
    list(role = "user", content = user_content)
  )
}

# ── 儲存新 Prompt 到檔案 ────────────────────────────────────────────────────
save_new_prompt <- function(analysis_name, var_id, prompt_text,
                           prompt_file = NULL, language = "zh_TW") {
  # 決定檔案路徑
  if (is.null(prompt_file)) {
    lang_dir <- if (language == "en_US" || language == "english") {
      "english"
    } else {
      "chinese"
    }
    prompt_file <- file.path("database/content", lang_dir, "general/prompt.csv")
  }

  # 載入現有 prompts
  existing_prompts <- load_prompts(prompt_file, language = language)
  
  # 檢查是否已存在
  if (var_id %in% existing_prompts$var_id) {
    warning(paste("Prompt with var_id", var_id, "already exists. Updating..."))
    existing_prompts <- existing_prompts[existing_prompts$var_id != var_id, ]
  }
  
  # 添加新 prompt
  new_row <- data.frame(
    analysis_name = analysis_name,
    var_id = var_id,
    prompt = prompt_text,
    stringsAsFactors = FALSE
  )
  
  updated_prompts <- rbind(existing_prompts, new_row)
  
  # 儲存到檔案
  write.csv(updated_prompts, prompt_file, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat("✅ Prompt saved successfully:", var_id, "\n")
  return(TRUE)
}

# ── 執行 GPT API 請求 ──────────────────────────────────────────────────────
execute_gpt_request <- function(var_id,
                                variables = list(),
                                chat_api_function = NULL,
                                model = "gpt-5-nano",
                                prompts_df = NULL,
                                language = "zh_TW",
                                ...) {

  # 準備消息
  messages <- prepare_gpt_messages(var_id, variables, prompts_df, language = language)

  # 如果沒有提供 chat_api 函數，返回準備好的消息
  if (is.null(chat_api_function)) {
    warning("No chat_api_function provided, returning prepared messages only")
    return(messages)
  }

  # 執行 API 請求
  # 注意：chat_api 函數已經內建 max_tokens 和 temperature 參數
  response <- tryCatch(
    chat_api_function(
      messages = messages,
      model = model,
      ...
    ),
    error = function(e) {
      warning(paste("GPT API error:", e$message))
      return(NULL)
    }
  )

  return(response)
}

# ── 格式化 Prompt 顯示 ──────────────────────────────────────────────────────
format_prompt_display <- function(var_id, variables = list(), prompts_df = NULL, language = "zh_TW") {
  messages <- prepare_gpt_messages(var_id, variables, prompts_df, language = language)

  formatted <- paste0(
    "=== SYSTEM ===\n",
    messages[[1]]$content,
    "\n\n=== USER ===\n",
    messages[[2]]$content
  )

  return(formatted)
}

# ── 測試 Prompt 系統 ────────────────────────────────────────────────────────
test_prompt_system <- function() {
  cat("🧪 測試 Prompt Management System\n")
  cat(paste(rep("=", 50), collapse = ""), "\n")
  
  # 載入 prompts
  prompts <- load_prompts()
  cat("📋 可用的 prompts:\n")
  print(list_available_prompts(prompts))
  
  # 測試變數替換
  if (nrow(prompts) > 0) {
    test_id <- prompts$var_id[1]
    cat("\n🔍 測試 prompt:", test_id, "\n")
    
    # 驗證變數
    validation <- validate_prompt_variables(test_id, prompts)
    cat("需要的變數:", paste(validation$variables, collapse = ", "), "\n")
    
    # 創建測試變數
    test_vars <- list()
    for (v in validation$variables) {
      test_vars[[v]] <- paste("測試", v)
    }
    
    # 準備消息
    messages <- prepare_gpt_messages(test_id, test_vars, prompts)
    cat("\n生成的消息:\n")
    cat(format_prompt_display(test_id, test_vars, prompts))
  }
  
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("✅ Prompt 系統測試完成\n")
}

# ── 匯出函數列表 ────────────────────────────────────────────────────────────
prompt_manager_exports <- list(
  load_prompts = load_prompts,
  load_prompts_reactive = load_prompts_reactive,  # 新增 reactive 版本
  get_prompt = get_prompt,
  parse_prompt = parse_prompt,
  replace_prompt_variables = replace_prompt_variables,
  prepare_gpt_messages = prepare_gpt_messages,
  execute_gpt_request = execute_gpt_request,  # 新增 GPT API 執行函數
  quick_prompt = quick_prompt,
  prepare_batch_prompts = prepare_batch_prompts,
  validate_prompt_variables = validate_prompt_variables,
  list_available_prompts = list_available_prompts,
  create_custom_prompt = create_custom_prompt,
  save_new_prompt = save_new_prompt,
  format_prompt_display = format_prompt_display,
  test_prompt_system = test_prompt_system
)
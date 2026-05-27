################################################################################
# OpenAI 工具函數 - Global Scripts 轉接器
#
# 更新日期: 2025-01-07
# 說明: 此檔案從 global_scripts/08_ai/ 載入 OpenAI 工具函數
#       提供 Rate Limiting、指數退避、平行處理等功能
#
# 功能來源:
#   - fn_chat_api.R: 基本 Chat API 呼叫
#   - fn_rate_comments.R: 評論評分（含 rate limiting）
#   - fn_process_property_ratings.R: 平行評分處理
################################################################################

# 取得 global_scripts 路徑
get_global_scripts_path <- function() {
  # 嘗試多種可能的路徑
  possible_paths <- c(
    "scripts/global_scripts/08_ai",
    "../scripts/global_scripts/08_ai",
    "../../scripts/global_scripts/08_ai"
  )

  for (path in possible_paths) {
    if (dir.exists(path)) {
      return(path)
    }
  }

  # 如果都找不到，使用絕對路徑
  return(NULL)
}

# 載入 global_scripts 的 AI 工具函數
load_ai_utils <- function() {
  gs_path <- get_global_scripts_path()

  if (is.null(gs_path)) {
    message("⚠️ 未找到 global_scripts/08_ai，使用內建版本")
    return(FALSE)
  }

  chat_api_path <- file.path(gs_path, "fn_chat_api.R")
  if (!file.exists(chat_api_path)) {
    message("⚠️ 找不到 ", chat_api_path, "，使用內建版本")
    return(FALSE)
  }

  source(chat_api_path, local = FALSE)

  optional_ai_helpers <- c(
    "fn_rate_comments.R",
    "fn_process_property_ratings.R"
  )

  for (helper_file in optional_ai_helpers) {
    helper_path <- file.path(gs_path, helper_file)
    if (file.exists(helper_path)) {
      source(helper_path, local = FALSE)
    } else {
      message("ℹ️ 未包含可選 AI helper: ", helper_path)
    }
  }

  message("✅ 已從 global_scripts/08_ai 載入 OpenAI 工具函數")
  return(TRUE)
}

# ============================================================================
# 向後相容的函數定義
# ============================================================================

#' 解析 OpenAI Rate Limit Headers
#'
#' @param resp httr2 回應物件
#' @return 等待秒數，或 NA（使用預設退避）
openai_rate_limit_handler <- function(resp) {
  retry_after <- httr2::resp_header(resp, "Retry-After")
  if (!is.na(retry_after)) {
    return(as.numeric(retry_after))
  }
  reset_unix <- httr2::resp_header(resp, "x-ratelimit-reset-requests")
  if (!is.na(reset_unix)) {
    return(as.numeric(reset_unix) - unclass(Sys.time()))
  }
  NA
}

#' 測試 OpenAI API 連接
#'
#' @param api_key OpenAI API 金鑰
#' @return TRUE 如果連接成功，FALSE 否則
test_openai_connection <- function(api_key = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(api_key)) {
    message("❌ OPENAI_API_KEY 未設定")
    return(FALSE)
  }

  test_resp <- try(
    httr2::request("https://api.openai.com/v1/models") |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_timeout(10) |>
      httr2::req_perform(),
    silent = TRUE
  )

  if (inherits(test_resp, "try-error")) {
    message("❌ OpenAI API 連接失敗")
    return(FALSE)
  }

  if (httr2::resp_status(test_resp) == 200) {
    message("✅ OpenAI API 連接成功")
    return(TRUE)
  }

  message(sprintf("❌ OpenAI API 回傳狀態碼: %d", httr2::resp_status(test_resp)))
  return(FALSE)
}

# 嘗試載入 global_scripts
# 如果成功，chat_api 會從 global_scripts 載入（正確支援 GPT-5 Responses API）
# 如果失敗，定義本地版本（使用 gpt-4o-mini，因為它與 Chat Completions API 相容）
if (!load_ai_utils()) {
  # 內建 chat_api（向後相容）
  # ⚠️ 注意：此版本使用 Chat Completions API，不支援 GPT-5 系列模型
  # GPT-5 需要 Responses API，請確保 global_scripts 已正確設定
  chat_api <- function(messages,
                       model             = "gpt-4o-mini",  # Changed from gpt-5-nano - Chat Completions compatible
                       api_key           = Sys.getenv("OPENAI_API_KEY"),
                       api_url           = "https://api.openai.com/v1/chat/completions",
                       timeout_sec       = 60,
                       temperature       = 0.3,
                       max_tokens        = 1024,
                       max_retries       = 6,
                       max_retry_seconds = 180) {

    # Warn if GPT-5 model is requested without global_scripts
    if (grepl("^gpt-5", model)) {
      warning("⚠️ GPT-5 models require global_scripts/08_ai/fn_chat_api.R for Responses API support. ",
              "Falling back to gpt-4o-mini. Please ensure global_scripts is properly configured.")
      model <- "gpt-4o-mini"
    }

    if (!nzchar(api_key)) {
      stop("🔑 OPENAI_API_KEY 未設定")
    }

    body <- list(
      model       = model,
      messages    = messages,
      temperature = temperature,
      max_tokens  = max_tokens
    )

    req <- httr2::request(api_url) |>
      httr2::req_auth_bearer_token(api_key) |>
      httr2::req_headers(`Content-Type` = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_retry(
        is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500:599),
        after        = openai_rate_limit_handler,
        backoff      = \(n) min(2^(n - 1), 30),
        max_tries    = max_retries,
        max_seconds  = max_retry_seconds
      ) |>
      httr2::req_timeout(timeout_sec)

    resp <- try(httr2::req_perform(req), silent = TRUE)

    if (inherits(resp, "try-error")) {
      error_msg <- as.character(resp)
      message(sprintf("⚠️ OpenAI API 連接錯誤: %s", error_msg))
      return(paste0("[ERROR:Connection_", gsub("\\s+", "_", substr(error_msg, 1, 50)), "]"))
    }

    if (httr2::resp_status(resp) != 200) {
      status <- httr2::resp_status(resp)
      message(sprintf("⚠️ OpenAI API HTTP 錯誤: %d", status))
      return(paste0("[ERROR:HTTP_", status, "]"))
    }

    content <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    if (!is.null(content$choices)) {
      response_text <- trimws(content$choices[[1]]$message$content)

      # 自動追蹤 Token 使用量 (如果 update_token_usage 存在)
      if (exists("update_token_usage", mode = "function", envir = .GlobalEnv)) {
        usage_info <- list(
          input_tokens = content$usage$prompt_tokens %||% 0,
          output_tokens = content$usage$completion_tokens %||% 0,
          total_tokens = content$usage$total_tokens %||% 0,
          model = model
        )

        tryCatch({
          update_token_usage <- get("update_token_usage", envir = .GlobalEnv)
          update_token_usage(usage_info)
          message(sprintf("📊 Token: +%d (in: %d, out: %d)",
                         usage_info$total_tokens,
                         usage_info$input_tokens,
                         usage_info$output_tokens))
        }, error = function(e) {
          message(sprintf("⚠️ Token 追蹤失敗: %s", e$message))
        })
      }

      return(response_text)
    }

    if (!is.null(content$error)) {
      message(sprintf("⚠️ OpenAI API 回傳錯誤: %s", content$error$message))
      return(paste0("[ERROR:API_", content$error$message, "]"))
    }

    "[ERROR:Unknown_response_format]"
  }
}

# 匯出函數供其他模組使用
openai_utils_exports <- list(
  chat_api = chat_api,
  test_openai_connection = test_openai_connection,
  openai_rate_limit_handler = openai_rate_limit_handler
)

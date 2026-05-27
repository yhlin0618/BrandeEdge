################################################################################
# OpenAI 工具函數 - Rate Limiting & Exponential Backoff 版本
#
# 更新日期: 2025-01-07
# 參考來源: MAMBA l4_enterprise/scripts/global_scripts/08_ai/fn_rate_comments.R
#
# 新增功能:
#   - 指數退避 (Exponential Backoff): 2, 4, 8, 16, 30 秒
#   - 解析 OpenAI Rate Limit Headers (Retry-After, x-ratelimit-reset-requests)
#   - 自動重試 429 (Rate Limit) 和 5xx 錯誤
#   - 最多重試 6 次，總計 180 秒
################################################################################

library(httr2)
library(jsonlite)

#' 解析 OpenAI Rate Limit Headers
#'
#' @param resp httr2 回應物件
#' @return 等待秒數，或 NA（使用預設退避）
openai_rate_limit_handler <- function(resp) {
  # 優先使用 Retry-After header
  retry_after <- httr2::resp_header(resp, "Retry-After")
  if (!is.null(retry_after) && !is.na(retry_after)) {
    return(as.numeric(retry_after))
  }

  # 備用：解析 x-ratelimit-reset-requests (Unix timestamp)
  reset_unix <- httr2::resp_header(resp, "x-ratelimit-reset-requests")
  if (!is.null(reset_unix) && !is.na(reset_unix)) {
    wait_time <- as.numeric(reset_unix) - unclass(Sys.time())
    if (wait_time > 0) return(wait_time)
  }

  NA  # 回傳 NA 使用預設指數退避
}

#' 呼叫 OpenAI Chat API（含 Rate Limiting 和指數退避）
#'
#' @param messages 訊息列表，包含 system 和 user 角色
#' @param model GPT 模型名稱
#' @param api_key OpenAI API 金鑰
#' @param api_url API 端點
#' @param timeout_sec 超時秒數
#' @param temperature 溫度參數 (0-2)
#' @param max_tokens 最大 token 數
#' @param max_retries 最大重試次數
#' @param max_retry_seconds 最大重試總時間（秒）
#' @return API 回應的內容文字，或錯誤訊息字串
chat_api <- function(messages,
                     model             = "gpt-5-nano",
                     api_key           = Sys.getenv("OPENAI_API_KEY"),
                     api_url           = "https://api.openai.com/v1/chat/completions",
                     timeout_sec       = 60,
                     temperature       = 0.3,
                     max_tokens        = 1024,
                     max_retries       = 6,
                     max_retry_seconds = 180) {

  if (!nzchar(api_key)) {
    stop("🔑 OPENAI_API_KEY 未設定")
  }

  # 準備請求 body
  body <- list(
    model       = model,
    messages    = messages,
    temperature = temperature,
    max_tokens  = max_tokens
  )

  # 建立請求（含 Rate Limiting 和指數退避）
  req <- httr2::request(api_url) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_retry(
      is_transient = \(resp) httr2::resp_status(resp) %in% c(429, 500:599),
      after        = openai_rate_limit_handler,
      backoff      = \(n) min(2^(n - 1), 30),  # 指數退避: 2, 4, 8, 16, 30 秒
      max_tries    = max_retries,
      max_seconds  = max_retry_seconds
    ) |>
    httr2::req_timeout(timeout_sec)

  # 執行請求（使用 try 捕捉連接錯誤）
  resp <- try(httr2::req_perform(req), silent = TRUE)

  # 處理連接錯誤
  if (inherits(resp, "try-error")) {
    error_msg <- as.character(resp)
    message(sprintf("⚠️ OpenAI API 連接錯誤: %s", error_msg))
    return(paste0("[ERROR:Connection_", gsub("\\s+", "_", substr(error_msg, 1, 50)), "]"))
  }

  # 處理 HTTP 錯誤
  if (httr2::resp_status(resp) != 200) {
    status <- httr2::resp_status(resp)
    message(sprintf("⚠️ OpenAI API HTTP 錯誤: %d", status))
    return(paste0("[ERROR:HTTP_", status, "]"))
  }

  # 解析回應
  content <- httr2::resp_body_json(resp, simplifyVector = FALSE)

  if (!is.null(content$choices)) {
    return(trimws(content$choices[[1]]$message$content))
  }

  if (!is.null(content$error)) {
    message(sprintf("⚠️ OpenAI API 回傳錯誤: %s", content$error$message))
    return(paste0("[ERROR:API_", content$error$message, "]"))
  }

  "[ERROR:Unknown_response_format]"
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

# 匯出函數供其他模組使用
openai_utils_exports <- list(
  chat_api = chat_api,
  test_openai_connection = test_openai_connection,
  openai_rate_limit_handler = openai_rate_limit_handler
)
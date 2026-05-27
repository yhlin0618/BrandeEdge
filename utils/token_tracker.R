# ============================================================================
# Token Usage and Cost Tracker
# ============================================================================
# 追蹤 OpenAI API 的 token 使用量和費用
# Version: 1.0
# Last Updated: 2026-01-24
# ============================================================================

# 定價表 (per 1M tokens, 2026-01)
# 來源: https://openai.com/api/pricing/
TOKEN_PRICING <- list(
  # GPT-5 系列
  "gpt-5-nano" = list(input = 0.05, output = 0.40),
  "gpt-5-mini" = list(input = 0.25, output = 2.00),
  "gpt-5" = list(input = 1.25, output = 10.00),
  "gpt-5-pro" = list(input = 15.00, output = 120.00),


  # GPT-4o 系列
  "gpt-4o-mini" = list(input = 0.15, output = 0.60),
  "gpt-4o" = list(input = 2.50, output = 10.00),

  # GPT-4.1 系列
  "gpt-4.1-nano" = list(input = 0.10, output = 0.40),
  "gpt-4.1-mini" = list(input = 0.40, output = 1.60),
  "gpt-4.1" = list(input = 2.00, output = 8.00),

  # O 系列 (Reasoning Models)
  "o4-mini" = list(input = 1.10, output = 4.40),
  "o3" = list(input = 2.00, output = 8.00),
  "o1" = list(input = 15.00, output = 60.00)
)

#' 計算 Token 費用
#' @param input_tokens 輸入 token 數量
#' @param output_tokens 輸出 token 數量
#' @param model 模型名稱
#' @return 費用 (USD)
calculate_token_cost <- function(input_tokens, output_tokens, model) {
  # 取得定價，找不到則使用 gpt-5-nano 預設值
  pricing <- TOKEN_PRICING[[model]]
  if (is.null(pricing)) {
    pricing <- TOKEN_PRICING[["gpt-5-nano"]]
  }

  input_cost <- (input_tokens / 1e6) * pricing$input
  output_cost <- (output_tokens / 1e6) * pricing$output

  return(input_cost + output_cost)
}

#' 格式化費用顯示
#' @param cost 費用 (USD)
#' @return 格式化字串
format_cost <- function(cost) {
  if (cost < 0.01) {
    sprintf("$%.4f", cost)
  } else if (cost < 1) {
    sprintf("$%.3f", cost)
  } else {
    sprintf("$%.2f", cost)
  }
}

#' 格式化 Token 數量顯示
#' @param tokens Token 數量
#' @return 格式化字串 (e.g., "1.2K", "15.3K")
format_tokens <- function(tokens) {
  if (tokens < 1000) {
    as.character(tokens)
  } else if (tokens < 1e6) {
    sprintf("%.1fK", tokens / 1000)
  } else {
    sprintf("%.2fM", tokens / 1e6)
  }
}

#' 建立初始 Token 狀態
#' @param model 初始模型名稱
#' @return Token 狀態 list
create_token_state <- function(model = "gpt-5-nano") {
  list(
    model = model,
    input_tokens = 0,
    output_tokens = 0,
    total_tokens = 0,
    total_cost = 0,
    call_count = 0
  )
}

#' 更新 Token 狀態
#' @param state 目前狀態
#' @param usage 從 chat_api 回傳的 usage 資訊
#' @return 更新後的狀態
update_token_state <- function(state, usage) {
  if (is.null(usage)) return(state)

  # 累加 token 數量
  state$input_tokens <- state$input_tokens + (usage$input_tokens %||% 0)
  state$output_tokens <- state$output_tokens + (usage$output_tokens %||% 0)
  state$total_tokens <- state$total_tokens + (usage$total_tokens %||% 0)
  state$call_count <- state$call_count + 1

  # 計算增量費用
  new_cost <- calculate_token_cost(
    usage$input_tokens %||% 0,
    usage$output_tokens %||% 0,
    usage$model %||% state$model
  )
  state$total_cost <- state$total_cost + new_cost

  # 更新模型 (如果有變更)
  if (!is.null(usage$model)) {
    state$model <- usage$model
  }

  return(state)
}

#' 取得模型定價資訊
#' @param model 模型名稱
#' @return 定價 list (input, output per 1M tokens)
get_model_pricing <- function(model) {
  pricing <- TOKEN_PRICING[[model]]
  if (is.null(pricing)) {
    pricing <- TOKEN_PRICING[["gpt-5-nano"]]
  }
  return(pricing)
}

#' 列出所有支援的模型
#' @return 模型名稱向量
list_supported_models <- function() {
  names(TOKEN_PRICING)
}

message("✅ Token Tracker 已載入 (支援 ", length(TOKEN_PRICING), " 個模型)")

#LOCK FILE
#
# fn_chat_api.R
#
# Following principles:
# - R21: One Function One File
# - R69: Function File Naming (fn_ prefix)
# - MP47: Functional Programming
# - MP81: Explicit Parameter Specification
# - MP123: AI Prompt Configuration Management
#
# Function to interact with OpenAI Chat API
# -----------------------------------------------------------------------------

#' Call OpenAI Chat API
#'
#' **Important - MP123: AI Prompt Configuration Management**
#' This function should be called with prompts from app_configs$ai_prompts,
#' which is pre-loaded during initialization. Do NOT re-read YAML files in components.
#'
#' Recommended usage pattern:
#' 1. Load prompt config: `prompt_config <- load_openai_prompt("section.prompt_name")`
#' 2. Prepare messages using prompt_config$system_prompt and user_prompt_template
#' 3. Call this function with model from prompt_config$model
#'
#' @param messages List. List of message objects with 'role' and 'content' fields.
#' @param api_key Character string. OpenAI API key (defaults to OPENAI_API_KEY env var).
#' @param model Character string. Model to use (defaults to "gpt-5-nano").
#'   **Tip**: Use prompt_config$model from centralized YAML configuration (MP123)
#' @param api_url Character string. API endpoint URL.
#' @param timeout_sec Numeric. Request timeout in seconds (defaults to 300 = 5 minutes).
#' @return Character string with the model's response.
#' @note Token usage is automatically tracked if `update_token_usage` function exists in global environment.
#' @examples
#' # Simple usage
#' sys <- list(role = "system", content = "You are a helpful assistant.")
#' usr <- list(role = "user", content = "Hello!")
#' response <- chat_api(list(sys, usr))
#'
#' # Recommended usage with centralized prompts (MP123)
#' prompt_config <- load_openai_prompt("position_analysis.strategy_quadrant_analysis")
#' user_content <- gsub("{strategy_data}", data_json, prompt_config$user_prompt_template)
#' sys <- list(role = "system", content = prompt_config$system_prompt)
#' usr <- list(role = "user", content = user_content)
#' response <- chat_api(list(sys, usr), model = prompt_config$model)
chat_api <- function(messages,
                       api_key = Sys.getenv("OPENAI_API_KEY"),
                       model = "gpt-5-nano",
                       api_url = "https://api.openai.com/v1/chat/completions",
                       timeout_sec = 300) {
  
  # Check for API key
  if (!nzchar(api_key)) {
    stop("🔑 OPENAI_API_KEY is missing. Please set it in environment variables or pass it directly.")
  }
  
  # Validate API key format
  if (!grepl("^sk-", api_key)) {
    warning("OpenAI API key format appears incorrect. Should start with 'sk-'")
  }
  
  # Check if httr2 is available
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for API calls. Please install it.")
  }
  
  # Check if jsonlite is available for JSON handling
  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package 'jsonlite' is required for JSON handling. Please install it.")
  }
  
  # Check if this is a GPT-5 model (uses Responses API)
  is_gpt5 <- grepl("^gpt-5", model)

  if (is_gpt5) {
    # GPT-5 uses Responses API with different format
    # Convert messages to input format
    # Combine system and user messages into single input
    system_msg <- ""
    user_msg <- ""

    for (msg in messages) {
      if (msg$role == "system") {
        system_msg <- paste0(system_msg, msg$content, "\n\n")
      } else if (msg$role == "user") {
        user_msg <- paste0(user_msg, msg$content, "\n\n")
      }
    }

    # Combine system and user messages
    full_input <- paste0(trimws(system_msg), "\n\n", trimws(user_msg))

    body <- list(
      model = model,
      input = trimws(full_input),
      reasoning = list(effort = "low"),  # low reasoning for faster response
      text = list(verbosity = "medium"), # medium verbosity
      max_output_tokens = 16000  # Increased from 4000 to support 10,000 word outputs
    )

    # Use Responses API endpoint
    api_url <- "https://api.openai.com/v1/responses"

  } else {
    # Non-GPT-5 models use Chat Completions API
    body <- list(
      model = model,
      messages = messages
    )

    # Add model-specific parameters
    if (grepl("^o3", model)) {
      body$max_completion_tokens <- 4000
    } else if (grepl("^o1", model)) {
      body$max_completion_tokens <- 2000
    } else {
      # Traditional models (gpt-4, gpt-3.5, etc.)
      body$temperature <- 0.3
      body$max_tokens <- 1024
    }
  }

  # Create and perform request
  req <- httr2::request(api_url) |>
    httr2::req_auth_bearer_token(api_key) |>
    httr2::req_headers(`Content-Type` = "application/json") |>
    httr2::req_body_json(body) |>
    httr2::req_timeout(timeout_sec)
  
  # Execute request
  resp <- httr2::req_perform(req)
  
  # Handle errors with detailed information
  if (httr2::resp_status(resp) >= 400) {
    err_text <- httr2::resp_body_string(resp)
    status_code <- httr2::resp_status(resp)
    
    # Try to parse JSON error for more details
    tryCatch({
      err_json <- jsonlite::fromJSON(err_text)
      if (!is.null(err_json$error$message)) {
        err_msg <- err_json$error$message
      } else {
        err_msg <- err_text
      }
    }, error = function(e) {
      err_msg <- err_text
    })
    
    stop(sprintf("Chat API error %s for model '%s':\n%s", status_code, model, err_msg))
  }
  
  # Extract response content based on API type
  content <- httr2::resp_body_json(resp)

  if (is_gpt5) {
    # Responses API format: content$output is an array of items
    # Need to find the "message" type item and extract its content
    if (!is.null(content$output) && length(content$output) > 0) {
      # Find the message item (type: "message")
      message_item <- NULL
      for (item in content$output) {
        if (!is.null(item$type) && item$type == "message") {
          message_item <- item
          break
        }
      }

      if (!is.null(message_item) && !is.null(message_item$content)) {
        # Extract text from content array
        text_items <- sapply(message_item$content, function(content_item) {
          if (!is.null(content_item$text)) {
            return(content_item$text)
          }
          return("")
        })
        response_text <- paste(text_items, collapse = "\n")
      } else {
        stop("No message content found in GPT-5 Responses API response")
      }
    } else {
      stop("Unexpected response format from GPT-5 Responses API: output array is empty or missing")
    }
  } else {
    # Chat Completions API format: content$choices[[1]]$message$content
    response_text <- content$choices[[1]]$message$content
  }

  # 自動追蹤 Token 使用量 (如果 update_token_usage 存在)
  if (exists("update_token_usage", mode = "function", envir = .GlobalEnv)) {
    if (is_gpt5) {
      # GPT-5 Responses API 格式
      usage_info <- list(
        input_tokens = content$usage$input_tokens %||% 0,
        output_tokens = content$usage$output_tokens %||% 0,
        total_tokens = (content$usage$input_tokens %||% 0) +
                       (content$usage$output_tokens %||% 0),
        model = model
      )
    } else {
      # Chat Completions API 格式
      usage_info <- list(
        input_tokens = content$usage$prompt_tokens %||% 0,
        output_tokens = content$usage$completion_tokens %||% 0,
        total_tokens = content$usage$total_tokens %||% 0,
        model = model
      )
    }

    tryCatch({
      update_token_usage <- get("update_token_usage", envir = .GlobalEnv)
      update_token_usage(usage_info)
      message(sprintf("📊 Token: +%d (in: %d, out: %d)",
                     usage_info$total_tokens,
                     usage_info$input_tokens,
                     usage_info$output_tokens))
    }, error = function(e) {
      # 追蹤失敗不影響主流程
      message(sprintf("⚠️ Token 追蹤失敗: %s", e$message))
    })
  }

  return(trimws(response_text))
}
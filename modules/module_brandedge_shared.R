# BrandEdge Shared Functions
# 共用語言系統函數和API函數
# Version: 1.0
# Last Updated: 2025-10-06

library(httr2)
library(jsonlite)
library(stringr)
library(markdown)

# 載入 Prompt 管理系統
if (!exists("prompts_df")) {
  source("utils/prompt_manager.R")
  prompts_df <- load_prompts()
}

# Source global chat_api for GPT-5 Responses API support
source("scripts/global_scripts/08_ai/fn_chat_api.R")

# ========== 語言系統函數 ==========

#' 取得模組語言文字
#' @param key 語言鍵值（支援巢狀路徑，如 "ui.welcome.banner"）
#' @param default 預設值
#' @param ... 格式化參數（用於 glue::glue）
#' @return 語言文字
get_lang_text <- function(key, default = "", module_name = NULL, ...) {
  tryCatch({
    cat("🔍 [get_lang_text] START - key:", key, "\n")

    # 確認 global_lang_content 存在
    if (!exists("global_lang_content", envir = .GlobalEnv)) {
      cat("⚠️ [get_lang_text] global_lang_content NOT EXISTS\n")
      return(default)
    }

    cat("✅ [get_lang_text] global_lang_content exists\n")

    lang_content_func <- get("global_lang_content", envir = .GlobalEnv)
    cat("🔍 [get_lang_text] Type:", class(lang_content_func)[1], "\n")

    # 檢查是否為 reactive，如果是則需要在 reactive context 中調用
    if (is.function(lang_content_func)) {
      cat("🔧 [get_lang_text] Is a reactive function\n")
      # 首先嘗試從 global_language_state 直接讀取（避免在非 reactive context 調用 reactive）
      if (exists("global_language_state", envir = .GlobalEnv)) {
        cat("🔧 [get_lang_text] Trying global_language_state first\n")
        state <- get("global_language_state", envir = .GlobalEnv)
        if (!is.null(state$language_content)) {
          cat("✅ [get_lang_text] Got content from global_language_state\n")
          lang_content <- state$language_content
        } else {
          cat("⚠️ [get_lang_text] global_language_state has NULL content, returning default\n")
          return(default)
        }
      } else {
        cat("⚠️ [get_lang_text] global_language_state not exists, returning default\n")
        return(default)
      }
    } else {
      cat("🔧 [get_lang_text] Not a function, using directly\n")
      lang_content <- lang_content_func
    }

    # 如果 key 以 "modules." 開頭，從 modules 中查找
    if (grepl("^modules\\.", key)) {
      keys <- strsplit(key, "\\.")[[1]]
      if (length(keys) >= 2) {
        module_name <- keys[2]
        remaining_keys <- keys[-(1:2)]

        module_content <- lang_content$content$modules[[module_name]]
        if (is.null(module_content)) {
          cat("⚠️ [get_lang_text] Module content not found:", module_name, "\n")
          return(default)
        }

        value <- module_content
        for (k in remaining_keys) {
          if (is.list(value) && k %in% names(value)) {
            value <- value[[k]]
          } else {
            cat("⚠️ [get_lang_text] Key not found:", k, "\n")
            return(default)
          }
        }
      } else {
        return(default)
      }
    } else {
      # 如果沒有指定模組，嘗試從所有模組中查找
      # 或者從 common 中查找
      keys <- strsplit(key, "\\.")[[1]]

      # 先嘗試從 common 查找
      if (!is.null(lang_content$content$common)) {
        value <- lang_content$content$common
        found <- TRUE
        for (k in keys) {
          if (is.list(value) && k %in% names(value)) {
            value <- value[[k]]
          } else {
            found <- FALSE
            break
          }
        }
        if (found && !is.null(value)) {
          # 格式化並返回
          if (length(list(...)) > 0 && is.character(value)) {
            dots <- list(...)
            for (name in names(dots)) {
              value <- gsub(paste0("\\{", name, "\\}"), dots[[name]], value)
            }
          }
          cat("✅ [get_lang_text] Found in common, returning\n")
          return(as.character(value))
        }
      }

      # 如果沒找到，返回默認值
      cat("⚠️ [get_lang_text] Not found, returning default\n")
      return(default)
    }

    # 如果有格式化參數，使用字串替換
    if (length(list(...)) > 0 && is.character(value)) {
      dots <- list(...)
      for (name in names(dots)) {
        value <- gsub(paste0("\\{", name, "\\}"), dots[[name]], value)
      }
    }

    cat("✅ [get_lang_text] SUCCESS, returning value\n")
    return(as.character(value))
  }, error = function(e) {
    cat("❌ [get_lang_text] OUTER ERROR:", e$message, "\n")
    cat("❌ [get_lang_text] Error class:", class(e), "\n")
    return(default)
  })
}

#' 取得訊息文字（便捷函數）
#' @param type 訊息類型：success, error, warning, info, processing
#' @param key 訊息鍵值
#' @param ... 格式化參數
get_msg <- function(type, key, ...) {
  full_key <- paste0("messages.", type, ".", key)
  get_lang_text(full_key, default = paste0("[", key, "]"), ...)
}

# ========== 共用函數 ==========
# Note: chat_api() is sourced from scripts/global_scripts/08_ai/fn_chat_api.R
# which correctly handles GPT-5 Responses API format

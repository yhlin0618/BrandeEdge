#!/usr/bin/env Rscript
# ==========================================
# 統一內容管理系統 - Unified Content Manager
# ==========================================
# 統一管理語言、Markdown、Prompt、Hint 內容的即時切換
# 確保所有模組在語言變更時自動更新內容
# Version: 1.0
# Last Updated: 2025-09-29

library(shiny)

# ==========================================
# 安全文字取得器 - Safe Text Getter
# ==========================================
safe_get_text <- function(content, path, default = NULL) {
  # 安全地從嵌套列表中取得文字
  # path: 點分隔的路徑，如 "modules.sales_model.title"
  # content: 語言內容物件
  # default: 預設值

  tryCatch({
    if (is.null(content) || is.null(path)) {
      return(default %||% path)
    }

    # 如果 path 不是字串，直接返回預設值
    if (!is.character(path) || length(path) == 0) {
      return(default %||% "[INVALID_PATH]")
    }

    # 分割路徑
    keys <- strsplit(path, "\\.")[[1]]
    current <- content

    # 遞迴取得值
    for (key in keys) {
      if (is.list(current) && key %in% names(current)) {
        current <- current[[key]]
      } else {
        # 找不到時返回預設值
        return(default %||% path)
      }
    }

    # 確保返回值是字串
    if (is.null(current)) {
      return(default %||% path)
    }

    return(as.character(current))

  }, error = function(e) {
    cat("❌ [Safe Text Getter] 錯誤:", e$message, "\n")
    cat("   路徑:", path, "\n")
    return(default %||% path)
  })
}

# ==========================================
# 當前語言狀態管理
# ==========================================
# 使用非反應式變數作為備用，供測試環境使用
current_language_state_data <- list(
  language = "zh_TW",
  content = NULL,
  hints = NULL,
  prompts = NULL,
  last_updated = Sys.time()
)

# 只在 Shiny 環境中使用 reactiveValues
current_language_state <- if (exists("reactiveValues")) {
  tryCatch({
    reactiveValues(
      language = "zh_TW",
      content = NULL,
      hints = NULL,
      prompts = NULL,
      last_updated = Sys.time()
    )
  }, error = function(e) {
    NULL
  })
} else {
  NULL
}

# ==========================================
# 取得當前語言
# ==========================================
get_current_language <- function() {
  # 優先使用反應式狀態
  if (!is.null(current_language_state)) {
    tryCatch({
      return(isolate(current_language_state$language))
    }, error = function(e) {
      # 在非 Shiny 環境中，fallback 到靜態狀態
      return(current_language_state_data$language)
    })
  }
  # 使用靜態狀態
  return(current_language_state_data$language)
}

# ==========================================
# 取得當前語言內容
# ==========================================
get_current_language_content <- function() {
  # 優先使用反應式狀態
  if (!is.null(current_language_state)) {
    tryCatch({
      return(isolate(current_language_state$content))
    }, error = function(e) {
      # 在非 Shiny 環境中，fallback 到靜態狀態
      return(current_language_state_data$content)
    })
  }
  # 使用靜態狀態
  return(current_language_state_data$content)
}

# ==========================================
# 取得當前 hints
# ==========================================
get_current_hints <- function() {
  # 優先使用反應式狀態
  if (!is.null(current_language_state)) {
    tryCatch({
      return(isolate(current_language_state$hints))
    }, error = function(e) {
      # 在非 Shiny 環境中，fallback 到靜態狀態
      return(current_language_state_data$hints)
    })
  }
  # 使用靜態狀態
  return(current_language_state_data$hints)
}

# ==========================================
# 取得當前 prompts
# ==========================================
get_current_prompts <- function() {
  # 優先使用反應式狀態
  if (!is.null(current_language_state)) {
    tryCatch({
      return(isolate(current_language_state$prompts))
    }, error = function(e) {
      # 在非 Shiny 環境中，fallback 到靜態狀態
      return(current_language_state_data$prompts)
    })
  }
  # 使用靜態狀態
  return(current_language_state_data$prompts)
}

# ==========================================
# 取得模組語言內容
# ==========================================
get_module_language_content <- function(module_id, language = NULL) {
  cat("\n🔍 [get_module_language_content] ===\n")
  cat("   module_id:", module_id, "\n")
  cat("   language param:", language, "\n")

  # Determine language
  if (is.null(language)) {
    language <- get_current_language()
    cat("   Language was NULL, using current:", language, "\n")
  }

  # ✅ FIX: Read from YAML file based on language parameter
  # Map language codes to directory names
  lang_dir_map <- list(
    "zh_TW" = "chinese",
    "en_US" = "english",
    "ja_JP" = "japanese",
    "ko_KR" = "korean"
  )

  lang_dir <- lang_dir_map[[language]]
  if (is.null(lang_dir)) {
    cat("   ⚠️ Unknown language code:", language, "- falling back to chinese\n")
    lang_dir <- "chinese"
  }

  # Construct YAML path
  yaml_path <- file.path("database", "content", lang_dir, "module", paste0(module_id, ".yaml"))
  cat("   📁 YAML path:", yaml_path, "\n")

  # Read fresh content from YAML file
  if (file.exists(yaml_path)) {
    cat("   ✅ Reading from YAML file...\n")
    module_content <- tryCatch({
      yaml::read_yaml(yaml_path)
    }, error = function(e) {
      cat("   ❌ Error reading YAML:", e$message, "\n")
      NULL
    })

    if (!is.null(module_content)) {
      module_content$language <- language
      cat("   ✅ Successfully loaded module content for", language, "\n")
      cat("   Keys:", paste(head(names(module_content), 10), collapse = ", "), "\n")
      cat("=== [get_module_language_content END] ===\n\n")
      return(module_content)
    }
  } else {
    cat("   ⚠️ YAML file not found:", yaml_path, "\n")
  }

  # Fallback: try global_language_state as last resort
  cat("   ⚠️ Falling back to global_language_state...\n")
  if (exists("global_language_state", envir = .GlobalEnv)) {
    lang_state <- get("global_language_state", envir = .GlobalEnv)
    if (!is.null(lang_state$language_content$modules[[module_id]])) {
      cat("   ✅ Found in global_language_state\n")
      module_content <- lang_state$language_content$modules[[module_id]]
      module_content$language <- language
      cat("=== [get_module_language_content END] ===\n\n")
      return(module_content)
    }
  }

  # Last resort: return empty with language tag
  cat("   ❌ Module content not found anywhere, returning empty\n")
  cat("=== [get_module_language_content END] ===\n\n")
  return(list(language = language))
}

# ==========================================
# 統一語言狀態更新器
# ==========================================
update_language_state <- function(new_language, lang_content = NULL, hints = NULL, prompts = NULL) {
  cat("🔄 [Language State] 更新語言狀態:", new_language, "\n")

  tryCatch({
    # 載入語言內容（如果沒有提供）
    if (is.null(lang_content)) {
      if (exists("load_language_content") && is.function(load_language_content)) {
        lang_content <- load_language_content(language = new_language)
      }
    }

    # 載入 hints（如果沒有提供）
    if (is.null(hints)) {
      if (exists("load_hints") && is.function(load_hints)) {
        hints <- load_hints(language = new_language)
      }
    }

    # 載入 prompts（如果沒有提供）
    if (is.null(prompts)) {
      if (exists("load_prompts") && is.function(load_prompts)) {
        # Get app_name from environment variable or use default
        app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
        prompts <- load_prompts(language = new_language, app_name = app_name)
      }
    }

    # 更新狀態 - 確保正確處理 lang_content 的結構
    # 如果 lang_content 有 content 屬性且它本身有 content 屬性，則使用它
    # 否則如果 lang_content 有 content 屬性，使用它
    # 最後使用 lang_content 本身
    if (!is.null(lang_content$content) && !is.null(lang_content$content$modules)) {
      # lang_content 已經有正確的結構
      actual_content <- lang_content$content
    } else if (!is.null(lang_content$modules)) {
      # lang_content 本身就是 content
      actual_content <- lang_content
    } else {
      # 兜底：保持原樣
      actual_content <- lang_content
    }

    actual_hints <- if (!is.null(lang_content$hints)) lang_content$hints else hints
    actual_prompts <- if (!is.null(lang_content$prompts)) lang_content$prompts else prompts

    if (!is.null(current_language_state)) {
      tryCatch({
        current_language_state$language <- new_language
        current_language_state$content <- actual_content
        current_language_state$hints <- actual_hints
        current_language_state$prompts <- actual_prompts
        current_language_state$last_updated <- Sys.time()
      }, error = function(e) {
        # 在非 Shiny 環境中，更新靜態狀態
        current_language_state_data$language <<- new_language
        current_language_state_data$content <<- actual_content
        current_language_state_data$hints <<- actual_hints
        current_language_state_data$prompts <<- actual_prompts
        current_language_state_data$last_updated <<- Sys.time()
      })
    } else {
      # 更新靜態狀態
      current_language_state_data$language <<- new_language
      current_language_state_data$content <<- actual_content
      current_language_state_data$hints <<- actual_hints
      current_language_state_data$prompts <<- actual_prompts
      current_language_state_data$last_updated <<- Sys.time()
    }

    cat("✅ [Language State] 語言狀態更新完成:", new_language, "\n")
    return(TRUE)

  }, error = function(e) {
    cat("❌ [Language State] 更新失敗:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================
# 模組語言更新器註冊系統
# ==========================================
module_language_updaters <- list()

# 創建模組語言更新器
create_module_language_updater <- function(module_id, update_function) {
  cat("📝 [Module Updater] 註冊模組語言更新器:", module_id, "\n")

  module_language_updaters[[module_id]] <<- update_function

  # 立即執行一次更新（使用當前語言內容）
  current_language <- get_current_language()
  current_content <- get_module_language_content(module_id, current_language)

  tryCatch({
    update_function(current_language, current_content)
  }, error = function(e) {
    cat("❌ [Module Updater] 初始更新失敗:", module_id, "-", e$message, "\n")
  })
}

# 觸發所有模組語言更新
trigger_all_module_updates <- function(new_language) {
  cat("🔄 [Module Updates] 觸發所有模組語言更新:", new_language, "\n")

  if (length(module_language_updaters) == 0) {
    cat("⚠️ [Module Updates] 沒有註冊的模組更新器\n")
    return()
  }

  for (module_id in names(module_language_updaters)) {
    tryCatch({
      updater <- module_language_updaters[[module_id]]
      module_content <- get_module_language_content(module_id, new_language)

      if (is.function(updater)) {
        updater(new_language, module_content)
        cat("✅ [Module Updates] 已更新模組:", module_id, "\n")
      }

    }, error = function(e) {
      cat("❌ [Module Updates] 模組更新失敗:", module_id, "-", e$message, "\n")
    })
  }

  cat("🎉 [Module Updates] 所有模組語言更新完成\n")
}

# ==========================================
# 統一語言化 Markdown 載入器
# ==========================================
load_language_markdown <- function(filename, language = NULL, fallback_path = NULL) {
  # 根據當前語言載入對應的 markdown 檔案

  if (is.null(language)) {
    language <- get_current_language()
  }

  # 決定語言目錄
  language_dir <- if (language == "en_US") "english" else "chinese"

  # 建立語言化檔案路徑
  language_path <- file.path("database", "content", language_dir, "markdown", filename)

  # 檢查語言化檔案是否存在
  if (file.exists(language_path)) {
    return(includeMarkdown(language_path))
  }

  # 如果語言化檔案不存在，嘗試使用備用路徑
  if (!is.null(fallback_path) && file.exists(fallback_path)) {
    cat("⚠️ [Language Markdown] 使用備用路徑:", fallback_path, "\n")
    return(includeMarkdown(fallback_path))
  }

  # 如果都不存在，嘗試使用原始 md/ 目錄
  original_path <- file.path("md", filename)
  if (file.exists(original_path)) {
    cat("⚠️ [Language Markdown] 使用原始檔案:", original_path, "\n")
    return(includeMarkdown(original_path))
  }

  # 都找不到檔案時返回錯誤訊息
  cat("❌ [Language Markdown] 找不到檔案:", filename, "\n")
  return(div(
    class = "alert alert-warning",
    style = "margin: 10px 0;",
    tags$strong("檔案載入失敗: "), filename
  ))
}

# ==========================================
# 反應式語言化 Markdown
# ==========================================
reactive_language_markdown <- function(filename, fallback_path = NULL) {
  # 用於 Server 端的反應式 Markdown 載入

  renderUI({
    current_language <- get_current_language()
    load_language_markdown(filename, current_language, fallback_path)
  })
}

# ==========================================
# 統一語言化 Prompt 取得器
# ==========================================
get_language_prompt <- function(prompt_id, variables = NULL, language = NULL) {
  # 根據當前語言取得 prompt

  if (is.null(language)) {
    language <- get_current_language()
  }

  prompts <- get_current_prompts()

  if (is.null(prompts) || !prompt_id %in% prompts$var_id) {
    cat("⚠️ [Language Prompt] 找不到 prompt:", prompt_id, "\n")
    return(paste("Prompt not found:", prompt_id))
  }

  # 使用現有的 prepare_gpt_messages 函數（如果存在）
  if (exists("prepare_gpt_messages") && is.function(prepare_gpt_messages)) {
    return(prepare_gpt_messages(prompt_id, variables, prompts))
  }

  # 備用方案：直接返回 prompt 內容
  prompt_row <- prompts[prompts$var_id == prompt_id, ]
  if (nrow(prompt_row) > 0) {
    return(prompt_row$content[1])
  }

  return(paste("Prompt content not available:", prompt_id))
}

# ==========================================
# 統一語言化 Hint 取得器
# ==========================================
get_language_hint <- function(hint_id, language = NULL) {
  # 根據當前語言取得 hint

  if (is.null(language)) {
    language <- get_current_language()
  }

  hints <- get_current_hints()

  if (is.null(hints) || !hint_id %in% hints$hint_id) {
    cat("⚠️ [Language Hint] 找不到 hint:", hint_id, "\n")
    return(paste("Hint not found:", hint_id))
  }

  hint_row <- hints[hints$hint_id == hint_id, ]
  if (nrow(hint_row) > 0) {
    return(hint_row$content[1])
  }

  return(paste("Hint content not available:", hint_id))
}

# ==========================================
# 語言狀態即時監控器
# ==========================================
create_language_monitor <- function(session) {
  # 創建語言狀態變更監控器

  observe({
    # 監聽全局語言變更
    if (exists("global_lang_content") && is.function(global_lang_content)) {
      current_global_content <- global_lang_content()

      if (!is.null(current_global_content)) {
        new_language <- current_global_content$language
        current_state_language <- get_current_language()

        # 只在語言真正改變時更新
        if (new_language != current_state_language) {
          cat("🌍 [Language Monitor] 偵測到語言變更:", current_state_language, "->", new_language, "\n")

          # 更新統一語言狀態
          update_success <- update_language_state(
            new_language = new_language,
            lang_content = current_global_content
          )

          if (update_success) {
            # 觸發所有模組更新
            trigger_all_module_updates(new_language)

            # 發送客戶端更新訊息
            if (!is.null(session)) {
              session$sendCustomMessage("unified_language_update", list(
                language = new_language,
                timestamp = as.numeric(Sys.time())
              ))
            }
          }
        }
      }
    }
  })
}

# ==========================================
# 即時語言切換狀態偵測器
# ==========================================
detect_language_status <- function() {
  # 即時偵測當前語言狀態

  # 安全取得最後更新時間
  last_updated <- if (!is.null(current_language_state)) {
    tryCatch({
      isolate(current_language_state$last_updated)
    }, error = function(e) {
      current_language_state_data$last_updated
    })
  } else {
    current_language_state_data$last_updated
  }

  status <- list(
    current_language = get_current_language(),
    last_updated = last_updated,
    content_available = !is.null(get_current_language_content()),
    hints_available = !is.null(get_current_hints()),
    prompts_available = !is.null(get_current_prompts()),
    registered_modules = if(exists("module_language_updaters")) names(module_language_updaters) else character(0)
  )

  return(status)
}

# ==========================================
# 語言狀態診斷報告器
# ==========================================
generate_language_status_report <- function() {
  cat("\n🔍 ========== 語言狀態診斷報告 ==========\n")

  status <- detect_language_status()

  cat("📋 基本資訊:\n")
  cat("   當前語言:", status$current_language, "\n")
  cat("   最後更新:", status$last_updated, "\n")
  cat("   語言內容:", if(status$content_available) "✅ 可用" else "❌ 不可用", "\n")
  cat("   Hints:", if(status$hints_available) "✅ 可用" else "❌ 不可用", "\n")
  cat("   Prompts:", if(status$prompts_available) "✅ 可用" else "❌ 不可用", "\n")

  cat("\n📦 註冊的模組更新器 (", length(status$registered_modules), "個):\n")
  if (length(status$registered_modules) > 0) {
    for (module in status$registered_modules) {
      cat("   -", module, "\n")
    }
  } else {
    cat("   ⚠️ 沒有註冊的模組更新器\n")
  }

  # 檢查模組內容可用性
  if (status$content_available) {
    content <- get_current_language_content()
    if (!is.null(content$modules)) {
      cat("\n📋 可用的模組語言內容:\n")
      for (module_name in names(content$modules)) {
        cat("   -", module_name, "\n")
      }
    }
  }

  cat("\n🔍 =======================================\n\n")

  return(status)
}

cat("✅ 已載入統一內容管理系統\n")
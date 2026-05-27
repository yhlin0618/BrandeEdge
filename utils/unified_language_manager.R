# ==========================================
# 统一语言管理器 - Unified Language Manager
# ==========================================
# 提供统一的语言切换检测和内容更新机制
# Version: 1.0
# Last Updated: 2025-09-29

library(shiny)

# ==========================================
# 全局语言状态管理
# ==========================================

# 创建全局反应式值存储当前语言状态
if (!exists("global_language_state")) {
  global_language_state <- reactiveValues(
    current_language = "zh_TW",
    language_content = NULL,
    hints_content = NULL,
    prompts_content = NULL,
    last_update_time = Sys.time(),
    update_trigger = 0
  )
}

# ==========================================
# 语言内容加载函数
# ==========================================

#' 统一加载语言内容
#' @param language 语言代码 (如 "zh_TW", "en_US")
#' @param force_reload 是否强制重新加载
load_unified_language_content <- function(language = "zh_TW", force_reload = FALSE) {
  tryCatch({
    cat("🌍 [统一语言管理] 开始加载语言内容:", language, "\n")
    cat("🔍 [CALL STACK] 調用堆棧:\n")
    call_stack <- sys.calls()
    for (i in length(call_stack):max(1, length(call_stack)-5)) {
      cat("  ", i, ": ", deparse(call_stack[[i]])[1], "\n", sep="")
    }
    cat("🔍 [END STACK]\n")

    # 安全检查当前语言状态
    current_lang <- tryCatch({
      if (exists("global_language_state")) {
        shiny::isolate(global_language_state$current_language)
      } else {
        NULL
      }
    }, error = function(e) {
      cat("⚠️ [统一语言管理] 获取当前语言状态失败:", e$message, "\n")
      NULL
    })

    current_content <- tryCatch({
      if (exists("global_language_state")) {
        shiny::isolate(global_language_state$language_content)
      } else {
        NULL
      }
    }, error = function(e) {
      cat("⚠️ [统一语言管理] 获取当前内容失败:", e$message, "\n")
      NULL
    })

    # 如果语言没有变化且不强制重载，直接返回
    if (!force_reload && !is.null(current_lang) && current_lang == language && !is.null(current_content)) {
      cat("🌍 [统一语言管理] 语言未变化，跳过重载\n")
      return(current_content)
    }

    # 1. 加载主要语言内容
    main_content <- if (exists("load_language_content") && is.function(load_language_content)) {
      # 嘗試獲取 app_config（如果存在）
      config_param <- if (exists("app_config", envir = .GlobalEnv)) {
        get("app_config", envir = .GlobalEnv)
      } else {
        NULL
      }
      load_language_content(config = config_param, language = language)
    } else {
      list(language = language, content = list())
    }

    # 2. 加载 Hints 内容
    hints_content <- if (exists("load_hints") && is.function(load_hints)) {
      tryCatch({
        load_hints(language = language)
      }, error = function(e) {
        cat("⚠️ [统一语言管理] Hints 加载失败:", e$message, "\n")
        NULL
      })
    } else {
      NULL
    }

    # 3. 加载 Prompts 内容
    prompts_content <- if (exists("load_prompts") && is.function(load_prompts)) {
      tryCatch({
        # Get app_name from environment variable or use default
        app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
        load_prompts(language = language, app_name = app_name)
      }, error = function(e) {
        cat("⚠️ [统一语言管理] Prompts 加载失败:", e$message, "\n")
        NULL
      })
    } else {
      NULL
    }

    # 4. 更新全局状态
    if (exists("global_language_state")) {
      # 安全更新反应式值
      tryCatch({
        current_trigger <- tryCatch({
          shiny::isolate(global_language_state$update_trigger)
        }, error = function(e) {
          0
        }) %||% 0

        global_language_state$current_language <- language
        global_language_state$language_content <- main_content
        global_language_state$hints_content <- hints_content
        global_language_state$prompts_content <- prompts_content
        global_language_state$last_update_time <- Sys.time()
        global_language_state$update_trigger <- current_trigger + 1

        # 🔧 Create NON-REACTIVE copy for UI generator (accessed outside reactive context)
        .GlobalEnv$global_language_content_static <- main_content

        cat("✅ [统一语言管理] 全局状态更新完成\n")

        # 立即驗證更新是否成功
        cat("🔍 [驗證] 更新後立即檢查:\n")
        verify_content <- shiny::isolate(global_language_state$language_content)
        cat("  - global_language_state$language_content$language:", verify_content$language %||% "NULL", "\n")
        cat("  - update_trigger 值:", current_trigger + 1, "\n")
      }, error = function(e) {
        cat("⚠️ [统一语言管理] 更新全局状态失败:", e$message, "\n")
      })
    }

    # 5. 同步到 global_lang_content（向后兼容）
    # NOTE: global_lang_content is a reactive() expression, NOT a reactiveVal()
    # It automatically derives its value from global_language_state
    # So we don't need to (and can't) set it directly
    cat("✅ [向后兼容] global_lang_content 由 global_language_state 自动派生\n")

    cat("✅ [统一语言管理] 语言内容加载完成:", language, "\n")
    return(main_content)

  }, error = function(e) {
    cat("❌ [统一语言管理] 加载失败:", e$message, "\n")
    return(NULL)
  })
}

# ==========================================
# 语言变化检测器
# ==========================================

#' 创建语言变化检测器
#' @param session Shiny session 对象
#' @param callback 语言变化时的回调函数
create_language_change_detector <- function(session = NULL, callback = NULL) {

  # 返回一个 observe 表达式，可以在模块中使用
  observe({
    # 检测全局语言状态变化
    if (exists("global_language_state")) {
      # 安全监听 update_trigger 变化（在反应式上下文中正常访问）
      trigger_value <- tryCatch({
        global_language_state$update_trigger
      }, error = function(e) {
        cat("⚠️ [语言变化检测] 获取触发器值失败:", e$message, "\n")
        0
      })

      # 安全获取当前语言内容
      current_content <- tryCatch({
        global_language_state$language_content
      }, error = function(e) {
        cat("⚠️ [语言变化检测] 获取语言内容失败:", e$message, "\n")
        NULL
      })

      current_language <- tryCatch({
        global_language_state$current_language
      }, error = function(e) {
        cat("⚠️ [语言变化检测] 获取当前语言失败:", e$message, "\n")
        NULL
      })

      if (!is.null(current_content) && !is.null(current_language)) {
        cat("🔄 [语言变化检测] 检测到语言更新 - 语言:", current_language, "触发器:", trigger_value, "\n")

        # 执行回调函数
        if (is.function(callback)) {
          tryCatch({
            callback(current_language, current_content)
          }, error = function(e) {
            cat("❌ [语言变化检测] 回调函数执行失败:", e$message, "\n")
          })
        }
      }
    }
  })
}

# ==========================================
# 模块语言内容获取器
# ==========================================

#' 获取模块的语言内容
#' @param module_name 模块名称
#' @param language 语言代码（可选，默认使用当前语言）
get_module_language_content <- function(module_name, language = NULL) {
  cat("\n🔍 [get_module_language_content] ===\n")
  cat("   module_name:", module_name, "\n")
  cat("   language param:", language, "\n")

  tryCatch({
    # 安全获取当前语言
    current_lang <- if (!is.null(language)) {
      language
    } else {
      tryCatch({
        if (exists("global_language_state")) {
          shiny::isolate(global_language_state$current_language)
        } else {
          NULL
        }
      }, error = function(e) {
        "zh_TW"
      }) %||% "zh_TW"
    }

    cat("   Using language:", current_lang, "\n")

    # ✅ FIX: Read directly from YAML file based on language parameter
    # Map language codes to directory names
    lang_dir_map <- list(
      "zh_TW" = "chinese",
      "en_US" = "english",
      "ja_JP" = "japanese",
      "ko_KR" = "korean"
    )

    lang_dir <- lang_dir_map[[current_lang]]
    if (is.null(lang_dir)) {
      cat("   ⚠️ Unknown language code:", current_lang, "- falling back to chinese\n")
      lang_dir <- "chinese"
    }

    # Construct YAML path
    yaml_path <- file.path("database", "content", lang_dir, "module", paste0(module_name, ".yaml"))
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
        module_content$language <- current_lang
        cat("   ✅ Successfully loaded module content for", current_lang, "\n")
        cat("   Keys:", paste(head(names(module_content), 10), collapse = ", "), "\n")

        # 調試：檢查 scoring 的 messages.info 內容
        if (module_name == "scoring" && !is.null(module_content$messages$info)) {
          cat("🔍 [調試] scoring messages.info 鍵數量:", length(names(module_content$messages$info)), "\n")
          cat("🔍 [調試] 前 5 個鍵:", paste(head(names(module_content$messages$info), 5), collapse = ", "), "\n")
          cat("🔍 [調試] 是否有 attributes_generated:", "attributes_generated" %in% names(module_content$messages$info), "\n")
          cat("🔍 [調試] 是否有 attributes_generated_notify:", "attributes_generated_notify" %in% names(module_content$messages$info), "\n")
        }

        cat("=== [get_module_language_content END] ===\n\n")
        return(module_content)
      }
    } else {
      cat("   ⚠️ YAML file not found:", yaml_path, "\n")
    }

    # Fallback: try global_language_state as last resort
    cat("   ⚠️ Falling back to global_language_state...\n")
    lang_content <- tryCatch({
      if (exists("global_language_state")) {
        shiny::isolate(global_language_state$language_content)
      } else {
        NULL
      }
    }, error = function(e) {
      cat("   ❌ Error getting global state:", e$message, "\n")
      NULL
    })

    if (!is.null(lang_content) &&
        !is.null(lang_content$content) &&
        !is.null(lang_content$content$modules) &&
        module_name %in% names(lang_content$content$modules)) {

      module_content <- lang_content$content$modules[[module_name]]
      module_content$language <- current_lang
      cat("   ✅ Found in global_language_state\n")
      cat("=== [get_module_language_content END] ===\n\n")
      return(module_content)
    } else {
      cat("   ⚠️ Module content not found anywhere\n")
      cat("=== [get_module_language_content END] ===\n\n")
      return(list(language = current_lang))
    }

  }, error = function(e) {
    cat("❌ [get_module_language_content] Error:", e$message, "\n")
    cat("=== [get_module_language_content END] ===\n\n")
    return(list(language = "zh_TW"))
  })
}

# ==========================================
# Hints 内容获取器
# ==========================================

#' 获取当前语言的 Hints 内容
get_current_hints <- function() {
  if (exists("global_language_state")) {
    tryCatch({
      return(shiny::isolate(global_language_state$hints_content))
    }, error = function(e) {
      cat("⚠️ [获取Hints] 失败:", e$message, "\n")
      return(NULL)
    })
  }
  return(NULL)
}

# ==========================================
# Prompts 内容获取器
# ==========================================

#' 获取当前语言的 Prompts 内容
get_current_prompts <- function() {
  if (exists("global_language_state")) {
    tryCatch({
      return(shiny::isolate(global_language_state$prompts_content))
    }, error = function(e) {
      cat("⚠️ [获取Prompts] 失败:", e$message, "\n")
      return(NULL)
    })
  }
  return(NULL)
}

# ==========================================
# 语言切换触发器
# ==========================================

#' 触发语言切换（供语言选择器使用）
#' @param new_language 新的语言代码
trigger_language_change <- function(new_language) {
  tryCatch({
    cat("🔄 [语言切换触发] 开始切换到:", new_language, "\n")

    # 加载新语言内容
    # 这会自动更新 global_language_state$language_content
    load_unified_language_content(new_language, force_reload = TRUE)

    # 注意：global_lang_content 是 reactive()，会自动反映 global_language_state$language_content 的变化
    # 因此不需要手动更新

    cat("✅ [语言切换触发] 切换完成:", new_language, "\n")
    return(TRUE)

  }, error = function(e) {
    cat("❌ [语言切换触发] 切换失败:", e$message, "\n")
    return(FALSE)
  })
}

# ==========================================
# 模块语言更新助手
# ==========================================

#' 为模块创建语言更新逻辑
#' @param module_name 模块名称
#' @param update_function 更新函数，接收 (language, content) 参数
create_module_language_updater <- function(module_name, update_function = NULL) {

  # 返回 observe 表达式
  observe({
    # 监听全局语言状态变化
    if (exists("global_language_state")) {
      # 安全获取触发器值（在反应式上下文中正常访问）
      trigger_value <- tryCatch({
        global_language_state$update_trigger
      }, error = function(e) {
        cat("⚠️ [", module_name, "] 获取触发器值失败:", e$message, "\n")
        0
      })

      current_language <- tryCatch({
        global_language_state$current_language
      }, error = function(e) {
        cat("⚠️ [", module_name, "] 获取当前语言失败:", e$message, "\n")
        NULL
      })

      if (!is.null(current_language)) {
        cat("🔄 [", module_name, "] 检测到语言变化:", current_language, "\n")

        # 获取模块语言内容
        module_content <- get_module_language_content(module_name, current_language)

        # 执行更新函数
        if (is.function(update_function)) {
          tryCatch({
            update_function(current_language, module_content)
            cat("✅ [", module_name, "] 语言内容更新完成\n")
          }, error = function(e) {
            cat("❌ [", module_name, "] 语言内容更新失败:", e$message, "\n")
          })
        }
      }
    }
  })
}

# ==========================================
# 语言文本安全获取器
# ==========================================

#' 安全获取语言文本，带有备用值
#' @param lang_content 语言内容对象
#' @param path 文本路径（如 "tabs.sales_model"）
#' @param default 默认值
safe_get_text <- function(lang_content, path, default = NULL) {
  tryCatch({
    if (is.null(lang_content)) {
      return(default)
    }

    # 分割路径
    keys <- strsplit(path, "\\.")[[1]]

    # 遍历路径
    current <- lang_content
    for (key in keys) {
      if (is.list(current) && key %in% names(current)) {
        current <- current[[key]]
      } else {
        return(default)
      }
    }

    return(current)

  }, error = function(e) {
    cat("⚠️ [安全文本获取] 获取失败:", path, "错误:", e$message, "\n")
    return(default)
  })
}

# ==========================================
# 调试工具
# ==========================================

#' 显示当前语言状态
show_language_status <- function() {
  if (exists("global_language_state")) {
    tryCatch({
      # 使用 isolate 安全获取所有状态信息
      current_language <- shiny::isolate(global_language_state$current_language)
      last_update_time <- shiny::isolate(global_language_state$last_update_time)
      update_trigger <- shiny::isolate(global_language_state$update_trigger)
      language_content <- shiny::isolate(global_language_state$language_content)
      hints_content <- shiny::isolate(global_language_state$hints_content)
      prompts_content <- shiny::isolate(global_language_state$prompts_content)

      cat("========== 语言状态信息 ==========\n")
      cat("当前语言:", current_language %||% "未设置", "\n")
      cat("最后更新:", as.character(last_update_time %||% "未设置"), "\n")
      cat("更新触发器:", update_trigger %||% "0", "\n")
      cat("主要内容:", if (is.null(language_content)) "未加载" else "已加载", "\n")
      cat("Hints内容:", if (is.null(hints_content)) "未加载" else "已加载", "\n")
      cat("Prompts内容:", if (is.null(prompts_content)) "未加载" else "已加载", "\n")
      cat("==================================\n")
    }, error = function(e) {
      cat("❌ 显示语言状态失败:", e$message, "\n")
    })
  } else {
    cat("❌ 全局语言状态未初始化\n")
  }
}

# ==========================================
# 初始化函数
# ==========================================

#' 初始化统一语言管理器
#' @param initial_language 初始语言
initialize_unified_language_manager <- function(initial_language = "zh_TW") {
  cat("🚀 [统一语言管理] 开始初始化...\n")

  # 初始化全局状态
  if (!exists("global_language_state")) {
    global_language_state <<- reactiveValues(
      current_language = initial_language,
      language_content = NULL,
      hints_content = NULL,
      prompts_content = NULL,
      last_update_time = Sys.time(),
      update_trigger = 0
    )
  }

  # 创建向后兼容的 global_lang_content 反应式表达式
  # 注意：reactive() 必須在 server 函數內創建才能正確響應變化
  # 因此這裡不創建，改由 app_dynamic.R 的 server 函數開頭創建
  cat("ℹ️ [向后兼容] global_lang_content 將由 server 函數創建\n")
  cat("ℹ️ [向后兼容] 請確保 app_dynamic.R 的 server 函數開頭有創建 global_lang_content reactive\n")

  # 加载初始语言内容
  # 注意：load_unified_language_content 会自动同步到 global_lang_content
  initial_content <- load_unified_language_content(initial_language, force_reload = TRUE)

  cat("✅ [统一语言管理] 初始化完成\n")
}

# ==========================================
# 兼容性函数
# ==========================================

# 为了向后兼容，提供一些别名
# 安全访问反应式值的函数 - 适用于非反应式环境
get_unified_language_content <- function() {
  if (exists("global_language_state")) {
    # 使用 tryCatch 和 isolate 安全访问反应式值
    tryCatch({
      # 在非反应式环境中使用 isolate
      return(shiny::isolate(global_language_state$language_content))
    }, error = function(e) {
      cat("⚠️ [安全访问] 获取语言内容失败:", e$message, "\n")
      return(NULL)
    })
  }
  return(NULL)
}

get_current_language <- function() {
  if (exists("global_language_state")) {
    # 使用 tryCatch 和 isolate 安全访问反应式值
    tryCatch({
      # 在非反应式环境中使用 isolate
      return(shiny::isolate(global_language_state$current_language))
    }, error = function(e) {
      cat("⚠️ [安全访问] 获取当前语言失败:", e$message, "\n")
      return("zh_TW")
    })
  }
  return("zh_TW")
}

#!/usr/bin/env Rscript
# ==========================================
# 統一語言同步架構 - Enhanced Version
# ==========================================
# 確保所有模組在語言切換時同步更新
# Version: 2.0 - 整合統一內容管理系統
# Last Updated: 2025-09-29

library(shiny)

# 載入統一內容管理系統
if (file.exists("utils/unified_content_manager.R")) {
  source("utils/unified_content_manager.R")
}

# ==========================================
# 增強版統一語言切換處理函數
# ==========================================
unified_language_switch <- function(session, new_language, app_config, lang_config = NULL, module_ui_update_trigger = NULL) {
  cat("\n🌐 ===============================\n")
  cat("🌐 [統一語言切換] 開始 - Enhanced Version\n")
  cat("🌐 目標語言:", new_language, "\n")
  cat("🌐 ===============================\n")

  tryCatch({
    # 1. 更新 app_config
    if (!is.null(app_config) && !is.null(app_config$language)) {
      # 檢查 app_config 是否可以修改
      if (exists("app_config", envir = .GlobalEnv)) {
        app_config_global <- get("app_config", envir = .GlobalEnv)
        if (!is.null(app_config_global$language)) {
          app_config_global$language$default <- new_language
          assign("app_config", app_config_global, envir = .GlobalEnv)
        }
      }
      cat("✅ [Step 1] app_config 語言已更新\n")
    } else {
      cat("⚠️ [Step 1] 跳過 app_config 更新（不存在或無語言設定）\n")
    }

    # 2. 載入新的語言內容
    # 使用 app_config（從 .GlobalEnv 獲取最新版本）
    config_to_use <- if (exists("app_config", envir = .GlobalEnv)) {
      get("app_config", envir = .GlobalEnv)
    } else {
      app_config
    }
    new_lang_content <- load_language_content(config_to_use, language = new_language)
    if (is.null(new_lang_content)) {
      stop("無法載入語言內容")
    }
    cat("✅ [Step 2] 語言內容載入成功:", new_lang_content$language, "\n")

    # 3. 更新統一語言狀態管理
    if (exists("update_language_state") && is.function(update_language_state)) {
      update_success <- update_language_state(
        new_language = new_language,
        lang_content = new_lang_content
      )
      if (update_success) {
        cat("✅ [Step 3] 統一語言狀態已更新\n")
      } else {
        cat("⚠️ [Step 3] 統一語言狀態更新失敗\n")
      }
    }

    # 4. 更新全局語言內容（已在 load_language_content 中自動同步）
    cat("✅ [Step 4] 全局語言內容已在 load_language_content 中同步\n")

    # 5. 觸發統一模組更新
    if (exists("trigger_all_module_updates") && is.function(trigger_all_module_updates)) {
      trigger_all_module_updates(new_language)
      cat("✅ [Step 5] 所有模組語言更新已觸發\n")
    }

    # 6. 更新側邊欄
    if (exists("update_sidebar_menu_text")) {
      cat("🔄 [Step 6] 開始更新側邊欄...\n")
      update_sidebar_menu_text(session, app_config$pages, new_lang_content)
      cat("✅ [Step 6] 側邊欄更新完成\n")
    } else {
      cat("⚠️ [Step 6] 側邊欄更新函數不存在\n")
    }

    # 7. 觸發模組 UI 更新
    if (!is.null(module_ui_update_trigger) && is.function(module_ui_update_trigger)) {
      current_value <- isolate(module_ui_update_trigger())
      module_ui_update_trigger(current_value + 1)
      cat("✅ [Step 7] 模組 UI 更新觸發器已執行 (從", current_value, "到", current_value + 1, ")\n")
    } else {
      cat("⚠️ [Step 7] module_ui_update_trigger 未傳入或無效 - 模組 UI 不會更新！\n")
    }

    # 8. 發送客戶端同步消息
    # TEMPORARILY DISABLED - force_module_language_sync exists but may not be needed
    # session$sendCustomMessage("force_module_language_sync", list(
    #   language = new_language,
    #   timestamp = as.numeric(Sys.time())
    # ))
    cat("⚠️ [Step 8] 客戶端同步消息已停用 (測試中)\n")

    # 9. 發送側邊欄更新消息
    # DISABLED - Handler does not exist in app, causing errors
    # session$sendCustomMessage("update_sidebar_language", list(
    #   language = new_language,
    #   pages = app_config$pages,
    #   content = new_lang_content$content
    # ))
    cat("⚠️ [Step 9] 側邊欄更新消息已停用 (handler不存在)\n")

    # 10. 發送統一語言更新消息
    # DISABLED - Handler does not exist in app, causing errors
    # session$sendCustomMessage("unified_language_update", list(
    #   language = new_language,
    #   timestamp = as.numeric(Sys.time()),
    #   modules = names(module_language_updaters)
    # ))
    cat("⚠️ [Step 10] 統一語言更新消息已停用 (handler不存在)\n")

    # 11. 顯示成功通知
    language_display_name <- if(new_language == "en_US") "English" else "中文"
    showNotification(
      paste("✅", language_display_name),
      type = "message",
      duration = 2
    )

    cat("🌐 ===============================\n")
    cat("🌐 [統一語言切換] 完成 - Enhanced Version\n")
    cat("🌐 ===============================\n\n")

    return(list(success = TRUE, language = new_language))

  }, error = function(e) {
    cat("❌ [統一語言切換] 失敗:", e$message, "\n")
    showNotification(
      paste("語言切換失敗:", e$message),
      type = "error",
      duration = 3
    )
    return(list(success = FALSE, error = e$message))
  })
}

# ==========================================
# 增強版模組語言同步處理
# ==========================================
sync_module_language <- function(module_name, new_language, session = NULL) {
  cat("📦 [模組同步] Enhanced -", module_name, "-> 語言:", new_language, "\n")

  # 使用統一內容管理系統取得模組內容
  if (exists("get_module_language_content") && is.function(get_module_language_content)) {
    module_content <- get_module_language_content(module_name, new_language)

    # 觸發模組更新器
    if (exists("module_language_updaters") && module_name %in% names(module_language_updaters)) {
      updater <- module_language_updaters[[module_name]]
      if (is.function(updater)) {
        updater(new_language, module_content)
      }
    }
  }

  # 發送模組特定的語言更新消息
  if (!is.null(session)) {
    session$sendCustomMessage(paste0("update_", module_name, "_language"), list(
      language = new_language,
      timestamp = as.numeric(Sys.time())
    ))
  }

  # 返回成功狀態
  return(TRUE)
}

# ==========================================
# 增強版批量同步所有模組
# ==========================================
sync_all_modules <- function(new_language, session, module_list = NULL) {
  cat("\n🔄 [批量同步] Enhanced - 開始同步所有模組...\n")

  # 使用統一內容管理系統取得註冊的模組列表
  if (is.null(module_list)) {
    if (exists("module_language_updaters") && length(module_language_updaters) > 0) {
      module_list <- names(module_language_updaters)
    } else {
      # 預設模組列表
      module_list <- c("upload", "scoring", "sales_model", "keyword_ads", "product_dev", "wo_b_v2")
    }
  }

  cat("📋 [批量同步] 要同步的模組:", paste(module_list, collapse = ", "), "\n")

  # 使用統一觸發器
  if (exists("trigger_all_module_updates") && is.function(trigger_all_module_updates)) {
    trigger_all_module_updates(new_language)
  } else {
    # 逐個同步模組
    for (module in module_list) {
      sync_module_language(module, new_language, session)
    }
  }

  cat("✅ [批量同步] Enhanced - 所有模組同步完成\n\n")
  return(TRUE)
}

# ==========================================
# 增強版語言切換結果驗證
# ==========================================
verify_language_switch <- function(expected_language) {
  cat("\n🔍 [驗證] Enhanced - 檢查語言切換結果...\n")

  # 檢查統一語言狀態
  if (exists("get_current_language") && is.function(get_current_language)) {
    current_language <- get_current_language()
    if (current_language == expected_language) {
      cat("✅ 統一語言狀態驗證成功:", current_language, "\n")
    } else {
      cat("❌ 統一語言狀態不匹配 - 預期:", expected_language, "實際:", current_language, "\n")
    }
  }

  # 檢查全局語言內容
  if (exists("global_lang_content")) {
    current_content <- isolate(global_lang_content())
    if (!is.null(current_content)) {
      actual_language <- current_content$language
      if (actual_language == expected_language) {
        cat("✅ 全局語言內容驗證成功:", actual_language, "\n")
      } else {
        cat("❌ 全局語言內容不匹配 - 預期:", expected_language, "實際:", actual_language, "\n")
      }
    }
  }

  # 檢查模組註冊狀態
  if (exists("module_language_updaters")) {
    registered_count <- length(module_language_updaters)
    cat("📊 已註冊的模組更新器數量:", registered_count, "\n")
    if (registered_count > 0) {
      cat("📋 已註冊模組:", paste(names(module_language_updaters), collapse = ", "), "\n")
    }
  }

  # 生成語言狀態報告
  if (exists("generate_language_status_report") && is.function(generate_language_status_report)) {
    status <- generate_language_status_report()
    return(status$current_language == expected_language)
  }

  cat("⚠️ 無法完整驗證語言切換\n")
  return(FALSE)
}

# ==========================================
# 初始化統一語言管理系統
# ==========================================
initialize_unified_language_system <- function(session) {
  cat("\n🚀 [初始化] 統一語言管理系統...\n")

  # 載入統一內容管理系統
  if (file.exists("utils/unified_content_manager.R")) {
    source("utils/unified_content_manager.R")
    cat("✅ [初始化] 統一內容管理系統已載入\n")
  }

  # 創建語言監控器
  if (exists("create_language_monitor") && is.function(create_language_monitor)) {
    create_language_monitor(session)
    cat("✅ [初始化] 語言監控器已創建\n")
  }

  # 初始化模組更新器註冊
  if (!exists("module_language_updaters")) {
    module_language_updaters <<- list()
    cat("✅ [初始化] 模組更新器註冊表已初始化\n")
  }

  cat("🎉 [初始化] 統一語言管理系統初始化完成\n\n")
}

cat("✅ 已載入統一語言同步架構 - Enhanced Version\n")
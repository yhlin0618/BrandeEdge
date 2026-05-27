# ==========================================
# 配置管理器 - Configuration Manager
# ==========================================
# 負責載入和管理 YAML 配置
# Version: 1.0
# Last Updated: 2025-09-26

library(yaml)
library(jsonlite)

# ==========================================
# 配置載入函數（支援模組化架構）
# ==========================================
load_app_config <- function(config_path = "app_config_advanced.yaml",
                           environment = NULL,
                           use_modular = TRUE) {

  # 檢查是否使用模組化配置
  if (use_modular && dir.exists("config/yaml/insightforge_config")) {
    message("📦 使用模組化 YAML 配置架構")
    cat("🔍 [Config Path Debug] config_path:", config_path, "\n")

    # 從 config_path 提取 app_type（如果有指定）
    app_type <- NULL
    if (!is.null(config_path) && grepl("config/yaml/", config_path)) {
      # 嘗試從路徑提取 app_type
      # 例如: config/yaml/vitalsigns_config/vitalsigns.yaml -> vitalsigns
      path_parts <- strsplit(config_path, "/")[[1]]
      cat("🔍 [Path Parts]:", paste(path_parts, collapse = ", "), "\n")
      config_dir_idx <- which(path_parts == "config") + 2  # yaml 之後的目錄
      cat("🔍 [config_dir_idx]:", config_dir_idx, "\n")
      if (config_dir_idx <= length(path_parts)) {
        config_dir <- path_parts[config_dir_idx]
        cat("🔍 [config_dir]:", config_dir, "\n")
        if (grepl("_config$", config_dir)) {
          app_type <- sub("_config$", "", config_dir)
          cat("🔍 [Extracted app_type]:", app_type, "\n")
          message("📝 從 config_path 提取應用類型: ", app_type)
        }
      }
    }

    config <- load_modular_config(app_type = app_type)
  } else {
    # 檢查配置檔是否存在
    if (!file.exists(config_path)) {
      stop(paste("配置檔不存在:", config_path))
    }

    # 載入單一 YAML 配置（舊架構）
    config <- tryCatch({
      yaml::read_yaml(config_path)
    }, error = function(e) {
      stop(paste("無法載入配置檔:", e$message))
    })
  }

  # 如果指定環境，覆蓋環境設定
  if (!is.null(environment)) {
    config$environment$mode <- environment
  }

  # 處理環境變數替換
  config <- process_env_vars(config)

  # 正規化 modules：支援 YAML list-of-entries 與 named-map 兩種寫法
  # (DRY: load_modular_config 也做同樣事，這裡統一保底)
  config <- normalize_modules_shape(config)

  # 驗證配置
  validate_config(config)

  # 根據環境調整設定
  config <- apply_environment_settings(config)

  class(config) <- c("app_config", class(config))
  return(config)
}

# ==========================================
# 把 config$modules 正規化成命名列表（key = id）
# ==========================================
normalize_modules_shape <- function(config) {
  mods <- config$modules
  if (is.null(mods) || !is.list(mods)) return(config)

  # Already named (either direct named-map or already normalized): 保留
  nms <- names(mods)
  if (!is.null(nms) && all(nzchar(nms))) return(config)

  # List of entries with $id
  named <- list()
  for (m in mods) {
    if (is.list(m) && !is.null(m$id) && nzchar(m$id)) {
      named[[m$id]] <- m
    }
  }
  if (length(named) > 0) {
    config$modules <- named
  }
  config
}

# ==========================================
# 載入模組化配置
# ==========================================
load_modular_config <- function(app_type = NULL) {
  cat("\n🔍 [Config Manager] load_modular_config 開始執行\n")
  cat("   傳入參數 app_type:", ifelse(is.null(app_type), "NULL", app_type), "\n")

  # 如果未指定 app_type，嘗試自動偵測
  if (is.null(app_type)) {
    # 檢查環境變數
    app_type <- Sys.getenv("APP_TYPE", unset = "")
    cat("   從環境變數讀取 APP_TYPE:", ifelse(app_type == "", "空字串", app_type), "\n")

    # 如果環境變數未設定，動態掃描 config/yaml 目錄
    if (app_type == "") {
      cat("   ⚠️  APP_TYPE 環境變數未設定，開始自動偵測...\n")
      config_base <- "config/yaml"

      # 列出所有 *_config 目錄
      config_dirs <- list.dirs(config_base, full.names = FALSE, recursive = FALSE)
      config_dirs <- config_dirs[grepl("_config$", config_dirs)]
      cat("   找到配置目錄:", paste(config_dirs, collapse = ", "), "\n")

      if (length(config_dirs) == 0) {
        stop("找不到任何應用配置目錄 (格式: {app_name}_config)")
      }

      # 遍歷每個配置目錄，找到有效的 yaml 檔案
      found_apps <- c()
      for (dir in config_dirs) {
        # 提取 app 名稱（移除 _config 後綴）
        app_name <- sub("_config$", "", dir)
        yaml_file <- file.path(config_base, dir, paste0(app_name, ".yaml"))

        if (file.exists(yaml_file)) {
          found_apps <- c(found_apps, app_name)
          cat("   ✅ 找到有效配置:", app_name, "\n")
        }
      }

      if (length(found_apps) == 0) {
        stop("找不到任何有效的應用配置檔案")
      }

      # 如果找到多個，使用第一個並發出警告
      if (length(found_apps) > 1) {
        cat("   ⚠️  找到多個應用配置: ", paste(found_apps, collapse = ", "), "\n")
        cat("   ⚠️  將使用第一個: ", found_apps[1], "\n")
        cat("   💡 提示: 請設定 APP_TYPE 環境變數來指定應用類型\n")
        message("⚠️  找到多個應用配置: ", paste(found_apps, collapse = ", "))
        message("   使用: ", found_apps[1])
      }

      app_type <- found_apps[1]
      cat("   📋 自動偵測應用類型: ", app_type, "\n")
      message("📋 自動偵測應用類型: ", app_type)
    } else {
      cat("   ✅ 使用環境變數指定的應用類型: ", app_type, "\n")
    }
  } else {
    cat("   ✅ 使用傳入參數指定的應用類型: ", app_type, "\n")
  }

  # 動態構建配置路徑（遵循命名規範）
  config_dir <- paste0(app_type, "_config")
  main_config_path <- file.path("config/yaml", config_dir, paste0(app_type, ".yaml"))

  cat("   📁 構建配置路徑:\n")
  cat("      config_dir:", config_dir, "\n")
  cat("      main_config_path:", main_config_path, "\n")

  # 確認檔案存在
  if (!file.exists(main_config_path)) {
    cat("   ❌ 配置檔案不存在!\n")
    stop(paste0(
      "找不到主配置檔案: ", main_config_path, "\n",
      "期望的命名規範:\n",
      "  - 目錄: config/yaml/{app_type}_config/\n",
      "  - 檔案: config/yaml/{app_type}_config/{app_type}.yaml"
    ))
  }

  cat("   ✅ 配置檔案存在，開始讀取...\n")
  config <- yaml::read_yaml(main_config_path)
  cat("   ✅ 配置已載入: ", main_config_path, "\n")
  message("✅ 已載入主配置: ", basename(main_config_path))

  # 載入模組配置
  if (!is.null(config$modules)) {
    module_configs <- list()

    for (module in config$modules) {
      if (!is.null(module$config_file)) {
        module_config_path <- file.path("config/yaml", module$config_file)

        if (file.exists(module_config_path)) {
          module_config <- yaml::read_yaml(module_config_path)
          module_configs[[module$id]] <- module_config
          message("  ✅ 已載入模組配置: ", module$config_file)
        } else {
          message("  ⚠️ 找不到模組配置: ", module$config_file)
        }
      }
    }

    # 將模組配置合併到主配置
    config$module_configs <- module_configs
  }

  # 將 modules 陣列轉換成命名列表格式（相容舊架構）
  if (!is.null(config$modules)) {
    modules_list <- list()
    for (module in config$modules) {
      modules_list[[module$id]] <- list(
        id = module$id,
        name = module$name,
        enabled = module$enabled,
        config_file = module$config_file,
        path = if (!is.null(config$module_configs[[module$id]]$module$path)) {
          config$module_configs[[module$id]]$module$path
        } else {
          NULL
        }
      )
    }
    config$modules <- modules_list

    # IMPORTANT: 保留原始的 pages 配置，不要從 modules 重新生成！
    # 原始的 pages 可能包含不在 modules 列表中的頁面（例如 about）
    # 只有當 pages 不存在時才從 modules 生成
    if (is.null(config$pages) || length(config$pages) == 0) {
      cat("   ⚠️  未找到 pages 配置，從 modules 生成...\n")
      config$pages <- list()

      for (module in config$modules) {
        # 建立頁面配置（相容舊架構）
        page <- list(
          id = module$id,
          title = module$name,
          type = "module",
          module = module$id,
          enabled = module$enabled,
          require_auth = TRUE
        )

        # 從模組配置取得詳細資訊
        if (!is.null(config$module_configs[[module$id]])) {
          module_detail <- config$module_configs[[module$id]]

          if (!is.null(module_detail$module$icon)) {
            page$icon <- module_detail$module$icon
          }
          if (!is.null(module_detail$ui$card_status)) {
            page$status <- module_detail$ui$card_status
          }
          if (!is.null(module_detail$module$path)) {
            page$path <- module_detail$module$path
          }
        }

        config$pages <- append(config$pages, list(page))
      }
    } else {
      cat("   ✅ 使用原始 pages 配置 (", length(config$pages), "個頁面)\n")
    }
  }

  return(config)
}

# ==========================================
# 處理環境變數
# ==========================================
process_env_vars <- function(config) {
  # 遞迴處理所有包含 _env 後綴的設定
  process_node <- function(node) {
    if (is.list(node)) {
      for (name in names(node)) {
        if (endsWith(name, "_env")) {
          # 從環境變數讀取實際值
          env_var <- node[[name]]
          actual_value <- Sys.getenv(env_var)

          # 建立新的 key（移除 _env 後綴）
          new_name <- sub("_env$", "", name)

          # 如果環境變數存在，使用其值
          if (nzchar(actual_value)) {
            node[[new_name]] <- actual_value
          }
        } else {
          # 遞迴處理子節點
          node[[name]] <- process_node(node[[name]])
        }
      }
    }
    return(node)
  }

  return(process_node(config))
}

# ==========================================
# 驗證配置
# ==========================================
validate_config <- function(config) {
  # 檢查必要欄位
  required_fields <- c("app_info", "modules", "pages")

  for (field in required_fields) {
    if (!field %in% names(config)) {
      stop(paste("配置缺少必要欄位:", field))
    }
  }

  # 驗證模組依賴
  validate_module_dependencies(config$modules)

  # 驗證頁面配置
  validate_pages(config$pages, config$modules)

  return(TRUE)
}

# ==========================================
# 驗證模組依賴
# ==========================================
validate_module_dependencies <- function(modules) {
  # 如果沒有模組，直接返回
  if (length(modules) == 0) {
    return(TRUE)
  }

  # 安全地取得啟用的模組
  enabled_modules <- character(0)
  if (length(modules) > 0) {
    enabled_list <- sapply(modules, function(m) {
      if (is.null(m$enabled)) FALSE else m$enabled
    })
    # 確保是邏輯向量
    if (is.list(enabled_list)) {
      enabled_list <- unlist(enabled_list)
    }
    if (length(enabled_list) > 0) {
      enabled_modules <- names(modules)[enabled_list]
    }
  }

  for (module_name in enabled_modules) {
    module <- modules[[module_name]]
    if (!is.null(module$dependencies)) {
      for (dep in module$dependencies) {
        if (!dep %in% enabled_modules) {
          warning(paste("模組", module_name, "依賴於未啟用的模組:", dep))
        }
      }
    }
  }

  return(TRUE)
}

# ==========================================
# 驗證頁面配置
# ==========================================
validate_pages <- function(pages, modules) {
  for (page in pages) {
    if (!is.null(page$module)) {
      if (!page$module %in% names(modules) || !modules[[page$module]]$enabled) {
        warning(paste("頁面", page$id, "引用了未啟用的模組:", page$module))
      }
    }

    # 遞迴驗證子頁面
    if (!is.null(page$subpages)) {
      validate_pages(page$subpages, modules)
    }
  }
}

# ==========================================
# 根據環境調整設定
# ==========================================
apply_environment_settings <- function(config) {
  # 確保 environment 存在
  if (is.null(config$environment)) {
    config$environment <- list(mode = "development")
  }

  mode <- config$environment$mode

  # 根據不同環境調整設定
  if (mode == "development") {
    config$environment$debug <- TRUE
    config$environment$log_level <- "DEBUG"
    # 確保欄位存在再設定
    if (!is.null(config$performance)) {
      if (is.null(config$performance$cache)) {
        config$performance$cache <- list()
      }
      config$performance$cache$enabled <- FALSE
    }
    if (!is.null(config$security)) {
      config$security$https_only <- FALSE
    }
    if (!is.null(config$features)) {
      config$features$show_debug_panel <- TRUE
    }

  } else if (mode == "test") {
    config$environment$debug <- TRUE
    config$environment$log_level <- "INFO"
    if (!is.null(config$database) && !is.null(config$database$primary)) {
      config$database$primary$type <- "sqlite"
    }

  } else if (mode == "production") {
    config$environment$debug <- FALSE
    config$environment$log_level <- "WARNING"
    if (!is.null(config$performance) && !is.null(config$performance$cache)) {
      config$performance$cache$enabled <- TRUE
    }
    if (!is.null(config$security)) {
      config$security$https_only <- TRUE
    }
    if (!is.null(config$features)) {
      config$features$show_debug_panel <- FALSE
    }
  }

  return(config)
}

# ==========================================
# 取得模組配置
# ==========================================
get_module_config <- function(config, module_name) {
  if (!module_name %in% names(config$modules)) {
    return(NULL)
  }

  module <- config$modules[[module_name]]

  if (!module$enabled) {
    return(NULL)
  }

  return(module$config)
}

# ==========================================
# 取得啟用的模組
# ==========================================
get_enabled_modules <- function(config) {
  modules <- config$modules

  # 如果沒有模組，返回空字符向量
  if (length(modules) == 0) {
    return(character(0))
  }

  # 取得啟用狀態
  enabled <- sapply(modules, function(m) {
    if (is.null(m$enabled)) FALSE else m$enabled
  })

  # 確保 enabled 是邏輯向量
  if (is.list(enabled)) {
    enabled <- unlist(enabled)
  }

  return(names(modules)[enabled])
}

# ==========================================
# 取得頁面配置
# ==========================================
get_page_config <- function(config, page_id) {
  find_page <- function(pages, target_id) {
    for (page in pages) {
      if (page$id == target_id) {
        return(page)
      }
      # 搜尋子頁面
      if (!is.null(page$subpages)) {
        result <- find_page(page$subpages, target_id)
        if (!is.null(result)) {
          return(result)
        }
      }
    }
    return(NULL)
  }

  return(find_page(config$pages, page_id))
}

# ==========================================
# 檢查功能是否啟用
# ==========================================
is_feature_enabled <- function(config, feature_name) {
  if (!is.null(config$features[[feature_name]])) {
    return(config$features[[feature_name]])
  }
  return(FALSE)
}

# ==========================================
# 取得資料庫連接配置
# ==========================================
get_db_config <- function(config) {
  db_config <- config$database$primary

  # 檢查是否需要使用備用資料庫
  if (db_config$type == "postgresql") {
    # 檢查 PostgreSQL 連接參數
    required_vars <- c("host", "port", "user", "password", "database")
    missing_vars <- sapply(required_vars, function(v) {
      is.null(db_config[[v]]) || nchar(db_config[[v]]) == 0
    })

    if (any(missing_vars)) {
      # 使用備用 SQLite
      message("PostgreSQL 配置不完整，使用備用 SQLite 資料庫")
      db_config <- config$database$fallback
    }
  }

  return(db_config)
}

# ==========================================
# 取得工作流程配置
# ==========================================
get_workflow <- function(config, workflow_name = "default") {
  if (!is.null(config$workflows[[workflow_name]])) {
    return(config$workflows[[workflow_name]])
  }
  return(NULL)
}

# ==========================================
# 配置更新函數
# ==========================================
update_config <- function(config, path, value) {
  # 使用點號分隔的路徑更新配置
  # 例如: "modules.data_upload.config.max_file_size_mb"

  keys <- strsplit(path, "\\.")[[1]]

  # 建立更新函數
  update_recursive <- function(obj, keys, value) {
    if (length(keys) == 1) {
      obj[[keys[1]]] <- value
    } else {
      if (is.null(obj[[keys[1]]])) {
        obj[[keys[1]]] <- list()
      }
      obj[[keys[1]]] <- update_recursive(obj[[keys[1]]], keys[-1], value)
    }
    return(obj)
  }

  config <- update_recursive(config, keys, value)
  return(config)
}

# ==========================================
# 配置序列化（儲存）
# ==========================================
save_config <- function(config, path = "app_config_advanced.yaml") {
  # 移除 class 屬性
  class(config) <- setdiff(class(config), "app_config")

  # 儲存為 YAML
  yaml::write_yaml(config, path)

  message(paste("配置已儲存至:", path))
}

# ==========================================
# 配置比較函數
# ==========================================
compare_configs <- function(config1, config2) {
  # 比較兩個配置的差異
  differences <- list()

  compare_recursive <- function(obj1, obj2, path = "") {
    if (is.list(obj1) && is.list(obj2)) {
      all_keys <- unique(c(names(obj1), names(obj2)))

      for (key in all_keys) {
        new_path <- if (nchar(path) > 0) paste(path, key, sep = ".") else key

        if (key %in% names(obj1) && key %in% names(obj2)) {
          compare_recursive(obj1[[key]], obj2[[key]], new_path)
        } else if (key %in% names(obj1)) {
          differences <<- c(differences, list(list(
            path = new_path,
            type = "removed",
            old_value = obj1[[key]],
            new_value = NULL
          )))
        } else {
          differences <<- c(differences, list(list(
            path = new_path,
            type = "added",
            old_value = NULL,
            new_value = obj2[[key]]
          )))
        }
      }
    } else if (!identical(obj1, obj2)) {
      differences <<- c(differences, list(list(
        path = path,
        type = "modified",
        old_value = obj1,
        new_value = obj2
      )))
    }
  }

  compare_recursive(config1, config2)
  return(differences)
}

# ==========================================
# 配置合併函數
# ==========================================
merge_configs <- function(base_config, override_config) {
  # 深度合併兩個配置（override_config 優先）

  merge_recursive <- function(base, override) {
    if (is.list(base) && is.list(override)) {
      # 合併列表
      result <- base
      for (key in names(override)) {
        if (key %in% names(base)) {
          result[[key]] <- merge_recursive(base[[key]], override[[key]])
        } else {
          result[[key]] <- override[[key]]
        }
      }
      return(result)
    } else {
      # 非列表直接覆蓋
      return(override)
    }
  }

  return(merge_recursive(base_config, override_config))
}

# ==========================================
# 匯出配置摘要
# ==========================================
export_config_summary <- function(config) {
  summary <- list(
    app_name = config$app_info$name,
    version = config$app_info$version,
    environment = config$environment$mode,
    enabled_modules = get_enabled_modules(config),
    total_pages = length(config$pages),
    database_type = config$database$primary$type,
    cache_enabled = config$performance$cache$enabled,
    features = config$features
  )

  class(summary) <- c("config_summary", class(summary))
  return(summary)
}

# ==========================================
# Print method for config_summary
# ==========================================
print.config_summary <- function(x, ...) {
  cat("========== 應用配置摘要 ==========\n")
  cat("應用名稱:", x$app_name, "\n")
  cat("版本:", x$version, "\n")
  cat("環境:", x$environment, "\n")
  cat("資料庫:", x$database_type, "\n")
  cat("快取:", ifelse(x$cache_enabled, "啟用", "停用"), "\n")
  cat("\n啟用的模組:\n")
  for (module in x$enabled_modules) {
    cat("  -", module, "\n")
  }
  cat("\n總頁面數:", x$total_pages, "\n")
  cat("==================================\n")
}
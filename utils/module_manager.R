# ==========================================
# 模組管理器 - Module Manager
# ==========================================
# 負責動態載入和管理 Shiny 模組
# Version: 1.0
# Last Updated: 2025-09-26

# ==========================================
# 模組註冊表（全域變數）
# ==========================================
.module_registry <- new.env(parent = emptyenv())

# ==========================================
# 載入所有啟用的模組
# ==========================================
load_modules <- function(config, session = NULL) {
  modules <- config$modules
  loaded_modules <- list()

  # 依照依賴順序載入模組
  modules_to_load <- get_modules_in_dependency_order(modules)

  for (module_name in modules_to_load) {
    module <- modules[[module_name]]

    if (module$enabled) {
      result <- load_single_module(
        module_name = module_name,
        module_config = module,
        app_config = config,
        session = session
      )

      if (!is.null(result)) {
        loaded_modules[[module_name]] <- result
        message(paste("✓ 載入模組:", module_name))
      } else {
        warning(paste("✗ 無法載入模組:", module_name))
      }
    }
  }

  return(loaded_modules)
}

# ==========================================
# 載入單一模組
# ==========================================
load_single_module <- function(module_name, module_config, app_config, session = NULL, con = NULL) {
  # 使用 path 或 module_path
  module_path <- module_config$path
  if (is.null(module_path)) {
    module_path <- module_config$module_path
  }

  # 檢查模組檔案是否存在
  if (is.null(module_path) || !file.exists(module_path)) {
    warning(paste("模組檔案不存在:", module_path))
    return(NULL)
  }

  # 載入模組檔案
  tryCatch({
    # Source 模組檔案
    source(module_path, local = TRUE)

    # 建立模組資訊
    module_info <- list(
      name = module_name,
      path = module_path,
      config = module_config$config,
      dependencies = module_config$dependencies,
      loaded_at = Sys.time()
    )

    # 尋找 UI 和 Server 函數
    # 試不同的函數命名模式
    possible_ui_names <- c(
      paste0(module_name, "ModuleUI"),
      paste0(module_name, "UI"),
      paste0(module_name, "_ui")
    )

    possible_server_names <- c(
      paste0(module_name, "ModuleServer"),
      paste0(module_name, "Server"),
      paste0(module_name, "_server")
    )

    # 檢查 UI 函數是否存在
    for (ui_name in possible_ui_names) {
      if (exists(ui_name)) {
        module_info$ui <- get(ui_name)
        break
      }
    }

    # 檢查 Server 函數是否存在
    for (server_name in possible_server_names) {
      if (exists(server_name)) {
        module_info$server <- get(server_name)
        break
      }
    }

    # 註冊模組
    register_module(module_name, module_info)

    return(module_info)

  }, error = function(e) {
    warning(paste("載入模組時發生錯誤:", module_name, "-", e$message))
    return(NULL)
  })
}

# ==========================================
# 註冊模組
# ==========================================
register_module <- function(module_name, module_info) {
  .module_registry[[module_name]] <- module_info
}

# ==========================================
# 取得模組
# ==========================================
get_module <- function(module_name) {
  if (module_name %in% names(.module_registry)) {
    return(.module_registry[[module_name]])
  }
  return(NULL)
}

# ==========================================
# 取得所有已載入的模組
# ==========================================
get_loaded_modules <- function() {
  return(as.list(.module_registry))
}

# ==========================================
# 檢查模組是否已載入
# ==========================================
is_module_loaded <- function(module_name) {
  return(module_name %in% names(.module_registry))
}

# ==========================================
# 依照依賴順序排序模組
# ==========================================
get_modules_in_dependency_order <- function(modules) {
  # 建立依賴圖
  graph <- list()
  for (module_name in names(modules)) {
    if (modules[[module_name]]$enabled) {
      deps <- modules[[module_name]]$dependencies
      if (is.null(deps)) deps <- character(0)
      graph[[module_name]] <- deps
    }
  }

  # 拓撲排序
  sorted <- topological_sort(graph)
  return(sorted)
}

# ==========================================
# 拓撲排序（用於依賴管理）
# ==========================================
topological_sort <- function(graph) {
  in_degree <- list()
  for (node in names(graph)) {
    if (!(node %in% names(in_degree))) {
      in_degree[[node]] <- 0
    }
    for (dep in graph[[node]]) {
      if (!(dep %in% names(in_degree))) {
        in_degree[[dep]] <- 0
      }
    }
  }

  for (node in names(graph)) {
    for (dep in graph[[node]]) {
      in_degree[[dep]] <- in_degree[[dep]] + 1
    }
  }

  queue <- names(in_degree)[sapply(in_degree, function(x) x == 0)]
  result <- character(0)

  while (length(queue) > 0) {
    node <- queue[1]
    queue <- queue[-1]
    result <- c(result, node)

    if (node %in% names(graph)) {
      for (dep in graph[[node]]) {
        in_degree[[dep]] <- in_degree[[dep]] - 1
        if (in_degree[[dep]] == 0) {
          queue <- c(queue, dep)
        }
      }
    }
  }

  # 處理循環依賴或獨立節點
  remaining <- setdiff(names(in_degree), result)
  result <- c(result, remaining)

  return(result)
}

# ==========================================
# 建立模組 UI
# ==========================================
create_module_ui <- function(module_name, id, ...) {
  module <- get_module(module_name)

  if (is.null(module)) {
    return(div(class = "alert alert-warning",
              paste("模組未載入:", module_name)))
  }

  if (is.null(module$ui)) {
    return(div(class = "alert alert-info",
              paste("模組沒有 UI 元件:", module_name)))
  }

  # 呼叫模組 UI 函數
  return(module$ui(id = id, config = module$config, ...))
}

# ==========================================
# 呼叫模組 Server
# ==========================================
call_module_server <- function(module_name, id, session, ...) {
  module <- get_module(module_name)

  if (is.null(module)) {
    warning(paste("模組未載入:", module_name))
    return(NULL)
  }

  if (is.null(module$server)) {
    warning(paste("模組沒有 Server 函數:", module_name))
    return(NULL)
  }

  # 呼叫模組 Server 函數
  return(callModule(
    module = module$server,
    id = id,
    config = module$config,
    ...
  ))
}

# ==========================================
# 重新載入模組
# ==========================================
reload_module <- function(module_name, config) {
  # 從註冊表移除舊模組
  if (module_name %in% names(.module_registry)) {
    rm(list = module_name, envir = .module_registry)
  }

  # 重新載入模組
  module_config <- config$modules[[module_name]]
  if (!is.null(module_config) && module_config$enabled) {
    return(load_single_module(
      module_name = module_name,
      module_config = module_config,
      app_config = config
    ))
  }

  return(NULL)
}

# ==========================================
# 卸載模組
# ==========================================
unload_module <- function(module_name) {
  if (module_name %in% names(.module_registry)) {
    rm(list = module_name, envir = .module_registry)
    message(paste("模組已卸載:", module_name))
    return(TRUE)
  }
  return(FALSE)
}

# ==========================================
# 檢查模組依賴
# ==========================================
check_module_dependencies <- function(module_name) {
  module <- get_module(module_name)

  if (is.null(module)) {
    return(list(satisfied = FALSE, missing = module_name))
  }

  missing_deps <- character(0)

  if (!is.null(module$dependencies)) {
    for (dep in module$dependencies) {
      if (!is_module_loaded(dep)) {
        missing_deps <- c(missing_deps, dep)
      }
    }
  }

  return(list(
    satisfied = length(missing_deps) == 0,
    missing = missing_deps
  ))
}

# ==========================================
# 取得模組狀態
# ==========================================
get_module_status <- function() {
  modules <- get_loaded_modules()

  status <- data.frame(
    Module = character(0),
    Status = character(0),
    Dependencies = character(0),
    LoadedAt = character(0),
    stringsAsFactors = FALSE
  )

  for (module_name in names(modules)) {
    module <- modules[[module_name]]
    deps_check <- check_module_dependencies(module_name)

    status <- rbind(status, data.frame(
      Module = module_name,
      Status = ifelse(deps_check$satisfied, "Ready", "Missing Dependencies"),
      Dependencies = paste(module$dependencies, collapse = ", "),
      LoadedAt = format(module$loaded_at, "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    ))
  }

  return(status)
}

# ==========================================
# 建立模組間通訊機制
# ==========================================
create_module_communication <- function() {
  # 建立共享的 reactiveValues
  shared <- reactiveValues(
    data = list(),
    events = list(),
    state = list()
  )

  # 建立通訊函數
  communication <- list(
    # 發送資料
    send = function(from_module, to_module, data) {
      if (is.null(shared$data[[to_module]])) {
        shared$data[[to_module]] <- list()
      }
      shared$data[[to_module]][[from_module]] <- data
      shared$events[[paste0(from_module, "_to_", to_module)]] <- Sys.time()
    },

    # 接收資料
    receive = function(to_module, from_module) {
      if (!is.null(shared$data[[to_module]][[from_module]])) {
        return(shared$data[[to_module]][[from_module]])
      }
      return(NULL)
    },

    # 廣播資料
    broadcast = function(from_module, data) {
      shared$state[[from_module]] <- data
      shared$events[[paste0(from_module, "_broadcast")]] <- Sys.time()
    },

    # 取得廣播資料
    get_broadcast = function(from_module) {
      return(shared$state[[from_module]])
    },

    # 監聽事件
    on_event = function(event_name) {
      return(reactive({
        shared$events[[event_name]]
      }))
    }
  )

  return(communication)
}

# ==========================================
# 模組效能監控
# ==========================================
monitor_module_performance <- function(module_name, operation, duration) {
  # 記錄模組效能資料
  if (!exists(".module_performance", envir = .GlobalEnv)) {
    assign(".module_performance", list(), envir = .GlobalEnv)
  }

  perf_data <- get(".module_performance", envir = .GlobalEnv)

  if (is.null(perf_data[[module_name]])) {
    perf_data[[module_name]] <- list()
  }

  perf_data[[module_name]][[operation]] <- c(
    perf_data[[module_name]][[operation]],
    duration
  )

  assign(".module_performance", perf_data, envir = .GlobalEnv)
}

# ==========================================
# 取得模組效能報告
# ==========================================
get_module_performance_report <- function() {
  if (!exists(".module_performance", envir = .GlobalEnv)) {
    return(NULL)
  }

  perf_data <- get(".module_performance", envir = .GlobalEnv)
  report <- list()

  for (module_name in names(perf_data)) {
    module_stats <- list()
    for (operation in names(perf_data[[module_name]])) {
      times <- perf_data[[module_name]][[operation]]
      module_stats[[operation]] <- list(
        count = length(times),
        mean = mean(times),
        median = median(times),
        max = max(times),
        min = min(times)
      )
    }
    report[[module_name]] <- module_stats
  }

  return(report)
}
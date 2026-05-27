# ============================================
# Server 邏輯引擎
# ============================================
# 根據 YAML 配置執行 Server 邏輯
# Version: 2.0
# Last Updated: 2025-09-26

library(shiny)
library(dplyr)
library(purrr)

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ============================================
# Server 引擎主類別
# ============================================
ServerEngine <- R6::R6Class("ServerEngine",
  public = list(
    # 屬性
    config = NULL,
    session = NULL,
    reactive_values = list(),
    observers = list(),
    event_handlers = list(),
    modules = list(),
    workflows = list(),

    # ============================================
    # 初始化
    # ============================================
    initialize = function(config, session) {
      self$config <- config
      self$session <- session

      # 執行初始化動作
      if (!is.null(config$server_logic$initialization)) {
        self$execute_initialization(config$server_logic$initialization)
      }

      # 註冊事件處理器
      if (!is.null(config$server_logic$event_handlers)) {
        self$register_event_handlers(config$server_logic$event_handlers)
      }

      # 建立觀察器
      if (!is.null(config$server_logic$observers)) {
        self$create_observers(config$server_logic$observers)
      }

      # 設定模組通訊
      if (!is.null(config$server_logic$module_communication)) {
        self$setup_module_communication(config$server_logic$module_communication)
      }
    },

    # ============================================
    # 執行初始化動作
    # ============================================
    execute_initialization = function(init_actions) {
      for (action in init_actions) {
        switch(action$action,
          "create_reactive_values" = {
            self$reactive_values[[action$id]] <- do.call(
              reactiveValues,
              action$values
            )
          },

          "set_options" = {
            do.call(options, action$options)
          },

          "load_data" = {
            self$load_initial_data(action)
          },

          "setup_environment" = {
            self$setup_environment(action)
          }
        )
      }
    },

    # ============================================
    # 註冊事件處理器
    # ============================================
    register_event_handlers = function(handlers) {
      for (handler in handlers) {
        handler_id <- paste0(handler$event, "_", handler$source, "_", handler$id)
        self$event_handlers[[handler_id]] <- handler

        # 根據事件類型建立適當的觀察器
        self$create_event_observer(handler)
      }
    },

    # ============================================
    # 建立事件觀察器
    # ============================================
    create_event_observer = function(handler) {
      if (handler$event == "input") {
        # 監聽 Shiny input
        observeEvent(self$session$input[[handler$id]], {
          self$handle_event(handler, self$session$input[[handler$id]])
        })

      } else if (handler$event == "module_output") {
        # 監聽模組輸出
        module_output <- self$get_module_output(handler$source, handler$output)
        if (!is.null(module_output)) {
          observeEvent(module_output(), {
            self$handle_event(handler, module_output())
          })
        }

      } else if (handler$event == "timer") {
        # 定時器事件
        observe({
          invalidateLater(handler$interval)
          self$handle_event(handler, Sys.time())
        })

      } else if (handler$event == "reactive") {
        # 監聽 reactive 值變化
        target <- self$resolve_path(handler$watch)
        if (!is.null(target)) {
          observeEvent(target(), {
            self$handle_event(handler, target())
          })
        }
      }
    },

    # ============================================
    # 處理事件
    # ============================================
    handle_event = function(handler, event_data) {
      # 檢查條件
      if (!is.null(handler$condition)) {
        if (!self$evaluate_condition(handler$condition, event_data)) {
          return()
        }
      }

      # 執行動作
      for (action in handler$actions) {
        self$execute_action(action, event_data)
      }
    },

    # ============================================
    # 執行動作
    # ============================================
    execute_action = function(action, event_data = NULL) {
      switch(action$type,
        # 更新 reactive 值
        "update_reactive" = {
          target <- self$resolve_path(action$target)
          value <- self$resolve_value(action$value, event_data)
          if (!is.null(target)) {
            target <- value
          }
        },

        # 顯示通知
        "show_notification" = {
          message <- self$interpolate_string(action$message, event_data)
          notification_type <- action$notification_type %||% action$type %||% "default"
          # 確保 type 是有效值
          valid_types <- c("default", "message", "warning", "error", "success", "info")
          if (!notification_type %in% valid_types) {
            notification_type <- "default"
          }
          showNotification(
            message,
            type = notification_type,
            duration = action$duration %||% 5
          )
        },

        # 更新 UI
        "update_ui" = {
          self$update_ui_element(action$element, action$value, event_data)
        },

        # 廣播事件
        "broadcast" = {
          self$broadcast_event(action$channel, action$data, event_data)
        },

        # 觸發模組動作
        "trigger_module" = {
          self$trigger_module_action(action$module, action$action, event_data)
        },

        # 檢查權限
        "check_permission" = {
          self$check_permission(action, event_data)
        },

        # 工作流程控制
        "workflow" = {
          self$handle_workflow(action, event_data)
        },

        # 儲存到資料庫
        "save_to_database" = {
          self$save_to_database(action, event_data)
        },

        # 執行自訂函數
        "execute_function" = {
          self$execute_custom_function(action, event_data)
        },

        # 條件判斷
        "evaluate" = {
          self$evaluate_expression(action, event_data)
        }
      )
    },

    # ============================================
    # 建立觀察器
    # ============================================
    create_observers = function(observer_configs) {
      for (obs_config in observer_configs) {
        if (obs_config$type == "timer") {
          # 定時器觀察器
          observe({
            invalidateLater(obs_config$interval)

            # 檢查條件
            if (!is.null(obs_config$condition)) {
              if (!self$evaluate_condition(obs_config$condition)) {
                return()
              }
            }

            # 執行動作
            for (action in obs_config$actions) {
              self$execute_action(action)
            }
          })

        } else if (obs_config$type == "reactive") {
          # Reactive 觀察器
          watch_expr <- obs_config$watch

          # 建立觀察器
          observe({
            # 在反應式環境中解析值
            if (grepl("\\$", watch_expr)) {
              parts <- strsplit(watch_expr, "\\$")[[1]]
              if (length(parts) == 2 && parts[1] %in% names(self$reactive_values)) {
                target_value <- self$reactive_values[[parts[1]]][[parts[2]]]

                # 執行動作
                for (action in obs_config$actions) {
                  self$execute_action(action, target_value)
                }
              }
            }
          })
        }
      }
    },

    # ============================================
    # 設定模組通訊
    # ============================================
    setup_module_communication = function(comm_config) {
      # 設定通訊管道
      if (!is.null(comm_config$channels)) {
        for (channel in comm_config$channels) {
          self$create_communication_channel(channel)
        }
      }

      # 設定資料流
      if (!is.null(comm_config$data_flows)) {
        for (flow in comm_config$data_flows) {
          self$create_data_flow(flow)
        }
      }
    },

    # ============================================
    # 建立通訊管道
    # ============================================
    create_communication_channel = function(channel) {
      # 建立 reactive 管道
      channel_reactive <- reactiveVal(NULL)

      # 監聽來源模組
      for (from_module in channel$from) {
        module_output <- self$get_module_output(from_module, "data")
        if (!is.null(module_output)) {
          observeEvent(module_output(), {
            channel_reactive(module_output())

            # 通知目標模組
            for (to_module in channel$to) {
              self$send_to_module(to_module, module_output())
            }
          })
        }
      }
    },

    # ============================================
    # 建立資料流
    # ============================================
    create_data_flow = function(flow) {
      # 解析來源
      source_parts <- strsplit(flow$source, "\\.")[[1]]
      source_module <- source_parts[1]
      source_output <- source_parts[2]

      # 取得來源資料
      source_data <- self$get_module_output(source_module, source_output)

      if (!is.null(source_data)) {
        observeEvent(source_data(), {
          data <- source_data()

          # 檢查條件
          if (!is.null(flow$condition)) {
            if (!self$evaluate_condition(flow$condition, data)) {
              return()
            }
          }

          # 轉換資料
          if (!is.null(flow$transform)) {
            transform_func <- eval(parse(text = flow$transform))
            data <- transform_func(data)
          }

          # 發送到目標
          for (target in flow$targets) {
            target_parts <- strsplit(target, "\\.")[[1]]
            target_module <- target_parts[1]
            target_input <- target_parts[2]

            self$send_to_module_input(target_module, target_input, data)
          }
        })
      }
    },

    # ============================================
    # 工具函數
    # ============================================
    resolve_path = function(path) {
      # 解析路徑字串，如 "shared_data$uploaded_data"
      parts <- strsplit(path, "\\$")[[1]]

      if (length(parts) == 2) {
        if (parts[1] %in% names(self$reactive_values)) {
          return(self$reactive_values[[parts[1]]][[parts[2]]])
        }
      }

      # 嘗試從 input 解析
      if (startsWith(path, "input$")) {
        input_name <- substring(path, 7)
        return(self$session$input[[input_name]])
      }

      return(NULL)
    },

    resolve_value = function(value_str, event_data = NULL) {
      # 解析值字串，支援變數替換
      if (is.character(value_str) && grepl("\\$\\{", value_str)) {
        # 替換變數
        value_str <- self$interpolate_string(value_str, event_data)
      }

      # 嘗試評估為 R 表達式
      tryCatch(
        eval(parse(text = value_str)),
        error = function(e) value_str
      )
    },

    interpolate_string = function(str, data = NULL) {
      # 字串插值，替換 ${variable} 格式的變數
      if (is.null(data)) return(str)

      # 如果 data 是列表或資料框，處理屬性
      if (is.list(data) || is.data.frame(data)) {
        # 替換 event.data.xxx 的屬性
        for (name in names(data)) {
          pattern <- paste0("\\$\\{event\\.data\\.", name, "\\}")
          replacement <- as.character(data[[name]][1])  # 只取第一個元素
          str <- gsub(pattern, replacement, str, fixed = FALSE)
        }

        # 替換 event.data（整個物件）
        if (grepl("\\$\\{event\\.data\\}", str)) {
          # 如果有 username，使用它
          if ("username" %in% names(data)) {
            str <- gsub("\\$\\{event\\.data\\}", as.character(data$username[1]), str)
          } else {
            str <- gsub("\\$\\{event\\.data\\}", "使用者", str)
          }
        }
      } else {
        # 如果 data 不是列表，直接替換
        str <- gsub("\\$\\{event\\.data\\}", as.character(data)[1], str)
      }

      return(str)
    },

    evaluate_condition = function(condition, data = NULL) {
      # 評估條件表達式
      condition_str <- self$interpolate_string(condition, data)

      tryCatch({
        eval(parse(text = condition_str))
      }, error = function(e) {
        FALSE
      })
    },

    update_ui_element = function(element_id, value, data = NULL) {
      # 更新 UI 元素
      value <- self$resolve_value(value, data)

      if (element_id == "sidebar_menu") {
        updateTabItems(self$session, "sidebar_menu", value)
      } else {
        # 通用更新方法
        updateTextInput(self$session, element_id, value = value)
      }
    },

    get_module_output = function(module_name, output_name) {
      # 取得模組輸出
      if (module_name %in% names(self$modules)) {
        module <- self$modules[[module_name]]
        if (output_name %in% names(module$outputs)) {
          return(module$outputs[[output_name]])
        }
      }
      return(NULL)
    },

    send_to_module = function(module_name, data) {
      # 發送資料到模組
      if (module_name %in% names(self$modules)) {
        module <- self$modules[[module_name]]
        if (!is.null(module$receive_data)) {
          module$receive_data(data)
        }
      }
    },

    send_to_module_input = function(module_name, input_name, data) {
      # 發送資料到模組的特定輸入
      if (module_name %in% names(self$modules)) {
        module <- self$modules[[module_name]]
        if (!is.null(module$inputs[[input_name]])) {
          module$inputs[[input_name]](data)
        }
      }
    },

    trigger_module_action = function(module_name, action, data = NULL) {
      # 觸發模組動作
      if (module_name %in% names(self$modules)) {
        module <- self$modules[[module_name]]
        if (!is.null(module[[action]])) {
          module[[action]](data)
        }
      }
    },

    broadcast_event = function(channel, data, event_data = NULL) {
      # 廣播事件到管道
      resolved_data <- self$resolve_value(data, event_data)
      # 實作廣播邏輯
    },

    check_permission = function(action, event_data) {
      # 檢查權限
      page <- self$resolve_value(action$page, event_data)
      # 實作權限檢查邏輯

      # 如果失敗，執行 on_fail 動作
      if (!is.null(action$on_fail)) {
        for (fail_action in action$on_fail) {
          self$execute_action(fail_action, event_data)
        }
      }
    },

    handle_workflow = function(action, event_data) {
      # 處理工作流程
      workflow_name <- action$workflow
      workflow_action <- action$action

      if (workflow_name %in% names(self$workflows)) {
        workflow <- self$workflows[[workflow_name]]

        if (workflow_action == "next") {
          workflow[["next"]]()
        } else if (workflow_action == "previous") {
          workflow$previous()
        }
      }
    },

    save_to_database = function(action, event_data) {
      # 儲存到資料庫
      table <- action$table
      data <- self$resolve_value(action$data, event_data)
      # 實作資料庫儲存邏輯
    },

    execute_custom_function = function(action, event_data) {
      # 執行自訂函數
      func_name <- action[["function"]]
      params <- lapply(action$params, function(p) {
        self$resolve_value(p, event_data)
      })

      if (exists(func_name)) {
        func <- get(func_name)
        do.call(func, params)
      }
    },

    evaluate_expression = function(action, event_data) {
      # 評估表達式
      expr <- self$interpolate_string(action$expression, event_data)
      result <- eval(parse(text = expr))

      # 根據結果執行動作
      if (isFALSE(result$allowed) && !is.null(action$on_false)) {
        for (false_action in action$on_false) {
          self$execute_action(false_action, list(result = result))
        }
      } else if (isTRUE(result$allowed) && !is.null(action$on_true)) {
        for (true_action in action$on_true) {
          self$execute_action(true_action, list(result = result))
        }
      }
    },

    # ============================================
    # 註冊模組
    # ============================================
    register_module = function(module_name, module_instance) {
      self$modules[[module_name]] <- module_instance
    },

    # ============================================
    # 註冊工作流程
    # ============================================
    register_workflow = function(workflow_name, workflow_instance) {
      self$workflows[[workflow_name]] <- workflow_instance
    },

    # ============================================
    # 登入狀態管理
    # ============================================
    setup_login_management = function() {
      # 建立登入狀態的 reactive value
      if (!"shared_state" %in% names(self$reactive_values)) {
        self$reactive_values$shared_state <- reactiveValues(
          user_info = NULL,
          logged_in = FALSE
        )
      }

      # 設定 output$user_logged_in
      self$session$output$user_logged_in <- reactive({
        !is.null(self$reactive_values$shared_state$user_info)
      })

      # 確保在 UI 中可見
      outputOptions(self$session$output, "user_logged_in", suspendWhenHidden = FALSE)

      return(self$reactive_values$shared_state)
    },

    # ============================================
    # 更新登入狀態
    # ============================================
    update_login_status = function(user_info) {
      if (!"shared_state" %in% names(self$reactive_values)) {
        self$setup_login_management()
      }

      self$reactive_values$shared_state$user_info <- user_info
      self$reactive_values$shared_state$logged_in <- !is.null(user_info)

      # 觸發登入成功事件
      if (!is.null(user_info)) {
        self$trigger_event("login_success", user_info)
      }
    },

    # ============================================
    # 觸發事件 (新方法)
    # ============================================
    trigger_event = function(event_name, data = NULL) {
      # 查找對應的事件處理器
      for (handler_name in names(self$event_handlers)) {
        handler <- self$event_handlers[[handler_name]]
        if (!is.null(handler$event) && handler$event == event_name) {
          self$handle_event(handler, data)
        }
      }
    }
  )
)
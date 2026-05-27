# ==========================================
# 工作流程工廠函數
# ==========================================
# 建立工作流程物件
# Version: 1.0
# Last Updated: 2025-09-26

# ==========================================
# 建立工作流程
# ==========================================
create_workflow <- function(workflow_config, session) {
  # 建立 reactive value 來追蹤當前步驟
  current_step_val <- reactiveVal(1)

  # 建立工作流程物件
  workflow <- new.env(parent = emptyenv())

  # 設定屬性
  workflow$name <- workflow_config$name
  workflow$type <- workflow_config$type
  workflow$steps <- workflow_config$steps
  workflow$current_step <- current_step_val

  # 定義 next 方法
  workflow[["next"]] <- function() {
    current <- workflow$current_step()
    if (current < length(workflow$steps)) {
      # 驗證當前步驟
      step <- workflow$steps[[current]]
      if (!is.null(step$validation)) {
        for (check in step$validation) {
          condition_met <- tryCatch({
            eval(parse(text = check$check))
          }, error = function(e) FALSE)

          if (!condition_met) {
            showNotification(
              check$message %||% "驗證失敗",
              type = "warning"
            )
            return(invisible(NULL))
          }
        }
      }

      # 移至下一步
      workflow$current_step(current + 1)
      next_step <- workflow$steps[[current + 1]]

      # 更新頁面
      if (!is.null(next_step$page)) {
        updateTabItems(session, "sidebar_menu", next_step$page)
      }

      # 觸發自動動作
      if (!is.null(next_step$auto_trigger) && next_step$auto_trigger) {
        # 可以在這裡觸發自動動作
      }

      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  # 定義 previous 方法
  workflow$previous <- function() {
    current <- workflow$current_step()
    if (current > 1) {
      workflow$current_step(current - 1)
      prev_step <- workflow$steps[[current - 1]]

      # 更新頁面
      if (!is.null(prev_step$page)) {
        updateTabItems(session, "sidebar_menu", prev_step$page)
      }

      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  # 定義 reset 方法
  workflow$reset <- function() {
    workflow$current_step(1)
    if (length(workflow$steps) > 0) {
      first_step <- workflow$steps[[1]]
      if (!is.null(first_step$page)) {
        updateTabItems(session, "sidebar_menu", first_step$page)
      }
    }
    return(invisible(TRUE))
  }

  # 定義 goto 方法（跳至特定步驟）
  workflow$goto <- function(step_id) {
    # 找到對應的步驟索引
    step_index <- which(sapply(workflow$steps, function(s) s$id == step_id))

    if (length(step_index) > 0) {
      workflow$current_step(step_index[1])
      target_step <- workflow$steps[[step_index[1]]]

      if (!is.null(target_step$page)) {
        updateTabItems(session, "sidebar_menu", target_step$page)
      }
      return(invisible(TRUE))
    }
    return(invisible(FALSE))
  }

  # 定義 get_current_step 方法
  workflow$get_current_step <- function() {
    current <- workflow$current_step()
    if (current <= length(workflow$steps)) {
      return(workflow$steps[[current]])
    }
    return(NULL)
  }

  # 定義 is_complete 方法
  workflow$is_complete <- function() {
    return(workflow$current_step() >= length(workflow$steps))
  }

  # 定義 get_progress 方法
  workflow$get_progress <- function() {
    return(list(
      current = workflow$current_step(),
      total = length(workflow$steps),
      percentage = round((workflow$current_step() / length(workflow$steps)) * 100)
    ))
  }

  # 定義 can_proceed 方法（檢查是否可以繼續）
  workflow$can_proceed <- function() {
    current <- workflow$current_step()
    if (current <= length(workflow$steps)) {
      step <- workflow$steps[[current]]
      if (!is.null(step$next_condition)) {
        return(tryCatch({
          eval(parse(text = step$next_condition))
        }, error = function(e) FALSE))
      }
    }
    return(TRUE)
  }

  # 將環境轉換為 list-like 物件
  class(workflow) <- c("workflow", "environment")

  return(workflow)
}

# ==========================================
# 工作流程管理器
# ==========================================
WorkflowManager <- R6::R6Class("WorkflowManager",
  public = list(
    workflows = list(),
    active_workflow = NULL,

    # 註冊工作流程
    register = function(name, workflow) {
      self$workflows[[name]] <- workflow
      if (is.null(self$active_workflow)) {
        self$active_workflow <- name
      }
    },

    # 取得工作流程
    get = function(name = NULL) {
      if (is.null(name)) {
        name <- self$active_workflow
      }
      return(self$workflows[[name]])
    },

    # 設定作用中的工作流程
    set_active = function(name) {
      if (name %in% names(self$workflows)) {
        self$active_workflow <- name
        return(TRUE)
      }
      return(FALSE)
    },

    # 執行下一步
    next_step = function(workflow_name = NULL) {
      workflow <- self$get(workflow_name)
      if (!is.null(workflow)) {
        return(workflow[["next"]]())
      }
      return(FALSE)
    },

    # 執行上一步
    previous = function(workflow_name = NULL) {
      workflow <- self$get(workflow_name)
      if (!is.null(workflow)) {
        return(workflow$previous())
      }
      return(FALSE)
    },

    # 重設工作流程
    reset = function(workflow_name = NULL) {
      workflow <- self$get(workflow_name)
      if (!is.null(workflow)) {
        return(workflow$reset())
      }
      return(FALSE)
    },

    # 取得所有工作流程狀態
    get_all_status = function() {
      status <- list()
      for (name in names(self$workflows)) {
        workflow <- self$workflows[[name]]
        status[[name]] <- workflow$get_progress()
      }
      return(status)
    }
  )
)

# ==========================================
# Helper 函數
# ==========================================

# Null coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
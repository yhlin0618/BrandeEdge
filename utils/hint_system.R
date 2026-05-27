# ============================================================================
# InsightForge Hint System - 提示系統功能
# 基於 BrandEdge 架構優化
# ============================================================================

library(shiny)
library(bs4Dash)
library(yaml)

# ── 語言代碼轉目錄名稱 (動態從 YAML 讀取) ───────────────────────────────────
get_language_dir <- function(language_code) {
  tryCatch({
    lang_config <- yaml::read_yaml("config/languages.yaml")
    for (lang in lang_config$supported_languages) {
      if (lang$code == language_code && lang$enabled) {
        return(lang$dir)
      }
    }
    fallback_code <- lang_config$fallback$fallback_language
    for (lang in lang_config$supported_languages) {
      if (lang$code == fallback_code) {
        return(lang$dir)
      }
    }
    return("chinese")
  }, error = function(e) {
    warning("Failed to load language config: ", e$message)
    return("chinese")
  })
}

# ── 載入提示資料 (支援 Reactive) ────────────────────────────────────────────
# Reactive 版本 - 用於動態語言切換
load_hints_reactive <- function(language) {
  reactive({
    lang <- if (is.reactive(language)) language() else language
    cat("🔄 [Hints Reactive] 載入語言:", lang, "\n")
    load_hints(language = lang)
  })
}

# ── 統一模組 Hint 載入函數 ──────────────────────────────────────────────────
# 確保所有模組使用統一的 hint 載入機制
load_module_hints <- function(current_language, module_name = "unknown") {
  hints_df <- NULL
  if (exists("load_hints") && is.function(load_hints)) {
    tryCatch({
      hints_df <- load_hints(language = current_language)
      cat("✅ [", module_name, "] 成功載入hints語言:", current_language, " - 提示數量:", nrow(hints_df), "\n")
    }, error = function(e) {
      cat("❌ [", module_name, "] 載入hints失敗:", e$message, "\n")
      hints_df <- NULL
    })
  } else {
    cat("⚠️ [", module_name, "] load_hints 函數不存在\n")
  }
  return(hints_df)
}

# ── 載入提示資料 (基礎版本) ────────────────────────────────────────────────
load_hints <- function(hint_file = NULL, language = "zh_TW", app_name = "insightforge") {
  # 根據語言決定檔案路徑
  if (is.null(hint_file)) {
    # 動態從 YAML 配置獲取目錄名稱
    lang_dir <- get_language_dir(language)
    hint_file <- file.path("database/content", lang_dir, "general", app_name, "hint.csv")
  }

  if (file.exists(hint_file)) {
    hints <- read.csv(hint_file, stringsAsFactors = FALSE, encoding = "UTF-8")
    cat("✅ [Hints] 載入", nrow(hints), "條 (", language, ")\n")
    return(hints)
  } else {
    warning("Hint file not found: ", hint_file)
    return(data.frame(
      concept_name = character(),
      var_id = character(),
      description = character(),
      stringsAsFactors = FALSE
    ))
  }
}

# ── 取得特定提示 (支援 Reactive) ────────────────────────────────────────────
get_hint <- function(var_id, hints_df = NULL, language = "zh_TW") {
  if (is.null(hints_df)) {
    hints_df <- load_hints(language = language)
  }

  # 如果 hints_df 是 reactive，取其值
  if (is.reactive(hints_df)) {
    hints_df <- hints_df()
  }

  hint_row <- hints_df[hints_df$var_id == var_id, ]

  if (nrow(hint_row) > 0) {
    return(hint_row$description[1])
  } else {
    return(NULL)
  }
}

# ── 為 UI 元素添加提示 (支援 Reactive) ──────────────────────────────────
add_hint <- function(ui_element, var_id, hints_df = NULL, enable_hints = TRUE, language = "zh_TW") {
  if (!enable_hints) {
    return(ui_element)
  }

  # 如果 hints_df 是 reactive，取其值
  if (is.reactive(hints_df)) {
    hints_df <- hints_df()
  }

  hint_text <- get_hint(var_id, hints_df, language = language)

  if (!is.null(hint_text)) {
    # 使用 bs4Dash 的 tooltip 功能
    return(
      bs4Dash::tooltip(
        tag = ui_element,
        title = hint_text,
        placement = "top"
      )
    )
  } else {
    return(ui_element)
  }
}

# ── 為 Shiny UI 元素添加提示 (兼容模式) ────────────────────────────────────
add_hint_bs4 <- function(ui_element, var_id, hints_df = NULL, enable_hints = TRUE, language = "zh_TW") {
  if (!enable_hints) {
    return(ui_element)
  }

  hint_text <- get_hint(var_id, hints_df, language = language)
  
  if (!is.null(hint_text)) {
    # 為元素添加 data attributes 和 title
    return(
      tags$div(
        ui_element,
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        title = hint_text,
        style = "display: inline-block;"
      )
    )
  } else {
    return(ui_element)
  }
}

# ── 添加信息圖標提示 ────────────────────────────────────────────────────────
add_info_icon <- function(var_id, hints_df = NULL, size = "sm", language = "zh_TW") {
  hint_text <- get_hint(var_id, hints_df, language = language)
  
  if (!is.null(hint_text)) {
    icon_size <- switch(size,
      "xs" = "12px",
      "sm" = "14px",
      "md" = "16px",
      "lg" = "18px",
      "14px"
    )
    
    return(
      tags$span(
        style = paste0("margin-left: 5px; cursor: help; font-size: ", icon_size, ";"),
        `data-toggle` = "tooltip",
        `data-placement` = "top",
        title = hint_text,
        icon("info-circle")
      )
    )
  } else {
    return(NULL)
  }
}

# ── 初始化提示系統 JavaScript ───────────────────────────────────────────────
init_hint_system <- function() {
  tags$script(HTML("
    $(document).ready(function(){
      // 初始化 Bootstrap tooltips
      $('[data-toggle=\"tooltip\"]').tooltip({
        trigger: 'hover',
        container: 'body',
        boundary: 'window',
        animation: true,
        delay: { show: 500, hide: 100 }
      });
      
      // 當新元素加入時重新初始化
      Shiny.addCustomMessageHandler('init_tooltips', function(message) {
        setTimeout(function() {
          $('[data-toggle=\"tooltip\"]').tooltip({
            trigger: 'hover',
            container: 'body',
            boundary: 'window',
            animation: true,
            delay: { show: 500, hide: 100 }
          });
        }, 100);
      });
      
      // 監聽 tab 切換事件，重新初始化 tooltips
      $(document).on('shown.bs.tab', function(e) {
        setTimeout(function() {
          $('[data-toggle=\"tooltip\"]').tooltip('dispose');
          $('[data-toggle=\"tooltip\"]').tooltip({
            trigger: 'hover',
            container: 'body',
            boundary: 'window'
          });
        }, 100);
      });
    });
  "))
}

# ── 批次添加提示 ────────────────────────────────────────────────────────────
add_hints_batch <- function(ui_list, var_ids, hints_df = NULL, enable_hints = TRUE, language = "zh_TW") {
  if (!enable_hints || length(ui_list) != length(var_ids)) {
    return(ui_list)
  }

  Map(function(ui, id) {
    add_hint(ui, id, hints_df, enable_hints, language = language)
  }, ui_list, var_ids)
}

# ── 創建帶提示的標籤 ────────────────────────────────────────────────────────
create_label_with_hint <- function(label_text, var_id, hints_df = NULL, language = "zh_TW") {
  hint_icon <- add_info_icon(var_id, hints_df, language = language)
  
  if (!is.null(hint_icon)) {
    return(
      tags$span(
        label_text,
        hint_icon
      )
    )
  } else {
    return(label_text)
  }
}

# ── 更新提示內容（動態） ─────────────────────────────────────────────────────
update_hint <- function(session, selector, new_hint) {
  session$sendCustomMessage(
    type = "update_tooltip",
    message = list(
      selector = selector,
      hint = new_hint
    )
  )
}

# ── JavaScript 處理動態更新 ──────────────────────────────────────────────────
hint_update_js <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('update_tooltip', function(data) {
      $(data.selector).attr('title', data.hint)
                     .attr('data-original-title', data.hint)
                     .tooltip('dispose')
                     .tooltip({
                       trigger: 'hover',
                       container: 'body',
                       boundary: 'window'
                     });
    });
  "))
}

# ── 匯出函數列表 ────────────────────────────────────────────────────────────
# 提供給應用程式使用的公開函數
hint_system_exports <- list(
  load_hints = load_hints,
  load_hints_reactive = load_hints_reactive,  # 新增 reactive 版本
  get_hint = get_hint,
  add_hint = add_hint,
  add_hint_bs4 = add_hint_bs4,
  add_info_icon = add_info_icon,
  init_hint_system = init_hint_system,
  add_hints_batch = add_hints_batch,
  create_label_with_hint = create_label_with_hint,
  update_hint = update_hint,
  hint_update_js = hint_update_js
)
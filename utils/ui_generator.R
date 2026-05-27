# ==========================================
# UI 生成器 - UI Generator
# ==========================================
# 根據 YAML 配置動態生成 Shiny UI
# Version: 1.1
# Last Updated: 2025-01-20

library(shiny)
library(bs4Dash)
library(shinyjs)

# ==========================================
# 註冊 global_scripts 靜態資源路徑
# ==========================================
# 讓 Shiny 可以存取 global_scripts 中的資源（icons, CSS 等）
global_scripts_path <- "scripts/global_scripts"
if (dir.exists(global_scripts_path)) {
  # 註冊整個 global_scripts 目錄，可存取子目錄如 24_assets/, 19_CSS/
  shiny::addResourcePath("global_assets", global_scripts_path)
}

# ==========================================
# 生成完整的應用程式 UI
# ==========================================
generate_app_ui <- function(config, module_manager = NULL) {
  # 檢查是否有登入模組（支援 named map 與 list of id:... 兩種 YAML 格式）
  mods <- config$modules
  has_login_module <- FALSE
  if (is.list(mods)) {
    # Case A: named map — modules: { login: { enabled: true, ... } }
    if (!is.null(mods$login)) {
      has_login_module <- isTRUE(mods$login$enabled)
    } else {
      # Case B: YAML list of entries — modules: [ { id: login, enabled: true } ]
      login_entry <- Filter(function(m) identical(m$id, "login"), mods)
      if (length(login_entry) > 0) {
        has_login_module <- isTRUE(login_entry[[1]]$enabled)
      }
    }
  }

  if (has_login_module) {
    return(generate_conditional_login_ui(config, module_manager))
  } else {
    # 根據框架選擇 - 預設使用 bs4Dash
    framework <- config$ui$theme$framework %||% "bs4Dash"
    if (identical(framework, "bs4Dash")) {
      return(generate_bs4dash_ui(config, module_manager))
    } else {
      return(generate_default_ui(config, module_manager))
    }
  }
}

# ==========================================
# 生成條件式登入 UI (登入模組模式)
# ==========================================
generate_conditional_login_ui <- function(config, module_manager = NULL) {
  # 載入 login 模組檔案
  login_config <- config$modules$login
  login_module_path <- login_config$path
  if (is.null(login_module_path)) {
    login_module_path <- "scripts/global_scripts/10_rshinyapp_components/login/login_module.R"
  }

  if (file.exists(login_module_path)) {
    source(login_module_path, local = FALSE)
  }

  # 生成主應用 UI
  main_ui <- if (config$ui$theme$framework == "bs4Dash") {
    generate_bs4dash_ui(config, module_manager)
  } else {
    generate_default_ui(config, module_manager)
  }

  # 返回條件式 UI
  fluidPage(
    useShinyjs(),

    # 資源設定
    tags$head(
      # Font Awesome icons
      tags$link(rel = "stylesheet", type = "text/css",
                href = "https://use.fontawesome.com/releases/v5.15.4/css/all.css"),
      # Modern Login Page CSS (from global_scripts/19_CSS/login.css)
      tags$link(rel = "stylesheet", type = "text/css",
                href = "global_assets/19_CSS/login.css")
    ),

    # 登入頁面 (當 user_logged_in == false 時顯示)
    conditionalPanel(
      condition = "output.user_logged_in == false",
      div(
        class = "login-gradient",
        # 使用 Supabase 登入 UI（全面取代舊版 bcrypt 登入）
        if (exists("loginSupabaseUI")) {
          cat("🔐 [UI Generator] 使用 Supabase 登入 UI\n")

          # 取得 app title 和 subtitle
          app_title <- tryCatch({
            if (exists("global_language_content_static", envir = .GlobalEnv)) {
              lang_content <- get("global_language_content_static", envir = .GlobalEnv)
              if (!is.null(lang_content) && is.list(lang_content) &&
                  "content" %in% names(lang_content) &&
                  "common" %in% names(lang_content$content) &&
                  "app" %in% names(lang_content$content$common) &&
                  "title" %in% names(lang_content$content$common$app)) {
                lang_content$content$common$app$title
              } else {
                config$app_info$name %||% "登入"
              }
            } else {
              config$app_info$name %||% "登入"
            }
          }, error = function(e) {
            config$app_info$name %||% "登入"
          })

          # 取得 app subtitle（從 YAML 讀取，用於登入頁面副標題）
          app_subtitle <- tryCatch({
            if (exists("global_language_content_static", envir = .GlobalEnv)) {
              lang_content <- get("global_language_content_static", envir = .GlobalEnv)
              if (!is.null(lang_content) && is.list(lang_content) &&
                  "content" %in% names(lang_content) &&
                  "common" %in% names(lang_content$content) &&
                  "app" %in% names(lang_content$content$common) &&
                  "subtitle" %in% names(lang_content$content$common$app)) {
                lang_content$content$common$app$subtitle
              } else {
                NULL
              }
            } else {
              NULL
            }
          }, error = function(e) {
            NULL
          })

          # 取得 app icon（從 config 讀取，預設使用 global_scripts 中的 icon）
          # 路徑: global_assets/ 對應 scripts/global_scripts/
          app_icon <- config$app_info$icon %||% config$ui$login$icon %||% "global_assets/24_assets/icons/app_icon.png"

          loginSupabaseUI("login1", title = app_title, subtitle = app_subtitle, app_icon = app_icon)
        } else {
          stop("❌ loginSupabaseUI 函數不存在，請確認 Supabase 登入模組已載入 (scripts/global_scripts/11_rshinyapp_utils/supabase_auth/)")
        }
      )
    ),

    # 主應用 (當 user_logged_in == true 時顯示)
    conditionalPanel(
      condition = "output.user_logged_in == true",
      main_ui
    )
  )
}

# NULL 合併運算子
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

# ==========================================
# 生成 bs4Dash UI
# ==========================================
generate_bs4dash_ui <- function(config, module_manager = NULL) {
  bs4DashPage(
    title = config$app_info$name,

    # Header
    header = generate_header(config),

    # Sidebar
    sidebar = generate_sidebar(config),

    # Body
    body = generate_body(config, module_manager),

    # Footer (optional)
    footer = if (!is.null(config$ui$footer) && config$ui$footer$enabled) {
      generate_footer(config)
    } else {
      NULL
    },

    # 從 YAML 配置讀取 help 設定
    help = if (!is.null(config$features$enable_help)) {
      config$features$enable_help
    } else {
      FALSE  # 預設關閉
    },

    # 其他可選參數
    fullscreen = if (!is.null(config$ui$fullscreen)) {
      config$ui$fullscreen
    } else {
      TRUE  # 預設開啟全螢幕
    }
  )
}

# ==========================================
# 生成 Header
# ==========================================
generate_header <- function(config) {
  # 建立語言切換按鈕 - bs4DashNavbar 要求必須有 dropdown class
  # Load language choices from YAML configuration
  language_choices <- tryCatch({
    insightforge_config <- yaml::read_yaml("config/yaml/insightforge_config/insightforge.yaml")
    languages_config <- yaml::read_yaml("config/languages.yaml")

    available_langs <- insightforge_config$language$available
    choices <- c()

    # Map language directory names to codes and display names
    for (lang_dir in available_langs) {
      lang_info <- languages_config$supported_languages[[
        which(sapply(languages_config$supported_languages, function(x) x$dir == lang_dir && x$enabled))
      ]]
      if (!is.null(lang_info)) {
        label <- paste(lang_info$flag, lang_info$display_name)
        choices[label] <- lang_info$code
      }
    }

    # Default language
    default_lang <- switch(insightforge_config$language$default,
      "chinese" = "zh_TW",
      "english" = "en_US",
      "japanese" = "ja_JP",
      "zh_TW"
    )

    list(choices = choices, default = default_lang)
  }, error = function(e) {
    # Fallback if config files not available
    list(
      choices = c("🇹🇼 中文" = "zh_TW", "🇺🇸 English" = "en_US", "🇯🇵 日本語" = "ja_JP"),
      default = "zh_TW"
    )
  })

  # Language switcher with dropdown
  language_switcher <- tags$li(
    class = "dropdown",
    style = "display: flex; align-items: center; margin-right: 10px;",

    tags$a(
      href = "#",
      class = "nav-link",
      onclick = "return false;",
      style = "padding: 0.3rem 0.5rem;",

      tags$div(
        style = "display: inline-flex; align-items: center;",

        # Language icon
        tags$span(
          style = "margin-right: 8px; color: rgba(255,255,255,0.9); font-size: 14px;",
          tags$i(class = "fas fa-globe")
        ),

        # Language dropdown selector
        selectInput(
          inputId = "navbar_language_selector",
          label = NULL,
          choices = language_choices$choices,
          selected = language_choices$default,
          width = "140px",
          selectize = FALSE
        )
      )
    )
  )

  # Token 使用量 Banner
  token_banner <- tags$li(
    class = "dropdown",
    style = "display: flex; align-items: center; margin-right: 15px;",
    tags$div(
      id = "token_usage_banner",
      style = paste0(
        "display: flex; align-items: center; ",
        "background: linear-gradient(135deg, #1a1a2e 0%, #16213e 100%); ",
        "padding: 6px 12px; border-radius: 8px; ",
        "font-size: 12px; color: #e0e0e0; gap: 12px;"
      ),
      # 模型指示器
      tags$span(
        style = "display: flex; align-items: center; gap: 4px;",
        tags$i(class = "fas fa-robot", style = "color: #4fc3f7;"),
        uiOutput("token_model_display", inline = TRUE)
      ),
      # 分隔符
      tags$span("|", style = "color: #555;"),
      # Token 數量
      tags$span(
        style = "display: flex; align-items: center; gap: 4px;",
        tags$i(class = "fas fa-coins", style = "color: #ffd54f;"),
        uiOutput("token_count_display", inline = TRUE)
      ),
      # 分隔符
      tags$span("|", style = "color: #555;"),
      # 費用
      tags$span(
        style = "display: flex; align-items: center; gap: 4px;",
        tags$i(class = "fas fa-dollar-sign", style = "color: #81c784;"),
        uiOutput("token_cost_display", inline = TRUE)
      )
    )
  )

  # 建立右側 UI
  right_ui_components <- list()

  # 添加 Token Banner（僅在 debug 模式下顯示）
  is_debug_mode <- isTRUE(config$environment$debug)
  if (is_debug_mode) {
    right_ui_components$token_banner <- token_banner
    message("🔧 Debug 模式啟用 - Token 使用量 Banner 已顯示")
  }

  # 添加語言切換器（總是顯示）
  right_ui_components$language_switcher <- language_switcher

  # 資料庫狀態 - 使用 dropdown 結構（bs4DashNavbar 要求）
  if (!is.null(config$ui$navbar$show_db_status) && config$ui$navbar$show_db_status) {
    right_ui_components$db_status <- tags$li(
      class = "dropdown",
      style = "display: flex; align-items: center;",
      tags$a(
        href = "#",
        class = "nav-link",
        onclick = "return false;",
        uiOutput("db_status")
      )
    )
  }

  # 用戶選單 - 使用 dropdown 結構（bs4DashNavbar 要求）
  if (!is.null(config$ui$navbar$show_user_menu) && config$ui$navbar$show_user_menu) {
    right_ui_components$user_menu <- tags$li(
      class = "dropdown",
      style = "display: flex; align-items: center;",
      tags$a(
        href = "#",
        class = "nav-link",
        onclick = "return false;",
        uiOutput("user_menu")
      )
    )
  }

  # 只有在有元件時才建立 right_ui
  right_ui <- if (length(right_ui_components) > 0) {
    do.call(tagList, right_ui_components)
  } else {
    NULL
  }

  # 獲取應用程式名稱，確保不顯示變數模板
  app_title <- if (!is.null(config$app_info$name) && !grepl("\\{", config$app_info$name)) {
    config$app_info$name
  } else {
    "InsightForge 精準行銷平台"  # 預設名稱
  }

  bs4DashNavbar(
    title = app_title,
    rightUi = right_ui
  )
}

# ==========================================
# 生成 Sidebar
# ==========================================
generate_sidebar <- function(config) {
  # 生成選單項目
  menu_items <- generate_menu_items(config$pages)

  # 設定寬度（檢查 sidebar_width 或 width）
  sidebar_width <- config$ui$sidebar$width
  if (is.null(sidebar_width)) {
    sidebar_width <- config$ui$sidebar$sidebar_width
  }
  if (is.null(sidebar_width)) {
    sidebar_width <- 250  # 預設值
  }

  bs4DashSidebar(
    skin = "dark",
    status = config$ui$theme$primary_color %||% "primary",
    collapsed = config$ui$sidebar$collapsed_on_start %||% FALSE,
    fixed = config$ui$sidebar$fixed %||% TRUE,
    width = paste0(sidebar_width, "px"),

    bs4SidebarMenu(
      id = "sidebar_menu",
      menu_items
    )
  )
}

# ==========================================
# 生成選單項目
# ==========================================
generate_menu_items <- function(pages) {
  menu_items <- list()
  current_group <- NULL

  for (page in pages) {
    # 跳過隱藏的選單項目或登入頁面
    if (!is.null(page$hide_in_menu) && page$hide_in_menu) {
      next
    }
    if (!is.null(page$id) && page$id == "login") {
      next
    }

    if (is.null(page$enabled) || page$enabled) {
      # 檢查是否有新的分組
      # 如果 page$group 存在且不同於當前分組，添加分組標題
      # 如果 page$group 是 NULL 且之前有分組，也表示新的區段開始
      page_group <- if (!is.null(page$group)) page$group else NA_character_

      if (!is.null(page$group) && nzchar(page$group)) {
        # 有具體的分組名稱
        if (is.null(current_group) || page$group != current_group) {
          current_group <- page$group
          # 添加分組標題
          menu_items[[length(menu_items) + 1]] <- bs4SidebarHeader(current_group)
        }
      } else if (is.null(page$group) && !is.null(current_group)) {
        # group 為 null 表示獨立區段（如 About），添加分隔線
        current_group <- NULL  # 重置當前分組
      }

      # 檢查是否有子頁面
      if (!is.null(page$subpages) && length(page$subpages) > 0) {
        # 生成包含子選單的項目
        subitems <- lapply(page$subpages, function(subpage) {
          bs4SidebarMenuItem(
            text = subpage$title,
            tabName = subpage$id,
            icon = if (!is.null(subpage$icon)) icon(subpage$icon) else NULL
          )
        })

        menu_items[[length(menu_items) + 1]] <- bs4SidebarMenuItem(
          text = page$title,
          icon = if (!is.null(page$icon)) icon(page$icon) else icon("circle"),
          startExpanded = FALSE,
          subitems
        )
      } else {
        # 生成單一選單項目 - 添加 data-page-id 屬性用於動態更新
        menu_items[[length(menu_items) + 1]] <- bs4SidebarMenuItem(
          text = span(page$title, id = paste0("sidebar_text_", page$id)),
          tabName = page$id,
          icon = if (!is.null(page$icon)) icon(page$icon) else icon("circle")
        )
      }
    }
  }

  # 添加分隔線
  menu_items[[length(menu_items) + 1]] <- tags$hr(style = "border-color: rgba(255,255,255,0.1); margin: 10px 0;")

  # 添加登出按鈕 (固定在底部)
  menu_items[[length(menu_items) + 1]] <- tags$div(
    style = "padding: 10px 15px;",
    actionButton(
      inputId = "logout_btn",
      label = span(
        icon("sign-out-alt"),
        span(id = "logout_btn_text", "登出", style = "margin-left: 8px;")
      ),
      class = "btn-danger btn-block",
      style = "width: 100%; border-radius: 5px;"
    )
  )

  do.call(tagList, menu_items)
}

# ==========================================
# 生成 Body
# ==========================================
generate_body <- function(config, module_manager = NULL) {
  # 生成 Tab Items
  tab_items <- generate_tab_items(config$pages, config, module_manager)

  bs4DashBody(
    # 自訂 CSS 和 Hint 系統初始化
    tags$head(
      # 自訂 CSS
      if (!is.null(config$ui$custom_css)) {
        tags$style(HTML(config$ui$custom_css))
      },

      # 添加美化的CSS樣式
      tags$style(HTML("
        /* 語言切換按鈕美化樣式 */
        .language-toggle-container {
          box-shadow: 0 2px 6px rgba(0,0,0,0.15);
          transition: all 0.3s ease;
        }

        .language-toggle-container:hover {
          box-shadow: 0 3px 8px rgba(0,0,0,0.2);
        }

        .language-toggle-container .btn {
          transition: all 0.3s ease;
          border: none !important;
          box-shadow: none !important;
        }

        .language-toggle-container .btn:hover {
          transform: scale(1.05);
        }

        .language-toggle-container .btn:focus {
          outline: none;
          box-shadow: none;
        }

        /* Header 按鈕激活狀態樣式 */
        #header_lang_btn_zh.active,
        #header_lang_btn_en.active {
          background: linear-gradient(135deg, #007bff, #0056b3) !important;
          color: white !important;
          font-weight: 600 !important;
          box-shadow: inset 0 1px 3px rgba(0,0,0,0.3) !important;
        }

        #header_lang_btn_zh.inactive,
        #header_lang_btn_en.inactive {
          background: transparent !important;
          color: rgba(255,255,255,0.7) !important;
          font-weight: 400 !important;
        }

        /* 添加平滑過渡效果 */
        .language-toggle-container .fas.fa-globe {
          animation: spin 0.5s ease-in-out;
        }

        @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
        }

        /* Navbar 項目對齊修正 */
        .navbar-nav .nav-item.dropdown {
          display: flex;
          align-items: center;
        }
      ")),

      # 語言切換 JavaScript
      tags$script(HTML("
        // 全局變量追蹤當前語言
        var currentLanguage = 'zh_TW';  // 預設中文

        // 統一的語言切換函數
        function switchHeaderLanguage(language) {
          console.log('🌍 === Header語言切換調試 ===');
          console.log('🌍 當前語言:', currentLanguage);
          console.log('🌍 請求切換至:', language);
          console.log('🌍 時間戳:', new Date().toISOString());

          // 如果語言相同，跳過處理
          if (currentLanguage === language) {
            console.log('⚡ 語言相同，跳過切換');
            return;
          }

          // 更新全局變量
          currentLanguage = language;

          // 更新所有語言按鈕樣式（包括 header 和其他位置的按鈕）
          updateAllLanguageButtons(language);

          // 更新調試顯示
          updateDebugDisplay(language);

          // 儲存語言偏好
          sessionStorage.setItem('selected_language', language);
          console.log('💾 語言偏好已儲存至 sessionStorage:', language);

          // 觸發統一的語言變更事件
          triggerLanguageChange(language, 'header_buttons');

          console.log('🌍 === 語言切換完成 ===');
        }

        // 更新調試顯示
        function updateDebugDisplay(language) {
          var debugElement = document.getElementById('debug_language_status');
          if (debugElement) {
            debugElement.style.display = 'inline';
            debugElement.textContent = '[DEBUG: ' + language + ' @ ' + new Date().toLocaleTimeString() + ']';
          }
        }

        // 通用語言切換函數（向後兼容）
        function switchLanguage(language) {
          console.log('🌍 通用語言切換被呼叫:', language);
          switchHeaderLanguage(language);
        }

        // 更新所有語言按鈕樣式的統一函數
        function updateAllLanguageButtons(language) {
          console.log('🎨 === 更新語言按鈕樣式 ===');
          console.log('🎨 目標語言:', language);

          // 所有中文按鈕的選擇器
          var allZhButtons = ['#header_lang_btn_zh', '#home_lang_zh', '#lang_btn_zh', '#home_card_lang_btn_zh'];
          // 所有英文按鈕的選擇器
          var allEnButtons = ['#header_lang_btn_en', '#home_lang_en', '#lang_btn_en', '#home_card_lang_btn_en'];

          // 更新中文按鈕
          allZhButtons.forEach(function(selector) {
            var element = $(selector);
            if (element.length > 0) {
              console.log('🔧 找到中文按鈕:', selector);
              if (language === 'zh_TW') {
                element.removeClass('inactive').addClass('active');
                element.css({
                  'background': '#007bff',
                  'color': 'white',
                  'font-weight': '600'
                });
                console.log('✅ 中文按鈕激活');
              } else {
                element.removeClass('active').addClass('inactive');
                element.css({
                  'background': 'transparent',
                  'color': 'rgba(255,255,255,0.6)',
                  'font-weight': '400'
                });
                console.log('⭕ 中文按鈕停用');
              }
            }
          });

          // 更新英文按鈕
          allEnButtons.forEach(function(selector) {
            var element = $(selector);
            if (element.length > 0) {
              console.log('🔧 找到英文按鈕:', selector);
              if (language === 'en_US') {
                element.removeClass('inactive').addClass('active');
                element.css({
                  'background': '#007bff',
                  'color': 'white',
                  'font-weight': '600'
                });
                console.log('✅ 英文按鈕激活');
              } else {
                element.removeClass('active').addClass('inactive');
                element.css({
                  'background': 'transparent',
                  'color': 'rgba(255,255,255,0.6)',
                  'font-weight': '400'
                });
                console.log('⭕ 英文按鈕停用');
              }
            }
          });

          console.log('🎨 === 按鈕樣式更新完成 ===');
        }

        // 統一的語言變更觸發函數 - 增強版
        function triggerLanguageChange(language, source) {
          console.log('📡 === 觸發語言變更事件 ===');
          console.log('📡 語言:', language);
          console.log('📡 來源:', source);
          console.log('📡 時間戳:', Date.now());
          console.log('📡 Shiny 可用性檢查:', typeof Shiny !== 'undefined');
          console.log('📡 setInputValue 可用性:', typeof Shiny !== 'undefined' && typeof Shiny.setInputValue === 'function');

          // 檢查 Shiny 可用性
          if (typeof Shiny === 'undefined') {
            console.error('❌ Shiny 不可用，無法發送語言變更事件');
            return false;
          }

          if (typeof Shiny.setInputValue !== 'function') {
            console.error('❌ Shiny.setInputValue 不可用');
            return false;
          }

          // 觸發全局語言變更事件（統一的事件）
          var eventData = {
            language: language,
            timestamp: Date.now(),
            source: source,
            fromHeader: source.includes('header'),
            debug: true
          };

          console.log('📡 事件數據:', JSON.stringify(eventData));

          try {
            Shiny.setInputValue('global_language_change', eventData, {priority: 'event'});
            console.log('✅ 主要事件已發送');

            // 同時觸發 header 專用事件（向後兼容）
            if (source.includes('header')) {
              Shiny.setInputValue('header_language_change', eventData, {priority: 'event'});
              console.log('✅ Header 事件已發送');
            }

            // 發送備用事件以防主事件失敗
            setTimeout(function() {
              try {
                Shiny.setInputValue('language_change_backup', eventData, {priority: 'event'});
                console.log('✅ 備用事件已發送');
              } catch (backupError) {
                console.error('❌ 備用事件發送失敗:', backupError);
              }
            }, 200);

          } catch (error) {
            console.error('❌ 語言變更事件發送失敗:', error);
            return false;
          }

          console.log('📡 === 事件觸發完成 ===');
          return true;
        }

        // 頁面載入時初始化
        $(document).ready(function() {
          console.log('📱 === 頁面載入初始化 ===');
          console.log('📱 時間:', new Date().toISOString());

          // 從 sessionStorage 載入儲存的語言偏好
          var storedLang = sessionStorage.getItem('selected_language');
          console.log('🔍 sessionStorage 中的語言:', storedLang);

          // 決定使用的語言（優先使用儲存的，否則使用預設）
          var initialLang = storedLang || 'zh_TW';
          currentLanguage = initialLang;  // 更新全局變數

          console.log('🌍 初始語言設定為:', initialLang);

          // 立即更新按鈕樣式
          updateAllLanguageButtons(initialLang);
          updateDebugDisplay(initialLang);

          // 等待 Shiny 完全載入後通知初始語言設定
          var shinyInitTimer = setInterval(function() {
            if (window.Shiny && Shiny.setInputValue) {
              clearInterval(shinyInitTimer);
              console.log('📤 發送初始語言設定到 Shiny:', initialLang);
              Shiny.setInputValue('initial_language_from_storage', initialLang, {priority: 'event'});
            }
          }, 100);

          console.log('📱 === 初始化完成 ===');
        });

        // 監聽 sessionStorage 變更（跨標籤頁同步）
        window.addEventListener('storage', function(e) {
          if (e.key === 'selected_language' && e.newValue) {
            console.log('🔄 檢測到跨標籤頁語言變更:', e.newValue);
            updateAllLanguageButtons(e.newValue);
          }
        });
      ")),

      # 初始化 Hint 系統 (如果啟用且函數存在)
      if (!is.null(config$features$enable_hints) && config$features$enable_hints) {
        if (exists("init_hint_system") && is.function(init_hint_system)) {
          init_hint_system()
        }
      }
    ),

    # Tab Items
    do.call(bs4TabItems, tab_items)
  )
}

# ==========================================
# 生成 Tab Items
# ==========================================
generate_tab_items <- function(pages, config, module_manager = NULL, parent_id = NULL) {
  tab_items <- list()

  for (page in pages) {
    # 跳過登入頁面，它會在條件式 UI 中處理
    if (!is.null(page$id) && page$id == "login") {
      next
    }

    if (is.null(page$enabled) || page$enabled) {
      # 處理子頁面
      if (!is.null(page$subpages) && length(page$subpages) > 0) {
        # 遞迴生成子頁面的 tab items
        sub_items <- generate_tab_items(page$subpages, config, module_manager, page$id)
        tab_items <- c(tab_items, sub_items)
      } else {
        # 生成單一 tab item
        tab_items[[length(tab_items) + 1]] <- generate_single_tab_item(
          page, config, module_manager
        )
      }
    }
  }

  # 返回列表而非 tagList
  tab_items
}

# ==========================================
# 生成單一 Tab Item
# ==========================================
generate_single_tab_item <- function(page, config, module_manager = NULL) {
  bs4TabItem(
    tabName = page$id,

    # 頁面標題 - 使用 uiOutput 使其可響應語言變更
    uiOutput(paste0("page_title_", page$id)),

    # 頁面內容
    if (!is.null(page$module)) {
      # 如果頁面關聯到模組，載入模組 UI
      generate_module_ui(page$module, page$id, config, module_manager)
    } else {
      # 靜態內容或預設內容
      generate_static_content(page)
    }
  )
}

# ==========================================
# 生成模組 UI
# ==========================================
generate_module_ui <- function(module_name, page_id, config, module_manager = NULL) {
  # 檢查模組是否啟用
  if (!module_name %in% names(config$modules) ||
      !config$modules[[module_name]]$enabled) {
    return(
      bs4Card(
        title = "模組未啟用",
        status = "warning",
        width = 12,
        p(paste("模組", module_name, "未啟用或不存在"))
      )
    )
  }

  # 🎚️ 關鍵修改: 使用 uiOutput 包裝模組 UI，使其可以動態更新
  # 這樣當語言切換時，模組 UI 可以重新渲染
  # 注意：NS 應該使用 page_id 作為基礎命名空間
  return(
    div(
      class = "dynamic-module-container",
      `data-module` = module_name,
      `data-page` = page_id,
      uiOutput(paste0(page_id, "_", module_name, "_ui_container"))
    )
  )
}

# 原始的靜態生成函數，改名為 generate_module_ui_static
generate_module_ui_static <- function(module_name, page_id, config, module_manager = NULL) {
  # 檢查模組是否啟用
  if (!module_name %in% names(config$modules) ||
      !config$modules[[module_name]]$enabled) {
    return(
      bs4Card(
        title = "模組未啟用",
        status = "warning",
        width = 12,
        p(paste("模組", module_name, "未啟用或不存在"))
      )
    )
  }

  # 先載入模組檔案（如果還沒載入）
  module_config <- config$modules[[module_name]]
  if (!is.null(module_config$path) && file.exists(module_config$path)) {
    tryCatch({
      source(module_config$path, local = FALSE)
      message(sprintf("✅ 已載入模組檔案: %s", module_config$path))
    }, error = function(e) {
      message(sprintf("❌ 載入模組檔案失敗 %s: %s", module_config$path, e$message))
    })
  } else {
    message(sprintf("⚠️ 模組檔案不存在或未設定: %s", module_config$path %||% "NULL"))
  }

  # 嘗試不同的函數命名模式來找到 UI 函數
  # 處理命名映射關係
  name_mappings <- list(
    "scoring" = "score",
    "keyword_ads" = "keywordAds",
    "product_dev" = "productDev",
    "sales_model" = "salesModel",  # sales_model 使用 salesModel 模組（Poisson regression）
    # BrandEdge submodule mappings
    "market_profile" = "marketProfile",
    "market_track" = "marketTrack",
    "advanced_attribute" = "advancedAttribute",
    "advanced_dna" = "advancedDNA",
    "brand_identity" = "brandIdentity",
    "ideal_point" = "idealPoint",
    "positioning_strategy" = "positioningStrategy",
    # VitalSigns submodule mappings
    "vitalsigns_upload" = "uploadVitalsigns",
    "vitalsigns_dna" = "dnaVitalsigns",
    "vitalsigns_acquisition" = "acquisitionVitalsigns",
    "vitalsigns_retention" = "retentionVitalsigns",
    "vitalsigns_engagement" = "engagementVitalsigns",
    "vitalsigns_revenue_pulse" = "revenuePulseVitalsigns"
  )

  base_name <- name_mappings[[module_name]] %||% module_name

  possible_ui_names <- c(
    paste0(module_name, "ModuleUI"),
    paste0(module_name, "UI"),
    paste0(module_name, "_ui"),
    paste0(base_name, "ModuleUI"),  # 處理映射後的名稱
    paste0(base_name, "UI"),
    paste0(base_name, "_ui")
  )

  ui_function <- NULL
  for (ui_name in possible_ui_names) {
    if (exists(ui_name)) {
      ui_function <- get(ui_name)
      break
    }
  }

  # 如果找到 UI 函數，呼叫它並傳遞模組配置
  if (!is.null(ui_function)) {
    # 從 config 取得模組配置（如果存在）
    module_detail_config <- NULL
    if (!is.null(config$module_configs) && !is.null(config$module_configs[[module_name]])) {
      module_detail_config <- config$module_configs[[module_name]]
    }

    # 取得模組語言內容（如果存在）
    module_lang_texts <- NULL
    if (exists("lang_content", envir = .GlobalEnv) && !is.null(lang_content)) {
      if (exists("get_module_texts") && is.function(get_module_texts)) {
        module_lang_texts <- tryCatch({
          get_module_texts(lang_content, module_name)
        }, error = function(e) NULL)
      }
    }

    # 嘗試傳遞配置和語言內容，如果函數不接受參數則只傳遞 id
    tryCatch({
      # 先嘗試傳遞配置和語言內容
      return(ui_function(page_id,
                        module_config = module_detail_config,
                        lang_texts = module_lang_texts))
    }, error = function(e) {
      # 如果失敗，嘗試只傳遞配置
      tryCatch({
        return(ui_function(page_id, module_config = module_detail_config))
      }, error = function(e2) {
        # 如果仍然失敗，只傳遞 id
        return(ui_function(page_id))
      })
    })
  }

  # 如果沒找到，返回錯誤訊息
  div(
    id = NS(page_id)("module_container"),
    class = "module-container",
    data_module = module_name,

    bs4Card(
      title = paste("模組載入錯誤:", module_name),
      status = "danger",
      width = 12,
      div(
        p(strong("⚠️ 無法載入模組 UI")),
        p("可能的原因："),
        tags$ul(
          tags$li(sprintf("找不到任何符合的 UI 函數：%s",
                         paste(possible_ui_names, collapse = ", "))),
          tags$li(sprintf("模組檔案路徑：%s",
                         module_config$path %||% "未設定")),
          tags$li("檔案可能未正確載入或函數名稱不符")
        ),
        br(),
        actionButton(
          NS(page_id)("reload_module"),
          "🔄 重新載入模組",
          class = "btn-warning"
        )
      )
    )
  )
}

# ==========================================
# 生成靜態內容
# ==========================================
generate_static_content <- function(page) {
  # 根據頁面 ID 生成對應的靜態內容
  content <- switch(page$id,
    "home" = generate_home_content(),
    "admin" = generate_admin_content(),
    "settings" = generate_settings_content(),

    # 預設內容
    bs4Card(
      title = page$title,
      status = "primary",
      width = 12,
      p("頁面內容載入中...")
    )
  )

  return(content)
}

# ==========================================
# 生成首頁內容
# ==========================================
generate_home_content <- function() {
  fluidRow(
    # 主要歡迎卡片
    bs4Card(
      title = div(
        style = "display: flex; justify-content: space-between; align-items: center;",
        span("歡迎使用 / Welcome"),
        div(
          style = "font-size: 14px; font-weight: normal;",
          span(
            style = "margin-right: 8px;",
            tags$i(class = "fas fa-globe"),
            "語言:"
          ),
          tags$a(
            href = "#",
            onclick = "switchLanguage('zh_TW'); return false;",
            id = "home_card_lang_btn_zh",  # Changed to avoid ID conflict
            style = "color: #007bff; margin-right: 8px; text-decoration: none;",
            "中文"
          ),
          span("|", style = "margin-right: 8px; color: #ccc;"),
          tags$a(
            href = "#",
            onclick = "switchLanguage('en_US'); return false;",
            id = "home_card_lang_btn_en",  # Changed to avoid ID conflict
            style = "color: #007bff; text-decoration: none;",
            "English"
          )
        )
      ),
      status = "primary",
      width = 8,
      solidHeader = TRUE,

      h4("系統簡介 / System Introduction"),
      p("這是一個基於 YAML 配置的動態 Shiny 應用程式。"),
      p("This is a dynamic Shiny application based on YAML configuration."),

      hr(),

      h4("快速開始 / Quick Start"),
      tags$ol(
        tags$li("選擇左側選單開始操作 / Select from the left menu to start"),
        tags$li("根據工作流程依序完成各步驟 / Complete each step according to the workflow"),
        tags$li("查看分析結果並下載報告 / View analysis results and download reports")
      )
    ),

    # 語言調試卡片
    bs4Card(
      title = "語言調試 / Language Debug",
      status = "info",
      width = 4,
      solidHeader = TRUE,

      h5("語言切換 / Language Switch"),
      p("使用頂部導航欄的語言按鈕或下方按鈕切換語言"),
      p("Use language buttons in the top navigation or below buttons to switch language"),

      # 大型語言切換按鈕
      div(
        class = "text-center mb-3",
        style = "gap: 10px;",

        actionButton(
          "home_lang_zh",
          "中文",
          class = "btn-primary",
          style = "margin-right: 10px; padding: 8px 16px;",
          onclick = "switchToLanguage('zh_TW')"
        ),

        actionButton(
          "home_lang_en",
          "English",
          class = "btn-secondary",
          style = "padding: 8px 16px;",
          onclick = "switchToLanguage('en_US')"
        )
      ),

      hr(),

      # 調試資訊
      h6("調試資訊 / Debug Info"),
      div(
        id = "debug_info",
        style = "font-size: 11px; background: #f8f9fa; padding: 8px; border-radius: 4px;",
        verbatimTextOutput("current_language_debug", placeholder = TRUE)
      ),

      # 語言切換 JavaScript
      tags$script(HTML("
        function switchToLanguage(language) {
          console.log('🏠 Home頁面語言切換:', language);

          // 更新按鈕樣式
          if (language === 'zh_TW') {
            $('#home_lang_zh').removeClass('btn-secondary').addClass('btn-primary');
            $('#home_lang_en').removeClass('btn-primary').addClass('btn-secondary');
          } else {
            $('#home_lang_en').removeClass('btn-secondary').addClass('btn-primary');
            $('#home_lang_zh').removeClass('btn-primary').addClass('btn-secondary');
          }

          // 儲存語言偏好
          sessionStorage.setItem('selected_language', language);

          // 觸發 Shiny 語言變更事件
          Shiny.setInputValue('home_language_change', {
            language: language,
            timestamp: Date.now(),
            source: 'home_page'
          }, {priority: 'event'});

          // 同時觸發全局語言變更
          Shiny.setInputValue('global_language_change', {
            language: language,
            timestamp: Date.now(),
            source: 'home_page_buttons'
          }, {priority: 'event'});

          // 更新調試資訊
          $('#debug_info').html(
            '<strong>當前語言 / Current Language:</strong> ' + language + '<br>' +
            '<strong>變更時間 / Change Time:</strong> ' + new Date().toLocaleString() + '<br>' +
            '<strong>來源 / Source:</strong> Home Page'
          );
        }

        // 頁面載入時初始化
        $(document).ready(function() {
          var currentLang = sessionStorage.getItem('selected_language') || 'zh_TW';
          switchToLanguage(currentLang);
        });
      "))
    )
  )
}

# ==========================================
# 生成管理頁面內容
# ==========================================
generate_admin_content <- function() {
  fluidRow(
    bs4Card(
      title = "系統管理",
      status = "danger",
      width = 12,
      solidHeader = TRUE,

      h4("管理功能"),
      p("系統管理員專用功能區域")
    )
  )
}

# ==========================================
# 生成設定頁面內容
# ==========================================
generate_settings_content <- function() {
  fluidRow(
    bs4Card(
      title = "系統設定",
      status = "info",
      width = 12,
      solidHeader = TRUE,

      h4("配置選項"),
      p("在此調整系統設定")
    )
  )
}

# ==========================================
# 生成 Footer
# ==========================================
generate_footer <- function(config) {
  bs4DashFooter(
    left = paste("© 2025", config$app_info$name),
    right = paste("Version", config$app_info$version)
  )
}

# ==========================================
# 生成預設 UI（非 bs4Dash）
# ==========================================
generate_default_ui <- function(config, module_manager = NULL) {
  fluidPage(
    title = config$app_info$name,

    # Header
    titlePanel(config$app_info$name),

    # Layout
    sidebarLayout(
      # Sidebar
      sidebarPanel(
        width = 3,
        generate_default_menu(config$pages)
      ),

      # Main Panel
      mainPanel(
        width = 9,
        generate_default_content(config$pages, config, module_manager)
      )
    )
  )
}

# ==========================================
# 生成預設選單
# ==========================================
generate_default_menu <- function(pages) {
  menu_choices <- list()

  for (page in pages) {
    if (is.null(page$enabled) || page$enabled) {
      menu_choices[[page$title]] <- page$id
    }
  }

  selectInput(
    inputId = "page_selector",
    label = "選擇頁面",
    choices = menu_choices,
    selected = menu_choices[[1]]
  )
}

# ==========================================
# 生成預設內容
# ==========================================
generate_default_content <- function(pages, config, module_manager = NULL) {
  conditionalPanels <- list()

  for (page in pages) {
    if (is.null(page$enabled) || page$enabled) {
      conditionalPanels[[length(conditionalPanels) + 1]] <- conditionalPanel(
        condition = paste0("input.page_selector == '", page$id, "'"),

        h3(page$title),

        if (!is.null(page$module)) {
          generate_module_ui(page$module, page$id, config, module_manager)
        } else {
          generate_static_content(page)
        }
      )
    }
  }

  do.call(tagList, conditionalPanels)
}

# ==========================================
# 生成動態 UI 元件
# ==========================================
generate_dynamic_ui <- function(config, element_type, element_id, ...) {
  # 根據配置生成特定類型的 UI 元件

  element_config <- get_ui_element_config(config, element_type, element_id)

  if (is.null(element_config)) {
    return(NULL)
  }

  # 根據元件類型生成對應的 UI
  switch(element_type,
    "card" = generate_card(element_config, ...),
    "table" = generate_table(element_config, ...),
    "plot" = generate_plot(element_config, ...),
    "form" = generate_form(element_config, ...),
    NULL
  )
}

# ==========================================
# 取得 UI 元件配置
# ==========================================
get_ui_element_config <- function(config, element_type, element_id) {
  # 從配置中查找特定 UI 元件的設定
  # 這裡可以擴展配置結構來支援更多 UI 元件

  if (!is.null(config$ui_elements)) {
    if (!is.null(config$ui_elements[[element_type]])) {
      return(config$ui_elements[[element_type]][[element_id]])
    }
  }

  return(NULL)
}
# ==========================================
# 動態側邊欄更新器 - Dynamic Sidebar Updater
# ==========================================
# 負責動態更新側邊欄選單文字以支援多語言切換
# Version: 1.0
# Last Updated: 2025-09-28

library(shiny)

# ==========================================
# 更新側邊欄選單文字
# ==========================================
update_sidebar_menu_text <- function(session, pages, lang_content) {
  cat("\n🔄 [Sidebar Updater] 開始更新側邊欄文字\n")

  # 詳細檢查語言內容
  cat("🔍 [Sidebar Updater] 語言內容檢查:\n")
  cat("    - lang_content 是否為 NULL:", is.null(lang_content), "\n")
  if (!is.null(lang_content)) {
    cat("    - 類型:", class(lang_content), "\n")
    cat("    - 欄位:", paste(names(lang_content), collapse=", "), "\n")
    if ("language" %in% names(lang_content)) {
      cat("    - language 值:", lang_content$language %||% "NULL", "\n")
    }
    cat("    - content 是否為 NULL:", is.null(lang_content$content), "\n")
  }

  # 確保有語言內容
  if (is.null(lang_content) || is.null(lang_content$content)) {
    cat("❌ [Sidebar Updater] 語言內容為空\n")
    return()
  }

  # 記錄當前語言
  current_language <- lang_content$language %||% "未知"
  cat("✅ [Sidebar Updater] 處理語言:", current_language, "\n")

  # 獲取頁面標題翻譯
  page_titles <- lang_content$content$common$pages

  if (is.null(page_titles)) {
    cat("⚠️ [Sidebar Updater] 找不到頁面標題翻譯\n")
    return()
  }

  # 登出文字（可選，支援 common$logout 或 common$nav$logout）
  logout_label <- lang_content$content$common$logout %||%
                  lang_content$content$common$nav$logout %||% NULL

  # 過濾：只保留可見且啟用的頁面（排除 login / hide_in_menu）
  if (!is.null(pages) && length(pages) > 0) {
    # Extract eligible page IDs
    page_ids <- vapply(pages, function(p) {
      if (is.null(p$id)) return("")
      if (!is.null(p$hide_in_menu) && p$hide_in_menu) return("")
      if (!is.null(p$enabled) && isFALSE(p$enabled)) return("")
      if (identical(p$id, "login")) return("")
      p$id
    }, character(1))
    page_ids <- page_ids[nzchar(page_ids)]
    cat("🔍 [Sidebar Updater] 頁面 IDs:", paste(page_ids, collapse=", "), "\n")

    # Filter page_titles to only include the enabled pages
    page_titles <- page_titles[names(page_titles) %in% page_ids]
    cat("✅ [Sidebar Updater] 找到頁面標題:", paste(names(page_titles), "=", page_titles, collapse=", "), "\n")
  } else {
    cat("✅ [Sidebar Updater] 找到頁面標題:", paste(names(page_titles), "=", page_titles, collapse=", "), "\n")
  }

  # 使用增強的 JavaScript 更新側邊欄選單文字
  js_code <- "
    (function() {
      var translations = %s;
      console.log('🔄 [Sidebar JS] 開始更新側邊欄文字');
      console.log('📝 翻譯內容:', translations);

      // 動態取得需要更新的頁面 ID（從 translations 物件）
      var pageIds = Object.keys(translations);

      // 方法1: 透過 ID 直接更新 span 元素
      Object.keys(translations).forEach(function(pageId) {
        var spanId = 'sidebar_text_' + pageId;
        var spanElement = document.getElementById(spanId);
        if (spanElement) {
          spanElement.textContent = translations[pageId];
          console.log('✅ [Sidebar JS] 透過 ID 更新:', spanId, '→', translations[pageId]);
        }
      });

      // 方法2: 透過 data-value 屬性更新（向後兼容）
      var menuItems = document.querySelectorAll('.sidebar-menu .nav-link');
      menuItems.forEach(function(item) {
        var tabName = item.getAttribute('data-value');
        if (!tabName) {
          var href = item.getAttribute('href');
          if (href && href.startsWith('#shiny-tab-')) {
            tabName = href.replace('#shiny-tab-', '');
          }
        }

        if (tabName && translations[tabName]) {
          // 優先尋找含有對應 ID 的 span
          var targetSpan = item.querySelector('#sidebar_text_' + tabName);
          if (targetSpan) {
            targetSpan.textContent = translations[tabName];
            console.log('✅ [Sidebar JS] 透過選擇器更新:', tabName, '→', translations[tabName]);
          } else {
            // 備用方案：更新 p 或 span 元素
            var textElement = item.querySelector('p') || item.querySelector('span:not(.nav-icon)');
            if (textElement) {
              textElement.textContent = translations[tabName];
              console.log('✅ [Sidebar JS] 透過備用方式更新:', tabName, '→', translations[tabName]);
            }
          }
        }
      });

      // 通用特別處理：確保所有頁面都更新成功
      Object.keys(translations).forEach(function(pageId) {
        if (!translations[pageId]) return;

        console.log('🎯 [Sidebar JS] 特別處理:', pageId);
        var pageText = translations[pageId];

        // 嘗試多種選擇器確保更新成功
        var selectors = [
          '#sidebar_text_' + pageId,
          '[data-value=\"' + pageId + '\"] span:not(.nav-icon)',
          '[data-value=\"' + pageId + '\"] p',
          '[href=\"#shiny-tab-' + pageId + '\"] span:not(.nav-icon)',
          '[href=\"#shiny-tab-' + pageId + '\"] p'
        ];

        var updated = false;
        selectors.forEach(function(selector) {
          var elements = document.querySelectorAll(selector);
          if (elements.length > 0) {
            elements.forEach(function(elem) {
              if (elem && !elem.classList.contains('nav-icon')) {
                elem.textContent = pageText;
                console.log('✅ [Sidebar JS] 頁面更新成功:', pageId, selector);
                updated = true;
              }
            });
          }
        });

        if (!updated) {
          console.log('⚠️ [Sidebar JS] 無法更新頁面文字:', pageId);
        }
      });

      // 更新群組標題
      var groupHeaders = document.querySelectorAll('.nav-header');
      groupHeaders.forEach(function(header) {
        var groupKey = header.textContent.trim();
        if (translations.groups) {
          Object.keys(translations.groups).forEach(function(key) {
            if (groupKey.includes(key) || groupKey === translations.groups[key]) {
              header.textContent = translations.groups[key];
              console.log('✅ [Sidebar JS] 更新群組標題:', groupKey, '→', translations.groups[key]);
            }
          });
        }
      });

      // 最終檢查關鍵頁面是否更新成功
      setTimeout(function() {
        ['sales_model', 'scoring', 'upload'].forEach(function(pageId) {
          var elem = document.getElementById('sidebar_text_' + pageId);
          if (elem) {
            console.log('🔍 [Sidebar JS] 最終檢查 -', pageId, ':', elem.textContent);
          } else {
            console.log('⚠️ [Sidebar JS] 最終檢查 - 找不到', pageId, '元素');
          }
        });
      }, 100);

      // 更新登出按鈕文字（如有）
      if (translations.logout) {
        var logoutElem = document.getElementById('logout_btn_text');
        if (logoutElem) {
          logoutElem.textContent = translations.logout;
          console.log('✅ [Sidebar JS] 更新登出按鈕:', translations.logout);
        }
      }

      console.log('✅ [Sidebar JS] 側邊欄選單文字更新完成');
    })();
  "

  # 將頁面標題轉換為 JSON（包含登出文字）
  if (!is.null(logout_label)) {
    page_titles$logout <- logout_label
  }
  translations_json <- jsonlite::toJSON(page_titles, auto_unbox = TRUE)
  cat("📦 [Sidebar Updater] 轉換為 JSON 完成\n")

  # 執行 JavaScript 代碼
  session$sendCustomMessage(
    type = "update_sidebar_text",
    message = list(
      translations = page_titles,
      js_code = sprintf(js_code, translations_json)
    )
  )

  # 直接執行 JavaScript（作為備用方案）
  shinyjs::runjs(sprintf(js_code, translations_json))
  cat("✅ [Sidebar Updater] JavaScript 已執行\n")
  cat("✅ 側邊欄選單文字已更新為", current_language, "\n")
}

# ==========================================
# 註冊 JavaScript 處理器
# ==========================================
register_sidebar_updater_js <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('update_sidebar_text', function(data) {
      console.log('收到側邊欄更新請求');

      if (data.js_code) {
        eval(data.js_code);
      } else if (data.translations) {
        // 備用更新方法
        var translations = data.translations;

        // 更新側邊欄選單項目
        $('.sidebar-menu .nav-link').each(function() {
          var $link = $(this);
          var tabName = $link.attr('data-value') ||
                       ($link.attr('href') || '').replace('#shiny-tab-', '');

          if (tabName && translations[tabName]) {
            var $text = $link.find('p').length > 0 ? $link.find('p') :
                       $link.find('span:not(.nav-icon)');
            if ($text.length > 0) {
              $text.text(translations[tabName]);
            }
          }
        });

        // 更新群組標題
        if (translations.groups) {
          $('.nav-header').each(function() {
            var $header = $(this);
            var headerText = $header.text().trim();

            Object.keys(translations.groups).forEach(function(key) {
              if (headerText.includes(key) || headerText === translations.groups[key]) {
                $header.text(translations.groups[key]);
              }
            });
          });
        }
      }
    });
  "))
}

# ==========================================
# 監聽語言變更並更新側邊欄
# ==========================================
observe_sidebar_language_change <- function(input, session, pages, global_lang_content) {
  observeEvent(global_lang_content(), {
    lang_content <- global_lang_content()
    if (!is.null(lang_content)) {
      cat("🔄 更新側邊欄選單文字 -", lang_content$language, "\n")
      update_sidebar_menu_text(session, pages, lang_content)
    }
  })
}

# ==========================================
# 匯出函數
# ==========================================
sidebar_updater_exports <- list(
  update_sidebar_menu_text = update_sidebar_menu_text,
  register_sidebar_updater_js = register_sidebar_updater_js,
  observe_sidebar_language_change = observe_sidebar_language_change
)

# ==========================================
# 動態側邊欄更新器 - Dynamic Sidebar Updater (修復版)
# ==========================================
# 負責動態更新側邊欄選單文字以支援多語言切換
# Version: 1.1 - Fixed
# Last Updated: 2025-09-29

library(shiny)

# ==========================================
# 更新側邊欄選單文字 (修復版)
# ==========================================
update_sidebar_menu_text_fixed <- function(session, pages, lang_content) {
  cat("\n🔄 [Sidebar Updater Fixed] 開始更新側邊欄文字\n")

  # 詳細檢查語言內容
  cat("🔍 [Sidebar Updater Fixed] 語言內容檢查:\n")
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
    cat("❌ [Sidebar Updater Fixed] 語言內容為空\n")
    return()
  }

  # 記錄當前語言
  current_language <- lang_content$language %||% "未知"
  cat("✅ [Sidebar Updater Fixed] 處理語言:", current_language, "\n")

  # 獲取頁面標題翻譯
  page_titles <- lang_content$content$common$pages

  if (is.null(page_titles)) {
    cat("⚠️ [Sidebar Updater Fixed] 找不到頁面標題翻譯\n")
    return()
  }

  cat("✅ [Sidebar Updater Fixed] 找到頁面標題:\n")
  for (page_id in names(page_titles)) {
    cat("    -", page_id, ":", page_titles[[page_id]], "\n")
  }

  # 確保包含所有重要頁面
  required_pages <- c("upload", "scoring", "sales_model", "keyword_ads", "product_dev")
  missing_pages <- setdiff(required_pages, names(page_titles))
  if (length(missing_pages) > 0) {
    cat("⚠️ [Sidebar Updater Fixed] 缺少頁面翻譯:", paste(missing_pages, collapse=", "), "\n")
  }

  # 使用增強的 JavaScript 更新側邊欄選單文字
  js_code <- "
    (function() {
      var translations = %s;
      console.log('🔄 [Sidebar JS Fixed] 開始更新側邊欄文字');
      console.log('📝 翻譯內容:', translations);

      // 定義需要更新的所有頁面 ID
      var pageIds = ['upload', 'scoring', 'sales_model', 'keyword_ads', 'product_dev'];

      // 逐個更新每個頁面
      pageIds.forEach(function(pageId) {
        if (translations[pageId]) {
          // 方法1: 透過 ID 直接更新 span 元素
          var spanId = 'sidebar_text_' + pageId;
          var spanElement = document.getElementById(spanId);
          if (spanElement) {
            spanElement.textContent = translations[pageId];
            console.log('✅ [Sidebar JS Fixed] 更新成功 (ID):', spanId, '→', translations[pageId]);
          } else {
            console.log('⚠️ [Sidebar JS Fixed] 找不到元素:', spanId);
          }

          // 方法2: 透過 data-value 屬性查找並更新 (備用)
          var menuItems = document.querySelectorAll('.sidebar-menu .nav-link[data-value=\"' + pageId + '\"]');
          menuItems.forEach(function(item) {
            var textElement = item.querySelector('p') || item.querySelector('span:not(.nav-icon)');
            if (textElement && !textElement.id) {
              textElement.textContent = translations[pageId];
              console.log('✅ [Sidebar JS Fixed] 更新成功 (data-value):', pageId, '→', translations[pageId]);
            }
          });

          // 方法3: 透過 href 屬性查找並更新 (備用)
          var hrefItems = document.querySelectorAll('.sidebar-menu .nav-link[href=\"#shiny-tab-' + pageId + '\"]');
          hrefItems.forEach(function(item) {
            var targetSpan = item.querySelector('#sidebar_text_' + pageId);
            if (targetSpan) {
              targetSpan.textContent = translations[pageId];
              console.log('✅ [Sidebar JS Fixed] 更新成功 (href+ID):', pageId, '→', translations[pageId]);
            } else {
              var textElement = item.querySelector('p') || item.querySelector('span:not(.nav-icon)');
              if (textElement) {
                textElement.textContent = translations[pageId];
                console.log('✅ [Sidebar JS Fixed] 更新成功 (href):', pageId, '→', translations[pageId]);
              }
            }
          });
        }
      });

      // 特別處理 sales_model (銷售模型)
      if (translations.sales_model) {
        console.log('🎯 [Sidebar JS Fixed] 特別處理銷售模型...');

        // 嘗試所有可能的選擇器
        var selectors = [
          '#sidebar_text_sales_model',
          '[data-value=\"sales_model\"] span:not(.nav-icon)',
          '[data-value=\"sales_model\"] p',
          '[href=\"#shiny-tab-sales_model\"] span:not(.nav-icon)',
          '[href=\"#shiny-tab-sales_model\"] p',
          'a[data-value=\"sales_model\"] span:not(.nav-icon)',
          '.nav-link[data-value=\"sales_model\"] span:not(.nav-icon)'
        ];

        var updated = false;
        selectors.forEach(function(selector) {
          var elements = document.querySelectorAll(selector);
          if (elements.length > 0) {
            elements.forEach(function(elem) {
              if (elem && !elem.classList.contains('nav-icon')) {
                elem.textContent = translations.sales_model;
                console.log('✅ [Sidebar JS Fixed] 銷售模型更新成功:', selector);
                updated = true;
              }
            });
          }
        });

        if (!updated) {
          console.log('❌ [Sidebar JS Fixed] 無法更新銷售模型文字');
        }
      }

      // 更新群組標題
      var groupHeaders = document.querySelectorAll('.nav-header');
      groupHeaders.forEach(function(header) {
        var groupKey = header.textContent.trim();
        if (translations.groups) {
          Object.keys(translations.groups).forEach(function(key) {
            if (groupKey.includes(key) || groupKey === translations.groups[key]) {
              header.textContent = translations.groups[key];
              console.log('✅ [Sidebar JS Fixed] 更新群組標題:', groupKey, '→', translations.groups[key]);
            }
          });
        }
      });

      // 最終檢查 sales_model 是否更新成功
      setTimeout(function() {
        var rmElem = document.getElementById('sidebar_text_sales_model');
        if (rmElem) {
          console.log('🔍 [Sidebar JS Fixed] 最終檢查 - 銷售模型文字:', rmElem.textContent);
        } else {
          console.log('⚠️ [Sidebar JS Fixed] 最終檢查 - 找不到銷售模型元素');
        }
      }, 100);

      console.log('✅ [Sidebar JS Fixed] 側邊欄選單文字更新完成');
    })();
  "

  # 將頁面標題轉換為 JSON
  translations_json <- jsonlite::toJSON(page_titles, auto_unbox = TRUE)
  cat("📦 [Sidebar Updater Fixed] 轉換為 JSON 完成\n")

  # 執行 JavaScript 代碼
  if (exists("shinyjs::runjs")) {
    shinyjs::runjs(sprintf(js_code, translations_json))
    cat("✅ [Sidebar Updater Fixed] JavaScript 已執行 (shinyjs)\n")
  }

  # 使用 session$sendCustomMessage 作為備用
  session$sendCustomMessage(
    type = "update_sidebar_text_fixed",
    message = list(
      translations = page_titles,
      js_code = sprintf(js_code, translations_json)
    )
  )
  cat("✅ [Sidebar Updater Fixed] 訊息已發送 (customMessage)\n")

  cat("✅ 側邊欄選單文字已更新為", current_language, "\n")
}

# ==========================================
# 註冊 JavaScript 處理器 (修復版)
# ==========================================
register_sidebar_updater_js_fixed <- function() {
  tags$script(HTML("
    Shiny.addCustomMessageHandler('update_sidebar_text_fixed', function(data) {
      console.log('收到側邊欄更新請求 (Fixed)');

      if (data.js_code) {
        eval(data.js_code);
      } else if (data.translations) {
        // 備用更新方法
        var translations = data.translations;

        // 定義所有需要更新的頁面
        var pageIds = ['upload', 'scoring', 'sales_model', 'keyword_ads', 'product_dev'];

        pageIds.forEach(function(pageId) {
          if (translations[pageId]) {
            // 更新帶 ID 的 span
            var spanElem = document.getElementById('sidebar_text_' + pageId);
            if (spanElem) {
              spanElem.textContent = translations[pageId];
              console.log('✅ 更新:', pageId, '→', translations[pageId]);
            }

            // 更新選單項目
            $('.sidebar-menu .nav-link').each(function() {
              var $link = $(this);
              var tabName = $link.attr('data-value') ||
                           ($link.attr('href') || '').replace('#shiny-tab-', '');

              if (tabName === pageId) {
                var $text = $link.find('#sidebar_text_' + pageId);
                if ($text.length > 0) {
                  $text.text(translations[pageId]);
                } else {
                  $text = $link.find('p').length > 0 ? $link.find('p') :
                         $link.find('span:not(.nav-icon)');
                  if ($text.length > 0) {
                    $text.text(translations[pageId]);
                  }
                }
              }
            });
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
# 監聽語言變更並更新側邊欄 (修復版)
# ==========================================
observe_sidebar_language_change_fixed <- function(input, session, pages, global_lang_content) {
  observeEvent(global_lang_content(), {
    lang_content <- global_lang_content()
    if (!is.null(lang_content)) {
      cat("🔄 更新側邊欄選單文字 -", lang_content$language, "\n")
      update_sidebar_menu_text_fixed(session, pages, lang_content)
    }
  })
}

# ==========================================
# 匯出函數
# ==========================================
sidebar_updater_fixed_exports <- list(
  update_sidebar_menu_text_fixed = update_sidebar_menu_text_fixed,
  register_sidebar_updater_js_fixed = register_sidebar_updater_js_fixed,
  observe_sidebar_language_change_fixed = observe_sidebar_language_change_fixed
)
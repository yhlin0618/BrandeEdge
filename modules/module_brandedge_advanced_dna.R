# BrandEdge Advanced DNA Module
# 進階品牌DNA比較模組
# Version: 2.0
# Last Updated: 2025-10-07
# Framework: Following InsightForge pattern with language support

# NULL coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ========== 4. 進階品牌DNA比較模組 ==========

advancedDNAModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Helper function to get text from lang_texts
  get_lang_value <- function(path, default) {
    if (is.null(lang_texts)) return(default)
    parts <- strsplit(path, "\\.")[[1]]
    current <- lang_texts
    for (part in parts) {
      if (is.list(current) && part %in% names(current)) {
        current <- current[[part]]
      } else {
        return(default)
      }
    }
    if (is.null(current)) default else current
  }

  # Get translated texts
  no_data_title <- get_lang_value("ui.no_data.title", "進階品牌DNA比較")
  no_data_message <- get_lang_value("ui.no_data.message", "請先上傳評論資料以查看品牌DNA比較分析")
  upload_button <- get_lang_value("ui.no_data.upload_button", "前往資料上傳")

  select_brands_label <- get_lang_value("ui.labels.select_brands", "選擇比較品牌")

  tagList(
    # Note: Page title is handled by app_brandedge.R, not here

    # Dynamic placeholder (rendered in Server function)
    uiOutput(ns("no_data_placeholder")),

    # Content area (shown when data exists)
    uiOutput(ns("content_area"))
  )
}

advancedDNAModuleServer <- function(id, data_full, lang_texts = reactive(NULL), module_config = NULL, api_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # 從共用 API 設定讀取模型（fallback 到 gpt-5-nano）
    cfg_ai_model <- if (!is.null(api_config$default_model)) {
      api_config$default_model
    } else {
      "gpt-5-nano"
    }

    # ========== Helper Functions ==========
    # Get current language for prompt system
    get_current_language <- function() {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) NULL)

      # Try to extract language from texts metadata
      if (!is.null(texts) && !is.null(texts$metadata) && !is.null(texts$metadata$language)) {
        return(texts$metadata$language)
      }

      # Try to get from global app_config
      if (exists("app_config", envir = .GlobalEnv)) {
        app_config <- get("app_config", envir = .GlobalEnv)
        if (!is.null(app_config$language) && !is.null(app_config$language$default)) {
          return(app_config$language$default)
        }
      }

      # Fallback to Chinese
      return("zh_TW")
    }

    # ========== Configuration Loading ==========
    cfg_default_brands <- if (!is.null(module_config$ui_defaults$brand_selection$default_count)) {
      module_config$ui_defaults$brand_selection$default_count
    } else { 5 } # ✅

    cfg_min_brands <- if (!is.null(module_config$validation$min_brands_for_comparison)) {
      module_config$validation$min_brands_for_comparison
    } else { 2 } # ✅

    cfg_strength_threshold <- if (!is.null(module_config$analysis$strength_threshold)) {
      module_config$analysis$strength_threshold
    } else { 4.0 } # ✅

    cfg_weakness_threshold <- if (!is.null(module_config$analysis$weakness_threshold)) {
      module_config$analysis$weakness_threshold
    } else { 3.0 } # ✅

    cfg_top_count <- if (!is.null(module_config$analysis$top_strengths_count)) {
      module_config$analysis$top_strengths_count
    } else { 3 } # ✅

    cfg_radar_range <- if (!is.null(module_config$visualization$radar$radial_axis$range)) {
      module_config$visualization$radar$radial_axis$range
    } else { c(0, 5) } # ✅

    cfg_radar_font_size <- if (!is.null(module_config$visualization$radar$title$font_size)) {
      module_config$visualization$radar$title$font_size
    } else { 20 } # ✅

    cfg_radar_direction <- if (!is.null(module_config$visualization$radar$angular_axis$direction)) {
      module_config$visualization$radar$angular_axis$direction
    } else { "clockwise" } # ✅

    cfg_radar_rotation <- if (!is.null(module_config$visualization$radar$angular_axis$rotation)) {
      module_config$visualization$radar$angular_axis$rotation
    } else { 90 } # ✅

    cfg_legend_orientation <- if (!is.null(module_config$visualization$radar$legend$orientation)) {
      module_config$visualization$radar$legend$orientation
    } else { "v" } # ✅

    cfg_legend_x <- if (!is.null(module_config$visualization$radar$legend$position$x)) {
      module_config$visualization$radar$legend$position$x
    } else { 1.1 } # ✅

    cfg_legend_y <- if (!is.null(module_config$visualization$radar$legend$position$y)) {
      module_config$visualization$radar$legend$position$y
    } else { 0.5 } # ✅

    cfg_ai_max_tokens <- if (!is.null(module_config$ai_insights$max_tokens)) {
      module_config$ai_insights$max_tokens
    } else { 800 } # ✅

    message("📝 Advanced DNA Module Config Loaded: default_brands=", cfg_default_brands,
            ", strength_threshold=", cfg_strength_threshold, ", ai_max_tokens=", cfg_ai_max_tokens)

    # Helper function to get text from reactive lang_texts
    get_lang_text <- function(path, default = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) {
        NULL
      })
      if (is.null(texts)) return(default)

      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          return(default)
        }
      }
      return(result)
    }

    # Dynamic placeholder rendering
    output$no_data_placeholder <- renderUI({
      has_data <- !is.null(data_full()) && nrow(data_full()) > 0

      if (!has_data) {
        div(
          class = "alert alert-info text-center",
          style = "margin: 50px auto; max-width: 600px;",
          icon("info-circle"), " ",
          strong(get_lang_text("ui.no_data.title", "品牌DNA比較")),
          br(), br(),
          get_lang_text("ui.no_data.message", "請先上傳評論資料以查看品牌DNA比較分析"),
          br(), br(),
          actionButton(ns("goto_upload"),
                      get_lang_text("ui.no_data.upload_button", "前往資料上傳"),
                      class = "btn-primary",
                      icon = icon("upload"))
        )
      }
    })

    # Navigation button handler
    observeEvent(input$goto_upload, {
      showNotification(
        get_lang_text("messages.redirect_to_upload", "請從側邊欄選擇「資料上傳」頁面"),
        type = "message",
        duration = 3
      )
    }, ignoreInit = TRUE)

    # Content area rendering
    output$content_area <- renderUI({
      df <- data_full()
      req(df)

      cat("\n🎨 [Advanced DNA UI] Rendering content area...\n")
      cat("  - data_full() rows:", nrow(df), "\n")

      # Get brands directly for initial choices
      if (!"Variation" %in% names(df)) {
        cat("  ❌ No 'Variation' column found – cannot render brand comparison UI\n")
        return(div(
          class = "alert alert-warning text-center",
          icon("exclamation-triangle"), " ",
          get_lang_text("ui.no_variation_col", "找不到品牌欄位 (Variation)，請確認上一步評分結果")
        ))
      }

      brands <- unique(na.omit(df$Variation))
      cat("  - Initial brands for dropdown:", paste(brands, collapse=", "), "\n")

      if (length(brands) < 2) {
        cat("  ❌ Not enough brands for comparison\n")
        return(div(
          class = "alert alert-info text-center",
          icon("info-circle"), " ",
          get_lang_text("ui.not_enough_brands", "品牌數不足，請至少提供 2 個品牌以進行比較")
        ))
      }

      # Calculate default selection
      num_to_select <- min(cfg_default_brands, length(brands))
      selected_brands <- head(brands, num_to_select)
      cat("  - Pre-selected brands:", paste(selected_brands, collapse=", "), "\n\n")

      select_brands_label <- get_lang_text("ui.labels.select_brands", "選擇比較品牌")

      tagList(
        selectInput(ns("select_brands"),
                    label = select_brands_label,
                    choices = brands,  # ✅ Initialize with brands directly
                    selected = selected_brands,  # ✅ Pre-select default brands
                    multiple = TRUE),
        plotlyOutput(ns("advanced_dna_plot"), height = "700px"),
        br(),
        uiOutput(ns("dna_insights"))
      )
    })

    # 更新品牌選擇 - Enhanced with isolate
    observe({
      cat("\n🔍 [Advanced DNA] === Brand Selection Update Triggered ===\n")

      # Use isolate to prevent reactive loops, but still react to data changes
      df <- data_full()

      cat("  📊 data_full() call returned:\n")
      cat("    - is.null:", is.null(df), "\n")

      if (is.null(df)) {
        cat("  ❌ data_full is NULL\n")
        return(NULL)
      }

      cat("    - is.data.frame:", is.data.frame(df), "\n")
      cat("    - nrow:", nrow(df), "\n")
      cat("    - ncol:", ncol(df), "\n")

      if (!is.data.frame(df) || nrow(df) == 0) {
        cat("  ❌ data_full is not a dataframe or empty\n")
        return(NULL)
      }

      cat("  ✅ data_full has", nrow(df), "rows,", ncol(df), "cols\n")
      cat("  📋 Column names:", paste(names(df), collapse=", "), "\n")

      if (!"Variation" %in% names(df)) {
        cat("  ❌ No 'Variation' column found!\n")
        return(NULL)
      }

      brands <- unique(df$Variation)
      cat("  ✅ Found", length(brands), "unique brands\n")
      cat("  🏷️  Brand names:", paste(head(brands, 10), collapse=", "), "\n")

      if (length(brands) == 0) {
        cat("  ❌ brands vector is empty!\n")
        return(NULL)
      }

      # Calculate how many to select
      num_to_select <- min(cfg_default_brands, length(brands))
      selected_brands <- head(brands, num_to_select)

      cat("  🎯 Calling updateSelectInput:\n")
      cat("    - inputId: select_brands\n")
      cat("    - choices count:", length(brands), "\n")
      cat("    - selected count:", length(selected_brands), "\n")
      cat("    - selected brands:", paste(selected_brands, collapse=", "), "\n")

      updateSelectInput(session, "select_brands",
                       choices = brands,
                       selected = selected_brands)

      cat("  ✅ updateSelectInput called successfully\n")
      cat("==========================================\n\n")
    })
    
    # 繪製進階DNA圖
    output$advanced_dna_plot <- renderPlotly({
      req(data_full(), input$select_brands)

      if (!"Variation" %in% names(data_full())) {
        return(NULL)
      }
      
      # 篩選選定的品牌
      selected_data <- data_full() %>%
        filter(Variation %in% input$select_brands)

      if (nrow(selected_data) == 0) {
        return(NULL)
      }
      
      # 轉換為長格式
      dta_long <- selected_data %>%
        pivot_longer(
          cols = -c("Variation"),
          names_to = "attribute", 
          values_to = "score"
        )
      
      # 創建雷達圖
      fig <- plot_ly(
        type = 'scatterpolar',
        mode = 'lines+markers',
        fill = 'toself'
      )
      
      # 為每個品牌添加軌跡
      for (brand in unique(dta_long$Variation)) {
        brand_data <- dta_long %>% filter(Variation == brand)
        
        fig <- fig %>%
          add_trace(
            r = brand_data$score,
            theta = brand_data$attribute,
            name = brand,
            text = paste(get_lang_text("plots.brand_label", "品牌:"), brand, "<br>",
                        get_lang_text("plots.attribute_label", "屬性:"), brand_data$attribute, "<br>",
                        get_lang_text("plots.score_label", "分數:"), round(brand_data$score, 2)),
            hoverinfo = "text"
          )
      }
      
      # 設定布局 ✅ 使用配置參數
      fig <- fig %>%
        layout(
          title = list(
            text = get_lang_text("plots.dna_comparison_title", "品牌DNA多維度比較"),
            font = list(size = cfg_radar_font_size) # ✅
          ),
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = cfg_radar_range, # ✅
              tickmode = "linear",
              tick0 = 0,
              dtick = 1,
              ticktext = c("0", "1", "2", "3", "4", "5"),
              tickvals = c(0, 1, 2, 3, 4, 5)
            ),
            angularaxis = list(
              direction = cfg_radar_direction, # ✅
              rotation = cfg_radar_rotation # ✅
            )
          ),
          showlegend = TRUE,
          legend = list(
            orientation = cfg_legend_orientation, # ✅
            x = cfg_legend_x, # ✅
            y = cfg_legend_y # ✅
          )
        )
      
      fig
    })
    outputOptions(output, "advanced_dna_plot", suspendWhenHidden = FALSE)

    # DNA洞察分析（改用 reactiveVal + observeEvent，避免頻繁重算觸發 progress 錯誤）
    dna_insights_content <- reactiveVal(NULL)

    observeEvent(list(data_full(), input$select_brands), ignoreNULL = TRUE, {
      req(data_full(), input$select_brands)

      selected_data <- data_full() %>%
        { if ("Variation" %in% names(.)) . else dplyr::rename(., Variation = 1) } %>%
        filter(Variation %in% input$select_brands)

      if (nrow(selected_data) < cfg_min_brands) {
        min_brands_msg <- get_lang_text("messages.min_brands_required", "請選擇至少2個品牌進行比較")
        dna_insights_content(HTML(paste0("<p>", min_brands_msg, "</p>")))
        return()
      }

      attributes <- names(selected_data)[-1]

      attr_variance <- selected_data %>%
        select(-Variation) %>%
        summarise(across(everything(), var, na.rm = TRUE)) %>%
        pivot_longer(everything(), names_to = "attribute", values_to = "variance") %>%
        arrange(desc(variance))

      top_diff_attrs <- head(attr_variance, 5)

      brand_avg_scores <- selected_data %>%
        pivot_longer(-Variation, names_to = "attribute", values_to = "score") %>%
        group_by(Variation) %>%
        summarise(avg_score = mean(score, na.rm = TRUE), .groups = 'drop') %>%
        arrange(desc(avg_score))

      leading_brand <- brand_avg_scores$Variation[1]

      strength_col <- get_lang_text("analysis.strength_attrs", "強項屬性")
      strength_count_col <- get_lang_text("analysis.strength_count", "強項數量")
      brand_strengths <- selected_data %>%
        pivot_longer(-Variation, names_to = "attribute", values_to = "score") %>%
        filter(score >= cfg_strength_threshold) %>%
        group_by(Variation) %>%
        summarise(
          !!strength_col := paste(head(attribute[order(score, decreasing = TRUE)], cfg_top_count), collapse = "、"),
          !!strength_count_col := n(),
          .groups = 'drop'
        )

      weakness_col <- get_lang_text("analysis.weakness_attrs", "弱項屬性")
      weakness_count_col <- get_lang_text("analysis.weakness_count", "弱項數量")

      market_avg_by_attr <- selected_data %>%
        pivot_longer(-Variation, names_to = "attribute", values_to = "score") %>%
        group_by(attribute) %>%
        summarise(market_avg = mean(score, na.rm = TRUE), .groups = 'drop')

      brand_weaknesses <- selected_data %>%
        pivot_longer(-Variation, names_to = "attribute", values_to = "score") %>%
        left_join(market_avg_by_attr, by = "attribute") %>%
        filter(score < market_avg) %>%
        group_by(Variation) %>%
        summarise(
          !!weakness_col := paste(head(attribute[order(score)], cfg_top_count), collapse = "、"),
          !!weakness_count_col := n(),
          .groups = 'drop'
        )

      better_perf_label <- get_lang_text("analysis.better_performance", "表現較佳：")
      needs_improve_label <- get_lang_text("analysis.needs_improvement", "待改進：")
      no_advantage_label <- get_lang_text("analysis.no_advantage", "無明顯優勢")
      balanced_label <- get_lang_text("analysis.balanced", "表現均衡")

      brands_detail <- ""
      for (i in 1:nrow(selected_data)) {
        brand_name <- selected_data$Variation[i]

        strengths <- brand_strengths %>% filter(Variation == brand_name)
        weaknesses <- brand_weaknesses %>% filter(Variation == brand_name)

        brand_scores <- selected_data %>%
          filter(Variation == brand_name) %>%
          select(-Variation) %>%
          pivot_longer(everything(), names_to = "attribute", values_to = "score") %>%
          arrange(desc(score))

        top_attrs <- head(brand_scores, 3)

        strength_attrs <- if(nrow(strengths) > 0 && nchar(strengths[[strength_col]]) > 0) {
          strengths[[strength_col]]
        } else {
          no_advantage_label
        }

        weakness_attrs <- if(nrow(weaknesses) > 0 && nchar(weaknesses[[weakness_col]]) > 0) {
          weaknesses[[weakness_col]]
        } else {
          balanced_label
        }

        brands_detail <- paste0(brands_detail,
          "<li><strong>", brand_name, "</strong>：<br>",
          "　", better_perf_label, " ",
          strength_attrs,
          "（", paste(round(top_attrs$score, 2), collapse = "、"), "分）<br>",
          "　", needs_improve_label, " ",
          weakness_attrs,
          "</li>"
        )
      }

      ai_insights <- tryCatch({
        data_for_ai <- selected_data %>%
          pivot_longer(-Variation, names_to = "attribute", values_to = "score") %>%
          group_by(Variation) %>%
          summarise(
            avg_score = mean(score, na.rm = TRUE),
            top_attrs = paste(attribute[order(score, decreasing = TRUE)][1:3], collapse = "、"),
            weak_attrs = paste(attribute[order(score)][1:3], collapse = "、"),
            .groups = 'drop'
          )
        
        current_lang <- get_current_language()
        message("🌐 [Advanced DNA Analysis] Using language: ", current_lang)

        messages <- prepare_gpt_messages(
          var_id = "dna_insights_analysis",
          variables = list(
            brands_data = toJSON(data_for_ai),
            top_diff_attrs = paste(top_diff_attrs$attribute[1:3], collapse = "、")
          ),
          language = current_lang,
          prompts_df = load_prompts(language = current_lang, app_name = "brandedge")
        )

        ai_response <- chat_api(messages = messages)

        if (is.null(ai_response) || length(ai_response) == 0 || nchar(trimws(as.character(ai_response))) == 0) {
          stop("AI response empty")
        }

        ai_text <- paste(ai_response, collapse = "\n")

        tryCatch(
          markdownToHTML(text = ai_text, fragment.only = TRUE),
          error = function(e_md) {
            message("⚠️ [Advanced DNA] markdownToHTML failed: ", e_md$message)
            paste0("<p>", htmltools::htmlEscape(ai_text), "</p>")
          }
        )
      }, error = function(e) {
        strategy_title <- get_lang_text("analysis.strategy_title", "策略建議：")
        strategy_1 <- get_lang_text("analysis.strategy_leader", "領先品牌應持續強化優勢屬性，建立更高的競爭門檻")
        strategy_2 <- get_lang_text("analysis.strategy_challenger", "挑戰者品牌應聚焦差異化定位，避免正面競爭")
        strategy_3 <- get_lang_text("analysis.strategy_market", "關注市場變化趨勢，及時調整產品策略")

        paste0(
          "<h6>", strategy_title, "</h6>",
          "<ul>",
          "<li>", strategy_1, "</li>",
          "<li>", strategy_2, "</li>",
          "<li>", strategy_3, "</li>",
          "</ul>"
        )
      })

      insights_title <- get_lang_text("analysis.dna_insights_title", "品牌DNA分析洞察：")
      leading_brand_label <- get_lang_text("analysis.leading_brand", "領先品牌：")
      avg_score_label <- get_lang_text("analysis.avg_score", "平均分數：")
      diff_attrs_label <- get_lang_text("analysis.diff_attributes", "差異化最大的屬性：")
      brand_count_label <- get_lang_text("analysis.brand_count", "品牌數量：")
      analysis_dims_label <- get_lang_text("analysis.analysis_dimensions", "分析維度：")
      attrs_suffix <- get_lang_text("analysis.attributes_suffix", "個屬性")
      brand_detail_title <- get_lang_text("analysis.brand_detail_title", "各品牌表現詳情：")
      ai_insights_title <- get_lang_text("analysis.ai_insights_title", "AI深入洞察與策略建議：")

      dna_insights_content(HTML(paste0(
        "<div class='dna-insights'>",
        "<h5>", insights_title, "</h5>",
        "<ul>",
        "<li><strong>", leading_brand_label, "</strong>", leading_brand,
        "（", avg_score_label, round(brand_avg_scores$avg_score[1], 2), "）</li>",
        "<li><strong>", diff_attrs_label, "</strong>",
        paste(top_diff_attrs$attribute[1:3], collapse = "、"), "</li>",
        "<li><strong>", brand_count_label, "</strong>", length(input$select_brands), "個</li>",
        "<li><strong>", analysis_dims_label, "</strong>", length(attributes), attrs_suffix, "</li>",
        "</ul>",
        "<h6>", brand_detail_title, "</h6>",
        "<ul>", brands_detail, "</ul>",
        "<div class='ai-insights-section'>",
        "<h6>", ai_insights_title, "</h6>",
        ai_insights,
        "</div>",
        "</div>"
      )))
    })

    output$dna_insights <- renderUI({
      req(dna_insights_content())
      dna_insights_content()
    })
    outputOptions(output, "dna_insights", suspendWhenHidden = FALSE)
  })
}


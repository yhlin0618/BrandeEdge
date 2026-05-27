# BrandEdge Brand Identity Module
# 品牌識別度建構策略模組
# Version: 2.0
# Last Updated: 2025-10-07
# Framework: Following InsightForge pattern with language support

# NULL coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ========== 5. 品牌識別度建構策略模組 ==========

brandIdentityModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
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
  no_data_title <- get_lang_value("ui.no_data.title", "品牌識別度建構")
  no_data_message <- get_lang_value("ui.no_data.message", "請先上傳評論資料以進行品牌識別度建構分析")
  upload_button <- get_lang_value("ui.no_data.upload_button", "前往資料上傳")

  select_brand_label <- get_lang_value("ui.labels.select_target_brand", "選擇目標品牌")
  generate_btn <- get_lang_value("ui.buttons.generate_identity", "生成識別度策略")

  tagList(
    # Note: Page title is handled by app_brandedge.R, not here

    # Dynamic placeholder (rendered in Server function)
    uiOutput(ns("no_data_placeholder")),

    # Content area (shown when data exists)
    uiOutput(ns("content_area"))
  )
}

brandIdentityModuleServer <- function(id, data, key_vars, lang_texts = reactive(NULL), module_config = NULL, api_config = NULL) {
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
    # Load all configuration values with fallback defaults
    cfg_unique_top_n <- if (!is.null(module_config$uniqueness_analysis$top_n)) {
      module_config$uniqueness_analysis$top_n
    } else { 5 } # ✅

    cfg_diff_decimal <- if (!is.null(module_config$uniqueness_analysis$diff_score_decimal)) {
      module_config$uniqueness_analysis$diff_score_decimal
    } else { 3 } # ✅

    cfg_target_marker_size <- if (!is.null(module_config$visualization$matrix$marker_size$target)) {
      module_config$visualization$matrix$marker_size$target
    } else { 20 } # ✅

    cfg_other_marker_size <- if (!is.null(module_config$visualization$matrix$marker_size$other)) {
      module_config$visualization$matrix$marker_size$other
    } else { 10 } # ✅

    cfg_target_color <- if (!is.null(module_config$visualization$matrix$colors$target)) {
      module_config$visualization$matrix$colors$target
    } else { "red" } # ✅

    cfg_other_color <- if (!is.null(module_config$visualization$matrix$colors$other)) {
      module_config$visualization$matrix$colors$other
    } else { "blue" } # ✅

    # Load quadrant positions
    cfg_annotations <- module_config$visualization$matrix$quadrant_positions
    if (!is.null(cfg_annotations)) {
      high_high_pos <- cfg_annotations$high_identity_high_strength$position
      low_high_pos <- cfg_annotations$low_identity_high_strength$position
      high_low_pos <- cfg_annotations$high_identity_low_strength$position
      low_low_pos <- cfg_annotations$low_identity_low_strength$position
      high_high_font <- cfg_annotations$high_identity_high_strength$font_size %||% 12
      low_high_font <- cfg_annotations$low_identity_high_strength$font_size %||% 12
      high_low_font <- cfg_annotations$high_identity_low_strength$font_size %||% 12
      low_low_font <- cfg_annotations$low_identity_low_strength$font_size %||% 12
    } else {
      # Fallback defaults
      high_high_pos <- list(x = 0.75, y = 0.75)
      low_high_pos <- list(x = 0.25, y = 0.75)
      high_low_pos <- list(x = 0.75, y = 0.25)
      low_low_pos <- list(x = 0.25, y = 0.25)
      high_high_font <- 12
      low_high_font <- 12
      high_low_font <- 12
      low_low_font <- 12
    } # ✅

    message("📝 Brand Identity Module Config Loaded: unique_top_n=", cfg_unique_top_n,
            ", diff_decimal=", cfg_diff_decimal, ", target_marker=", cfg_target_marker_size)

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
      has_data <- !is.null(data()) && nrow(data()) > 0

      if (!has_data) {
        div(
          class = "alert alert-info text-center",
          style = "margin: 50px auto; max-width: 600px;",
          icon("info-circle"), " ",
          strong(get_lang_text("ui.no_data.title", "品牌識別策略")),
          br(), br(),
          get_lang_text("ui.no_data.message", "請先上傳評論資料以進行品牌識別度建構分析"),
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
      df <- data()
      req(df)

      cat("\n🎨 [Brand Identity UI] Rendering content area...\n")
      cat("  - data() rows:", nrow(df), "\n")

      # Get brands directly for initial choices
      brands <- unique(df$Variation)
      cat("  - Initial brands for dropdown:", paste(brands, collapse=", "), "\n")
      cat("  - Default selection:", brands[1], "\n\n")

      select_brand_label <- get_lang_text("ui.labels.select_target_brand", "選擇目標品牌")
      generate_btn <- get_lang_text("ui.buttons.generate_identity", "生成識別度策略")

      tagList(
        selectInput(ns("target_brand"),
                    label = select_brand_label,
                    choices = brands,  # ✅ Initialize with brands directly
                    selected = brands[1]),  # ✅ Pre-select first brand
        actionButton(ns("generate_identity"),
                    label = generate_btn,
                    class = "btn-primary"),
        br(), br(),
        uiOutput(ns("identity_strategy")),
        br(),
        plotlyOutput(ns("identity_matrix"), height = "500px"),
        br(),
        uiOutput(ns("chart_interpretation"))
      )
    })

    rv <- reactiveValues(strategy = NULL)

    # 更新品牌選擇 - Enhanced with debugging
    observe({
      cat("\n🔍 [Brand Identity] === Brand Selection Update Triggered ===\n")

      df <- data()

      cat("  📊 data() call returned:\n")
      cat("    - is.null:", is.null(df), "\n")

      if (is.null(df)) {
        cat("  ❌ data() is NULL\n")
        return(NULL)
      }

      cat("    - is.data.frame:", is.data.frame(df), "\n")
      cat("    - nrow:", nrow(df), "\n")
      cat("    - ncol:", ncol(df), "\n")

      if (!is.data.frame(df) || nrow(df) == 0) {
        cat("  ❌ data() is not a dataframe or empty\n")
        return(NULL)
      }

      cat("  ✅ data() has", nrow(df), "rows,", ncol(df), "cols\n")
      cat("  📋 Column names:", paste(names(df), collapse=", "), "\n")

      if (!"Variation" %in% names(df)) {
        cat("  ❌ No 'Variation' column found!\n")
        return(NULL)
      }

      brands <- unique(df$Variation)
      cat("  ✅ Found", length(brands), "unique brands\n")
      cat("  🏷️  Brand names:", paste(brands, collapse=", "), "\n")

      if (length(brands) == 0) {
        cat("  ❌ brands vector is empty!\n")
        return(NULL)
      }

      cat("  🎯 Calling updateSelectInput:\n")
      cat("    - inputId: target_brand\n")
      cat("    - choices count:", length(brands), "\n")
      cat("    - selected:", brands[1], "\n")

      updateSelectInput(session, "target_brand",
                       choices = brands,
                       selected = brands[1])

      cat("  ✅ updateSelectInput called successfully\n")
      cat("==========================================\n\n")
    })
    
    # 生成識別度策略
    observeEvent(input$generate_identity, {
      req(input$target_brand, data(), key_vars())

      progress_msg <- get_lang_text("progress.analyzing_identity", "分析品牌識別度...")
      withProgress(message = progress_msg, value = 0, {
        incProgress(0.2)
        
        brand_data <- data() %>%
          filter(Variation == input$target_brand)
        
        # 分析品牌獨特性
        all_brands_avg <- data() %>%
          select(-Variation) %>%
          summarise(across(everything(), mean, na.rm = TRUE))
        
        brand_avg <- brand_data %>%
          select(-Variation) %>%
          summarise(across(everything(), mean, na.rm = TRUE))
        
        # 計算差異化指數
        differentiation <- abs(brand_avg - all_brands_avg)
        
        # 找出最獨特的屬性
        unique_attrs <- differentiation %>%
          pivot_longer(everything(), names_to = "attribute", values_to = "diff_score") %>%
          arrange(desc(diff_score)) %>%
          head(cfg_unique_top_n) # ✅
        
        incProgress(0.5)

        # 使用語言感知的 prompt 系統生成策略
        current_lang <- get_current_language()
        message("🌐 [Brand Identity Strategy] Using language: ", current_lang)

        messages <- prepare_gpt_messages(
          var_id = "brand_identity_strategy",
          variables = list(
            brand_name = input$target_brand,
            unique_attributes = paste(unique_attrs$attribute, collapse = "、")
          ),
          language = current_lang,
          prompts_df = load_prompts(language = current_lang, app_name = "brandedge")
        )

        strategy_text <- tryCatch(
          chat_api(messages, model = cfg_ai_model),
          error = function(e) {
            strategy_title <- get_lang_text("analysis.identity_strategy_title", "品牌識別度建構策略")
            strategy_1 <- get_lang_text("analysis.identity_strategy_1", "強化視覺識別：設計獨特的品牌標誌和包裝，提升視覺記憶點")
            strategy_2_prefix <- get_lang_text("analysis.identity_strategy_2_prefix", "差異化定位：聚焦於")
            strategy_2_suffix <- get_lang_text("analysis.identity_strategy_2_suffix", "等獨特優勢")
            strategy_3 <- get_lang_text("analysis.identity_strategy_3", "故事行銷：打造品牌故事，建立情感連結")
            strategy_4 <- get_lang_text("analysis.identity_strategy_4", "體驗優化：創造獨特的顧客體驗流程")
            strategy_5 <- get_lang_text("analysis.identity_strategy_5", "社群經營：建立品牌社群，培養忠誠顧客")

            paste0(
              "## ", strategy_title, "\n\n",
              "1. **", strategy_1, "**\n",
              "2. **", strategy_2_prefix, unique_attrs$attribute[1], strategy_2_suffix, "**\n",
              "3. **", strategy_3, "**\n",
              "4. **", strategy_4, "**\n",
              "5. **", strategy_5, "**"
            )
          }
        )
        
        rv$strategy <- list(
          text = strategy_text,
          unique_attrs = unique_attrs,
          brand = input$target_brand
        )
        
        incProgress(1)
      })
    })
    
    # 顯示策略
    output$identity_strategy <- renderUI({
      req(rv$strategy)

      html <- markdownToHTML(text = rv$strategy$text, fragment.only = TRUE)

      unique_attrs_title <- get_lang_text("analysis.unique_attributes_title", "品牌獨特屬性：")
      diff_index_label <- get_lang_text("analysis.diff_index", "差異化指數：")

      HTML(paste0(
        "<div class='identity-strategy'>",
        html,
        "<h5>", unique_attrs_title, "</h5>",
        "<ul>",
        paste0(
          "<li>", rv$strategy$unique_attrs$attribute,
          "（", diff_index_label, round(rv$strategy$unique_attrs$diff_score, cfg_diff_decimal), "）</li>", # ✅
          collapse = ""
        ),
        "</ul>",
        "</div>"
      ))
    })
    
    # 識別度矩陣圖
    output$identity_matrix <- renderPlotly({
      req(data())
      df <- data()

      # 取得屬性欄位（排除 Variation 和非數值欄位）
      attr_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(attr_cols, c("Variation"))

      # 如果沒有屬性欄位，返回空白圖表
      if (length(attr_cols) == 0) {
        no_scoring_title <- get_lang_text("plots.no_scoring_title", "請先完成屬性評分")
        return(plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = no_scoring_title, font = list(size = 16)),
            xaxis = list(title = ""),
            yaxis = list(title = "")
          ))
      }

      # 計算每個品牌的識別度指標
      identity_scores <- df %>%
        group_by(Variation) %>%
        summarise(
          avg_score = mean(c_across(all_of(attr_cols)), na.rm = TRUE),
          variance = var(c_across(all_of(attr_cols)), na.rm = TRUE),
          .groups = 'drop'
        ) %>%
        mutate(
          uniqueness = variance,  # 變異度代表獨特性
          strength = avg_score     # 平均分代表實力
        )
      
      # 標記目標品牌
      target_brand_label <- get_lang_text("plots.target_brand", "目標品牌")
      other_brands_label <- get_lang_text("plots.other_brands", "其他品牌")

      if (!is.null(input$target_brand)) {
        identity_scores <- identity_scores %>%
          mutate(
            is_target = ifelse(Variation == input$target_brand, target_brand_label, other_brands_label),
            marker_size = ifelse(Variation == input$target_brand, cfg_target_marker_size, cfg_other_marker_size) # ✅
          )
      } else {
        identity_scores$is_target <- other_brands_label
        identity_scores$marker_size <- cfg_other_marker_size # ✅
      }

      matrix_title <- get_lang_text("plots.identity_matrix_title", "品牌識別度定位矩陣")
      uniqueness_label <- get_lang_text("axes.uniqueness", "獨特性（識別度）")
      strength_label <- get_lang_text("axes.brand_strength", "品牌實力")

      quadrant_high_high <- get_lang_text("quadrants.high_identity_high_strength", "高識別高實力")
      quadrant_low_high <- get_lang_text("quadrants.low_identity_high_strength", "低識別高實力")
      quadrant_high_low <- get_lang_text("quadrants.high_identity_low_strength", "高識別低實力")
      quadrant_low_low <- get_lang_text("quadrants.low_identity_low_strength", "低識別低實力")

      colors_mapping <- setNames(c(cfg_target_color, cfg_other_color), c(target_brand_label, other_brands_label))

      # 計算數據的中心點作為象限分界
      center_x <- mean(identity_scores$uniqueness, na.rm = TRUE)
      center_y <- mean(identity_scores$strength, na.rm = TRUE)

      # 獲取軸的範圍
      x_range <- range(identity_scores$uniqueness, na.rm = TRUE)
      y_range <- range(identity_scores$strength, na.rm = TRUE)
      x_margin <- diff(x_range) * 0.1
      y_margin <- diff(y_range) * 0.1
      x_lim <- c(x_range[1] - x_margin, x_range[2] + x_margin)
      y_lim <- c(y_range[1] - y_margin, y_range[2] + y_margin)

      # 添加四個象限的背景色塊和分隔線
      quadrant_shapes <- list(
        # 背景色塊
        # 左下：低識別低實力（灰色）
        list(type = "rect", x0 = x_lim[1], x1 = center_x, y0 = y_lim[1], y1 = center_y,
             fillcolor = "rgba(128,128,128,0.06)", line = list(width = 0), layer = "below"),
        # 左上：低識別高實力（橘色）
        list(type = "rect", x0 = x_lim[1], x1 = center_x, y0 = center_y, y1 = y_lim[2],
             fillcolor = "rgba(255,165,0,0.08)", line = list(width = 0), layer = "below"),
        # 右上：高識別高實力（綠色）
        list(type = "rect", x0 = center_x, x1 = x_lim[2], y0 = center_y, y1 = y_lim[2],
             fillcolor = "rgba(34,139,34,0.1)", line = list(width = 0), layer = "below"),
        # 右下：高識別低實力（紫色）
        list(type = "rect", x0 = center_x, x1 = x_lim[2], y0 = y_lim[1], y1 = center_y,
             fillcolor = "rgba(147,112,219,0.08)", line = list(width = 0), layer = "below"),

        # 象限分隔線
        # 垂直線 (x = center_x)
        list(type = "line", x0 = center_x, x1 = center_x, y0 = y_lim[1], y1 = y_lim[2],
             line = list(color = "rgba(100,100,100,0.5)", width = 2, dash = "dash"), layer = "above"),
        # 水平線 (y = center_y)
        list(type = "line", x0 = x_lim[1], x1 = x_lim[2], y0 = center_y, y1 = center_y,
             line = list(color = "rgba(100,100,100,0.5)", width = 2, dash = "dash"), layer = "above")
      )

      plot_ly(identity_scores,
              x = ~uniqueness,
              y = ~strength,
              text = ~Variation,
              color = ~is_target,
              type = 'scatter',
              mode = 'markers',
              colors = colors_mapping,
              marker = list(
                size = ~marker_size,
                sizemode = 'diameter',
                sizeref = 1,
                sizemin = 8,
                line = list(color = 'white', width = 2),
                opacity = 0.85
              ),
              hovertext = ~paste0(
                "<b>", Variation, "</b><br>",
                "━━━━━━━━━━━━━━<br>",
                get_lang_text("axes.uniqueness", "獨特性（識別度）"), ": ",
                round(uniqueness, 2), "<br>",
                get_lang_text("axes.brand_strength", "品牌實力"), ": ",
                round(strength, 2)
              ),
              hoverinfo = "text") %>%
        # 只為重要品牌（較大的氣泡）添加文字標籤
        add_text(
          data = identity_scores %>% filter(marker_size > 15),  # 只標記重要品牌
          x = ~uniqueness,
          y = ~strength,
          text = ~Variation,
          textposition = 'top center',
          textfont = list(
            size = 10,
            family = "Arial, sans-serif",
            color = "rgba(0,0,0,0.7)"
          ),
          showlegend = FALSE,
          hoverinfo = "skip"
        ) %>%
        layout(
          title = list(
            text = matrix_title,
            font = list(size = 18, family = "Arial, sans-serif"),
            y = 0.97,
            yanchor = 'top'
          ),
          xaxis = list(
            title = paste0(uniqueness_label, " →"),
            range = x_lim,
            zeroline = TRUE,
            zerolinewidth = 1.5,
            zerolinecolor = 'rgba(100,100,100,0.3)',
            gridcolor = 'rgba(200,200,200,0.4)',
            showgrid = TRUE
          ),
          yaxis = list(
            title = paste0("↑ ", strength_label),
            range = y_lim,
            zeroline = TRUE,
            zerolinewidth = 1.5,
            zerolinecolor = 'rgba(100,100,100,0.3)',
            gridcolor = 'rgba(200,200,200,0.4)',
            showgrid = TRUE
          ),
          shapes = quadrant_shapes,  # 添加象限背景
          showlegend = TRUE,
          legend = list(
            x = 1.02,
            y = 1,
            xanchor = 'left',
            yanchor = 'top',
            bgcolor = 'rgba(255,255,255,0.9)',
            bordercolor = 'rgba(200,200,200,0.5)',
            borderwidth = 1
          ),
          hovermode = 'closest',
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(250,250,250,1)',
          margin = list(t = 80, b = 60, l = 80, r = 140)
          # 移除所有 annotations - 依靠背景顏色和下方說明
        )
    })

    # 圖表解讀說明
    output$chart_interpretation <- renderUI({
      # Helper function to convert markdown bold to HTML
      md_to_html <- function(text) {
        if (is.null(text) || text == "") return(text)
        # Use perl=TRUE for proper Unicode/multibyte character support (Japanese, Chinese, etc.)
        text <- gsub("\\*\\*([^*]+?)\\*\\*", "<strong>\\1</strong>", text, perl = TRUE)
        return(text)
      }

      interp_title <- md_to_html(get_lang_text("chart_interpretation.title", "📊 品牌識別度矩陣圖解讀"))
      interp_overview <- md_to_html(get_lang_text("chart_interpretation.overview", "此圖表展示品牌在市場中的識別度定位。"))

      axes_title <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.title", "座標軸說明："))
      x_axis_desc <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.x_axis", "橫軸說明"))
      y_axis_desc <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.y_axis", "縱軸說明"))

      marker_title <- md_to_html(get_lang_text("chart_interpretation.marker_explanation.title", "標記說明："))
      marker_target <- md_to_html(get_lang_text("chart_interpretation.marker_explanation.target", "目標品牌"))
      marker_others <- md_to_html(get_lang_text("chart_interpretation.marker_explanation.others", "其他品牌"))
      marker_labels <- md_to_html(get_lang_text("chart_interpretation.marker_explanation.labels", "品牌標籤"))

      quadrants_title <- md_to_html(get_lang_text("chart_interpretation.quadrants.title", "四象限策略定位："))
      quad_high_high <- md_to_html(get_lang_text("chart_interpretation.quadrants.high_high", "高識別高實力"))
      quad_low_high <- md_to_html(get_lang_text("chart_interpretation.quadrants.low_high", "低識別高實力"))
      quad_high_low <- md_to_html(get_lang_text("chart_interpretation.quadrants.high_low", "高識別低實力"))
      quad_low_low <- md_to_html(get_lang_text("chart_interpretation.quadrants.low_low", "低識別低實力"))

      how_to_use <- md_to_html(get_lang_text("chart_interpretation.how_to_use", "策略建議"))

      HTML(paste0(
        "<div class='alert alert-success' style='margin-top: 20px; padding: 20px; background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); color: white; border: none; border-radius: 10px;'>",
        "<h4 style='color: white; margin-top: 0;'>", interp_title, "</h4>",
        "<p style='font-size: 15px; margin-bottom: 15px;'>", interp_overview, "</p>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", axes_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", x_axis_desc, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", y_axis_desc, "</p>",
        "</div>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", marker_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", marker_target, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", marker_others, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", marker_labels, "</p>",
        "</div>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", quadrants_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_high_high, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_low_high, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_high_low, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", quad_low_low, "</p>",
        "</div>",

        "<p style='font-size: 15px; margin-top: 15px; margin-bottom: 0; padding: 12px; background: rgba(255,255,255,0.2); border-radius: 6px; border-left: 4px solid #ffd700;'>",
        "<strong>💡 ", how_to_use, "</strong>",
        "</p>",
        "</div>"
      ))
    })
  })
}


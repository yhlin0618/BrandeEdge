# BrandEdge Market Track Module
# 市場賽道分析模組
# Version: 2.0
# Last Updated: 2025-10-07
# Framework: Following InsightForge pattern with language support

# NULL coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ========== 2. 市場賽道分析模組 ==========

marketTrackModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
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
  no_data_title <- get_lang_value("ui.no_data.title", "市場賽道分析")
  no_data_message <- get_lang_value("ui.no_data.message", "請先上傳評論資料以查看市場賽道分析")
  upload_button <- get_lang_value("ui.no_data.upload_button", "前往資料上傳")

  tagList(
    # Note: Page title is handled by app_brandedge.R, not here

    # Dynamic placeholder (rendered in Server function)
    uiOutput(ns("no_data_placeholder")),

    # Content area (shown when data exists)
    uiOutput(ns("content_area"))
  )
}

marketTrackModuleServer <- function(id, data, key_vars, lang_texts = reactive(NULL), module_config = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # ========== Configuration Loading ==========
    # Load all configuration values with fallback defaults
    cfg_ideal_value <- if (!is.null(module_config$growth_tracks$ideal_value)) {
      module_config$growth_tracks$ideal_value
    } else { 1.0 } # ✅

    cfg_key_top_n <- if (!is.null(module_config$growth_tracks$key_attributes$top_n)) {
      module_config$growth_tracks$key_attributes$top_n
    } else { 5 } # ✅

    cfg_key_weight <- if (!is.null(module_config$growth_tracks$importance$key_weight)) {
      module_config$growth_tracks$importance$key_weight
    } else { 1.0 } # ✅

    cfg_non_key_weight <- if (!is.null(module_config$growth_tracks$importance$non_key_weight)) {
      module_config$growth_tracks$importance$non_key_weight
    } else { 0.5 } # ✅

    cfg_bubble_multiplier <- if (!is.null(module_config$visualization$quadrant$bubble_size_multiplier)) {
      module_config$visualization$quadrant$bubble_size_multiplier
    } else { 50 } # ✅

    cfg_color_scale <- if (!is.null(module_config$visualization$quadrant$color_scale)) {
      module_config$visualization$quadrant$color_scale
    } else { "Viridis" } # ✅

    cfg_x_range <- if (!is.null(module_config$visualization$quadrant$x_axis$range)) {
      module_config$visualization$quadrant$x_axis$range
    } else { c(-0.1, 1.1) } # ✅

    cfg_y_range <- if (!is.null(module_config$visualization$quadrant$y_axis$range)) {
      module_config$visualization$quadrant$y_axis$range
    } else { c(0, 1.2) } # ✅

    cfg_top_tracks <- if (!is.null(module_config$recommendations$top_tracks_count)) {
      module_config$recommendations$top_tracks_count
    } else { 5 } # ✅

    # Load annotation positions
    cfg_annotations <- module_config$visualization$quadrant$annotations
    if (!is.null(cfg_annotations)) {
      high_pos <- cfg_annotations$high_potential$position
      mature_pos <- cfg_annotations$mature$position
      niche_pos <- cfg_annotations$niche$position
      low_pos <- cfg_annotations$low_priority$position
      high_font_size <- cfg_annotations$high_potential$font_size %||% 14
      mature_font_size <- cfg_annotations$mature$font_size %||% 14
      niche_font_size <- cfg_annotations$niche$font_size %||% 14
      low_font_size <- cfg_annotations$low_priority$font_size %||% 14
    } else {
      # Fallback defaults
      high_pos <- list(x = 0.75, y = 0.75)
      mature_pos <- list(x = 0.25, y = 0.75)
      niche_pos <- list(x = 0.75, y = 0.25)
      low_pos <- list(x = 0.25, y = 0.25)
      high_font_size <- 14
      mature_font_size <- 14
      niche_font_size <- 14
      low_font_size <- 14
    } # ✅

    message("📝 Market Track Module Config Loaded: ideal_value=", cfg_ideal_value,
            ", key_top_n=", cfg_key_top_n, ", bubble_multiplier=", cfg_bubble_multiplier)

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
          strong(get_lang_text("ui.no_data.title", "市場賽道分析")),
          br(), br(),
          get_lang_text("ui.no_data.message", "請先上傳評論資料以查看市場賽道分析"),
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
      req(data())

      tagList(
        plotlyOutput(ns("growth_track_plot"), height = "500px"),
        br(),
        uiOutput(ns("chart_interpretation")),
        br(),
        uiOutput(ns("track_recommendations"))
      )
    })

    # 分析成長賽道
    growth_tracks <- reactive({
      req(data())
      df <- data()

      # 如果 key_vars 為 NULL，自動從 data 中提取所有數值型屬性（排除 Variation）
      keys <- tryCatch({
        k <- key_vars()
        if (is.null(k) || length(k) == 0) {
          # 自動提取：所有數值型欄位，排除 Variation
          numeric_cols <- names(df)[sapply(df, is.numeric)]
          setdiff(numeric_cols, c("Variation"))
        } else {
          k
        }
      }, error = function(e) {
        # 如果 key_vars() 出錯，自動提取
        numeric_cols <- names(df)[sapply(df, is.numeric)]
        setdiff(numeric_cols, c("Variation"))
      })

      # 確保有屬性可以分析
      if (length(keys) == 0) {
        return(NULL)
      }

      # 計算各屬性的成長潛力
      growth_data <- df %>%
        select(all_of(keys)) %>%
        summarise_all(mean, na.rm = TRUE) %>%
        pivot_longer(everything(), names_to = "attribute", values_to = "current_performance") %>%
        arrange(desc(current_performance)) %>%  # 按當前表現排序
        mutate(
          # 檢查數據範圍，自動判斷是 1-5 還是 0-1 scale
          data_scale = if_else(max(current_performance, na.rm = TRUE) > 2, "1-5", "0-1"),
          # 標準化到 0-1 scale
          current_norm = if_else(data_scale == "1-5",
                                  (current_performance - 1) / 4,  # 1-5 -> 0-1
                                  current_performance),           # already 0-1
          # 計算成長潛力 (ideal = 1.0 on normalized scale)
          growth_potential = pmax(0, 1.0 - current_norm),
          # 市場重要性（使用關鍵因素權重） ✅
          # 前 cfg_key_top_n 個屬性（按當前表現排序）視為關鍵因素
          rank = row_number(),
          is_key_factor = rank <= cfg_key_top_n,
          importance = ifelse(is_key_factor, cfg_key_weight, cfg_non_key_weight),
          # 成長賽道得分
          track_score = growth_potential * importance
        ) %>%
        arrange(desc(track_score))  # 最後按賽道得分排序

      # Debug log
      cat("\n=== Market Track Debug ===\n")
      cat("Data scale detected:", unique(growth_data$data_scale), "\n")
      cat("Growth potential range:", range(growth_data$growth_potential, na.rm = TRUE), "\n")
      cat("Track score range:", range(growth_data$track_score, na.rm = TRUE), "\n")
      cat("Bubble sizes (track_score * multiplier):",
          range(growth_data$track_score * cfg_bubble_multiplier, na.rm = TRUE), "\n")

      growth_data
    })
    
    # 成長賽道視覺化
    output$growth_track_plot <- renderPlotly({
      tracks <- growth_tracks()
      req(tracks, nrow(tracks) > 0)  # Ensure data is available

      # 添加四個象限的背景色塊和分隔線
      quadrant_shapes <- list(
        # 背景色塊
        # 左下：低優先賽道（灰色）
        list(type = "rect", x0 = -0.1, x1 = 0.5, y0 = 0, y1 = 0.5,
             fillcolor = "rgba(128,128,128,0.08)", line = list(width = 0), layer = "below"),
        # 左上：成熟賽道（藍色）
        list(type = "rect", x0 = -0.1, x1 = 0.5, y0 = 0.5, y1 = 1.2,
             fillcolor = "rgba(70,130,180,0.08)", line = list(width = 0), layer = "below"),
        # 右上：高潛力賽道（綠色）
        list(type = "rect", x0 = 0.5, x1 = 1.1, y0 = 0.5, y1 = 1.2,
             fillcolor = "rgba(34,139,34,0.12)", line = list(width = 0), layer = "below"),
        # 右下：利基賽道（橘色）
        list(type = "rect", x0 = 0.5, x1 = 1.1, y0 = 0, y1 = 0.5,
             fillcolor = "rgba(255,140,0,0.08)", line = list(width = 0), layer = "below"),

        # 象限分隔線
        # 垂直線 (x = 0.5)
        list(type = "line", x0 = 0.5, x1 = 0.5, y0 = 0, y1 = 1.2,
             line = list(color = "rgba(100,100,100,0.5)", width = 2, dash = "dash"), layer = "above"),
        # 水平線 (y = 0.5)
        list(type = "line", x0 = -0.1, x1 = 1.1, y0 = 0.5, y1 = 0.5,
             line = list(color = "rgba(100,100,100,0.5)", width = 2, dash = "dash"), layer = "above")
      )

      plot_ly(tracks,
              x = ~growth_potential,
              y = ~importance,
              type = 'scatter',
              mode = 'markers+text',  # 顯示氣泡和屬性標籤
              text = ~attribute,
              textposition = "middle center",
              textfont = list(size = 8, color = "white"),
              marker = list(
                size = ~track_score * cfg_bubble_multiplier,
                color = ~track_score,
                colorscale = cfg_color_scale,
                showscale = TRUE,
                colorbar = list(
                  title = get_lang_text("plots.track_potential", "賽道潛力"),
                  x = 1.02
                ),
                line = list(color = 'white', width = 2),
                opacity = 0.85
              ),
              # 詳細的 hover 信息
              hovertext = ~paste0(
                "<b>", attribute, "</b><br>",
                "━━━━━━━━━━━━━━<br>",
                get_lang_text("stats_labels.growth_potential", "成長潛力"), ": ",
                round(growth_potential * 100, 1), "%<br>",
                get_lang_text("stats_labels.importance", "重要性"), ": ",
                round(importance * 100, 0), "%<br>",
                get_lang_text("plots.track_potential", "賽道潛力"), ": ",
                round(track_score, 3)
              ),
              hoverinfo = "text") %>%
        layout(
          title = list(
            text = get_lang_text("plots.growth_track_title", "市場成長賽道機會圖"),
            font = list(size = 18, family = "Arial, sans-serif"),
            y = 0.97,
            yanchor = 'top'
          ),
          xaxis = list(
            title = get_lang_text("axes.growth_potential", "成長潛力 →"),
            range = cfg_x_range,
            gridcolor = 'rgba(200,200,200,0.4)',
            showgrid = TRUE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = get_lang_text("axes.market_importance", "↑ 市場重要性"),
            range = cfg_y_range,
            gridcolor = 'rgba(200,200,200,0.4)',
            showgrid = TRUE,
            zeroline = FALSE
          ),
          shapes = quadrant_shapes,  # 添加四個象限背景
          showlegend = FALSE,
          hovermode = 'closest',
          plot_bgcolor = 'rgba(255,255,255,1)',
          paper_bgcolor = 'rgba(250,250,250,1)',
          margin = list(t = 80, b = 60, l = 80, r = 120),
          # 只保留中心十字線標註
          annotations = list(
            # 中心參考線標註（可選）
            list(x = 0.5, y = -0.05, xref = "x", yref = "paper",
                 text = "← 低成長 | 高成長 →",
                 showarrow = FALSE,
                 font = list(size = 10, color = "gray", family = "Arial"),
                 opacity = 0.6)
          )
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

      interp_title <- get_lang_text("chart_interpretation.title", "📊 圖表解讀說明")
      interp_overview <- md_to_html(get_lang_text("chart_interpretation.overview", "本圖表呈現各產品屬性的成長機會分析。"))

      axes_title <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.title", "座標軸說明："))
      x_axis_desc <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.x_axis", "橫軸說明"))
      y_axis_desc <- md_to_html(get_lang_text("chart_interpretation.axes_explanation.y_axis", "縱軸說明"))

      bubble_title <- md_to_html(get_lang_text("chart_interpretation.bubble_explanation.title", "氣泡說明："))
      bubble_size <- md_to_html(get_lang_text("chart_interpretation.bubble_explanation.size", "氣泡大小說明"))
      bubble_color <- md_to_html(get_lang_text("chart_interpretation.bubble_explanation.color", "氣泡顏色說明"))
      bubble_position <- md_to_html(get_lang_text("chart_interpretation.bubble_explanation.position", "氣泡位置說明"))

      quadrants_title <- md_to_html(get_lang_text("chart_interpretation.quadrants.title", "四象限策略："))
      quad_high <- md_to_html(get_lang_text("chart_interpretation.quadrants.high_potential", "高潛力賽道"))
      quad_mature <- md_to_html(get_lang_text("chart_interpretation.quadrants.mature", "成熟賽道"))
      quad_niche <- md_to_html(get_lang_text("chart_interpretation.quadrants.niche", "利基賽道"))
      quad_low <- md_to_html(get_lang_text("chart_interpretation.quadrants.low_priority", "低優先賽道"))

      how_to_use <- md_to_html(get_lang_text("chart_interpretation.how_to_use", "如何使用此圖表"))

      HTML(paste0(
        "<div class='alert alert-info' style='margin-top: 20px; padding: 20px; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; border: none; border-radius: 10px;'>",
        "<h4 style='color: white; margin-top: 0;'>", interp_title, "</h4>",
        "<p style='font-size: 15px; margin-bottom: 15px;'>", interp_overview, "</p>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", axes_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", x_axis_desc, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", y_axis_desc, "</p>",
        "</div>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", bubble_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", bubble_size, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", bubble_color, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", bubble_position, "</p>",
        "</div>",

        "<div style='background: rgba(255,255,255,0.15); padding: 15px; border-radius: 8px; margin-bottom: 15px;'>",
        "<h5 style='color: #fff; margin-top: 0;'>", quadrants_title, "</h5>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_high, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_mature, "</p>",
        "<p style='margin-bottom: 8px; line-height: 1.6;'>", quad_niche, "</p>",
        "<p style='margin-bottom: 0; line-height: 1.6;'>", quad_low, "</p>",
        "</div>",

        "<p style='font-size: 15px; margin-top: 15px; margin-bottom: 0; padding: 12px; background: rgba(255,255,255,0.2); border-radius: 6px; border-left: 4px solid #ffd700;'>",
        "<strong>💡 ", how_to_use, "</strong>",
        "</p>",
        "</div>"
      ))
    })

    # 賽道建議
    output$track_recommendations <- renderUI({
      tracks <- growth_tracks()
      top_tracks <- head(tracks, cfg_top_tracks) # ✅

      track_title <- get_lang_text("segments.track_recommendations", "建議成長賽道：")
      growth_label <- get_lang_text("stats_labels.growth_potential", "成長潛力 ")
      importance_label <- get_lang_text("stats_labels.importance", "重要性 ")
      indicators_title <- get_lang_text("help.indicators_title", "指標說明")

      # Get help text sections
      growth_title <- get_lang_text("help.growth_potential.title", "成長潛力")
      growth_desc <- get_lang_text("help.growth_potential.description", "代表該屬性從目前表現提升到理想水準的改善空間。")
      growth_range_80_100 <- get_lang_text("help.growth_potential.range_80_100", "• 80-100%：有極大改善空間，是重要的成長機會")
      growth_range_50_80 <- get_lang_text("help.growth_potential.range_50_80", "• 50-80%：中等改善空間，值得積極投入資源")
      growth_range_20_50 <- get_lang_text("help.growth_potential.range_20_50", "• 20-50%：改善空間有限，需評估投資效益")
      growth_range_0_20 <- get_lang_text("help.growth_potential.range_0_20", "• 0-20%：已接近理想狀態，維持現狀即可")

      importance_title <- get_lang_text("help.market_importance.title", "市場重要性")
      importance_desc <- get_lang_text("help.market_importance.description", "反映該屬性對整體市場競爭力的影響程度。")
      importance_100 <- get_lang_text("help.market_importance.level_100", "• 100%：關鍵屬性，直接影響品牌成敗")
      importance_50 <- get_lang_text("help.market_importance.level_50", "• 50%：重要屬性，對品牌表現有顯著影響")
      importance_note <- get_lang_text("help.market_importance.note", "• 數值越高，代表消費者越重視此屬性")

      quadrant_title <- get_lang_text("help.quadrant.title", "四象限解讀")
      quadrant_high <- get_lang_text("help.quadrant.high_potential", "• 高潛力賽道（右上）：高成長潛力+高重要性 = 最優先投資")
      quadrant_mature <- get_lang_text("help.quadrant.mature", "• 成熟賽道（左上）：低成長潛力+高重要性 = 維持領先優勢")
      quadrant_niche <- get_lang_text("help.quadrant.niche", "• 利基賽道（右下）：高成長潛力+低重要性 = 差異化機會")
      quadrant_low <- get_lang_text("help.quadrant.low_priority", "• 低優先賽道（左下）：低成長潛力+低重要性 = 資源最低配置")

      HTML(paste0(
        "<div class='track-recommendations'>",
        "<h5>", track_title, "</h5>",
        "<ol>",
        paste0(
          "<li><strong>", top_tracks$attribute, "</strong>：",
          growth_label, round(top_tracks$growth_potential * 100, 1), "%，",
          importance_label, round(top_tracks$importance * 100, 0), "%</li>",
          collapse = ""
        ),
        "</ol>",
        "<hr>",
        "<div class='alert alert-info' style='margin-top: 20px; font-size: 15px;'>",
        "<h5><i class='fas fa-info-circle'></i> ", indicators_title, "</h5>",
        "<dl class='row' style='margin-bottom: 0;'>",
        "<dt class='col-sm-3' style='font-size: 15px;'><i class='fas fa-chart-line'></i> ", growth_title, "</dt>",
        "<dd class='col-sm-9' style='font-size: 14px;'>",
        growth_desc,
        "<br><span style='color: #495057; font-size: 13px;'>",
        growth_range_80_100, "<br>",
        growth_range_50_80, "<br>",
        growth_range_20_50, "<br>",
        growth_range_0_20,
        "</span>",
        "</dd>",
        "<dt class='col-sm-3' style='font-size: 15px;'><i class='fas fa-star'></i> ", importance_title, "</dt>",
        "<dd class='col-sm-9' style='font-size: 14px;'>",
        importance_desc,
        "<br><span style='color: #495057; font-size: 13px;'>",
        importance_100, "<br>",
        importance_50, "<br>",
        importance_note,
        "</span>",
        "</dd>",
        "<dt class='col-sm-3' style='font-size: 15px;'><i class='fas fa-bullseye'></i> ", quadrant_title, "</dt>",
        "<dd class='col-sm-9' style='font-size: 14px;'>",
        "<span style='color: #495057;'>",
        "<span class='text-success font-weight-bold'>", quadrant_high, "</span><br>",
        "<span class='text-primary font-weight-bold'>", quadrant_mature, "</span><br>",
        "<span class='text-warning font-weight-bold'>", quadrant_niche, "</span><br>",
        "<span class='text-secondary font-weight-bold'>", quadrant_low, "</span>",
        "</span>",
        "</dd>",
        "</dl>",
        "</div>",
        "</div>"
      ))
    })
  })
}


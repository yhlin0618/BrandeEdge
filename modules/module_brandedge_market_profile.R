# BrandEdge Market Profile Module
# 目標市場輪廓分析模組
# Version: 3.0
# Last Updated: 2025-10-09
# Framework: Following InsightForge pattern with language support
# Phase 1: K-means clustering + 5-tab UI + Plotly visualizations

# Required libraries
library(dplyr)
library(tidyr)
library(plotly)
library(DT)
library(stringr)

# NULL coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

# ========== 1. 目標市場輪廓模組 ==========

marketProfileModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
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
  no_data_title <- get_lang_value("ui.no_data.title", "市場輪廓分析")
  no_data_message <- get_lang_value("ui.no_data.message", "請先上傳評論資料以查看市場輪廓分析")
  upload_button <- get_lang_value("ui.no_data.upload_button", "前往資料上傳")

  tagList(
    # Note: Page title is handled by app_brandedge.R, not here

    # Dynamic placeholder (rendered in Server function)
    uiOutput(ns("no_data_placeholder")),

    # Content area (shown when data exists)
    uiOutput(ns("content_area"))
  )
}

marketProfileModuleServer <- function(id, data, brand_data, lang_texts = reactive(NULL), module_config = NULL, api_config = NULL) {
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

    # Load configuration values from YAML (with fallback defaults)
    cfg_high_score <- if (!is.null(module_config$segmentation$thresholds$high_score)) {
      module_config$segmentation$thresholds$high_score
    } else { 4 }

    cfg_leader_avg <- if (!is.null(module_config$segmentation$thresholds$leader_avg_score)) {
      module_config$segmentation$thresholds$leader_avg_score
    } else { 4.5 }

    cfg_niche_avg <- if (!is.null(module_config$segmentation$thresholds$niche_avg_score)) {
      module_config$segmentation$thresholds$niche_avg_score
    } else { 4.0 }

    cfg_decimal_places <- if (!is.null(module_config$output$tables$decimal_places)) {
      module_config$output$tables$decimal_places
    } else { 2 }

    cfg_use_median <- if (!is.null(module_config$segmentation$statistics$use_median_for_comparison)) {
      module_config$segmentation$statistics$use_median_for_comparison
    } else { TRUE }

    message("📝 [market_profile] 配置載入: high_score=", cfg_high_score,
            ", leader_avg=", cfg_leader_avg,
            ", niche_avg=", cfg_niche_avg)

    # Reactive values for AI analysis results
    ai_results <- reactiveValues(
      segment_names = NULL,        # Named list: segment_id -> AI-generated name
      customer_analysis = NULL,     # HTML/markdown text
      opportunities_analysis = NULL, # HTML/markdown text
      comprehensive_report = NULL,  # HTML/markdown text
      is_loading = FALSE,
      last_updated = NULL
    )

    # Helper function to get text from reactive lang_texts
    get_lang_text <- function(path, default = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) {
        cat("⚠️ [Market Profile get_lang_text ERROR]", path, "- Error:", e$message, "\n")
        NULL
      })

      if (is.null(texts)) {
        cat("⚠️ [Market Profile] No texts for:", path, "- Using default:", default, "\n")
        return(default)
      }

      # Check language
      if (!is.null(texts$language)) {
        cat("🌐 [Market Profile] Path:", path, "| Language:", texts$language, "\n")
      }

      parts <- strsplit(path, "\\.")[[1]]
      result <- texts
      for (part in parts) {
        if (!is.null(result[[part]])) {
          result <- result[[part]]
        } else {
          cat("⚠️ [Market Profile] Path not found:", path, "at part:", part, "- Using default:", default, "\n")
          return(default)
        }
      }

      cat("✅ [Market Profile] Retrieved:", path, "=", as.character(result), "\n")
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
          strong(get_lang_text("ui.no_data.title", "市場輪廓分析")),
          br(), br(),
          get_lang_text("ui.no_data.message", "請先上傳評論資料以查看市場輪廓分析"),
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

    # Content area rendering - NEW: Tabset structure following original
    output$content_area <- renderUI({
      req(data())

      tagList(
        # Analysis control panel
        wellPanel(
          fluidRow(
            column(4,
              selectInput(ns("segmentation_method"),
                         get_lang_text("controls.segmentation_method", "市場區隔方法"),
                         choices = list(
                           "K-means聚類" = "kmeans",
                           "屬性表現" = "performance"
                         ),
                         selected = "kmeans")
            ),
            column(4,
              numericInput(ns("num_segments"),
                          get_lang_text("controls.num_segments", "區隔數量"),
                          value = 4,
                          min = 3,
                          max = 6)
            ),
            column(4,
              actionButton(ns("run_ai_analysis"),
                          get_lang_text("controls.run_ai_analysis", "執行AI分析"),
                          class = "btn-success btn-block",
                          icon = icon("brain"),
                          style = "margin-top: 25px;")
            )
          )
        ),

        # Tabset Panel (5 tabs following original)
        tabsetPanel(
          id = ns("analysis_tabs"),

          # Tab 1: Market Segmentation Overview
          tabPanel(
            title = get_lang_text("tabs.overview", "市場區隔總覽"),
            value = "overview",
            br(),
            fluidRow(
              column(12,
                div(class = "segment-overview",
                    uiOutput(ns("segment_summary_cards"))
                )
              )
            ),
            br(),
            fluidRow(
              column(6,
                plotlyOutput(ns("segment_distribution_plot"), height = "400px")
              ),
              column(6,
                plotlyOutput(ns("segment_characteristics_plot"), height = "400px")
              )
            )
          ),

          # Tab 2: Detailed Segmentation Analysis
          tabPanel(
            title = get_lang_text("tabs.detailed", "市場區隔分析"),
            value = "detailed",
            br(),
            fluidRow(
              column(12,
                h4(get_lang_text("tables.segment_summary", "市場區隔總覽")),
                DTOutput(ns("segment_summary_table"))
              )
            )
          ),

          # Tab 3: Customer Preference Analysis
          tabPanel(
            title = get_lang_text("tabs.preferences", "客群偏好分析"),
            value = "preferences",
            br(),
            fluidRow(
              column(6,
                plotlyOutput(ns("preference_heatmap"), height = "500px")
              ),
              column(6,
                plotlyOutput(ns("preference_radar"), height = "500px")
              )
            ),
            br(),
            fluidRow(
              column(12,
                uiOutput(ns("ai_customer_insights"))
              )
            )
          ),

          # Tab 4: Market Opportunities
          tabPanel(
            title = get_lang_text("tabs.opportunities", "潛在市場機會"),
            value = "opportunities",
            br(),
            fluidRow(
              column(12,
                plotlyOutput(ns("opportunity_matrix"), height = "600px")
              )
            ),
            br(),
            fluidRow(
              column(12,
                uiOutput(ns("ai_opportunities_insights"))
              )
            )
          ),

          # Tab 5: Summary Report
          tabPanel(
            title = get_lang_text("tabs.summary", "分析摘要"),
            value = "summary",
            br(),
            fluidRow(
              column(12,
                uiOutput(ns("analysis_summary_report"))
              )
            )
          )
        )
      )
    })

    # ========== AI Analysis Helper Functions ==========

    # AI Segment Naming Function
    generate_segment_names_ai <- function(segments_data, current_lang) {
      tryCatch({
        # Load prompts
        app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
        prompts_df <- load_prompts(language = current_lang, app_name = app_name)

        # Get segment naming prompt
        segment_naming_prompt <- get_prompt("segment_naming", prompts_df, language = current_lang)

        # Generate names for each segment
        segment_names <- list()

        # Get attribute columns
        attr_cols <- setdiff(names(segments_data), c("Variation", "segment", "cluster",
                                                      "avg_score", "top_attributes",
                                                      "consistency", "market_share",
                                                      "performance_ratio", "competitiveness_score",
                                                      "market_position"))

        for (seg_id in unique(segments_data$segment)) {
          seg_data <- segments_data %>% filter(segment == seg_id)

          # Prepare prompt variables
          avg_score <- mean(seg_data$avg_score, na.rm = TRUE)
          top_attrs <- mean(seg_data$top_attributes, na.rm = TRUE)

          # Get key attributes (top 3)
          key_attrs <- seg_data %>%
            select(all_of(attr_cols)) %>%
            summarise(across(everything(), mean, na.rm = TRUE)) %>%
            pivot_longer(everything(), names_to = "attr", values_to = "score") %>%
            arrange(desc(score)) %>%
            slice(1:3) %>%
            pull(attr) %>%
            paste(collapse = ", ")

          brands_list <- paste(seg_data$Variation, collapse = ", ")
          market_share_pct <- round(nrow(seg_data) / nrow(segments_data) * 100, 1)

          # Fill prompt template
          filled_prompt <- segment_naming_prompt
          filled_prompt <- gsub("\\{avg_score\\}", round(avg_score, 2), filled_prompt)
          filled_prompt <- gsub("\\{top_attributes\\}", round(top_attrs, 1), filled_prompt)
          filled_prompt <- gsub("\\{key_attributes\\}", key_attrs, filled_prompt)
          filled_prompt <- gsub("\\{brands\\}", brands_list, filled_prompt)
          filled_prompt <- gsub("\\{market_share\\}", paste0(market_share_pct, "%"), filled_prompt)

          # Call OpenAI API using chat_api（使用共用 API 設定的模型）
          messages <- list(
            list(role = "system", content = "You are a marketing analyst expert specializing in market segmentation."),
            list(role = "user", content = filled_prompt)
          )
          response <- chat_api(
            messages = messages,
            model = cfg_ai_model
          )

          # Store result
          segment_names[[as.character(seg_id)]] <- response
        }

        return(segment_names)
      }, error = function(e) {
        message("⚠️ AI segment naming error: ", e$message)
        return(NULL)
      })
    }

    # AI Customer Preference Analysis
    generate_customer_analysis_ai <- function(segments_data, current_lang) {
      tryCatch({
        # Load prompts
        app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
        prompts_df <- load_prompts(language = current_lang, app_name = app_name)

        # Get customer preference prompt
        preference_prompt <- get_prompt("customer_preference_analysis", prompts_df, language = current_lang)

        # Prepare segment data summary
        segment_summary <- segments_data %>%
          group_by(segment, market_position) %>%
          summarise(
            brand_count = n(),
            avg_score = mean(avg_score, na.rm = TRUE),
            top_attrs_avg = mean(top_attributes, na.rm = TRUE),
            .groups = 'drop'
          )

        # Convert to readable text
        segment_text <- segment_summary %>%
          mutate(
            desc = paste0("區隔", segment, "（", market_position, "）：",
                         brand_count, "個品牌，平均評分", round(avg_score, 2),
                         "，優勢屬性", round(top_attrs_avg, 1), "個")
          ) %>%
          pull(desc) %>%
          paste(collapse = "\n")

        # Fill prompt
        filled_prompt <- gsub("\\{segment_data\\}", segment_text, preference_prompt)

        # Call OpenAI API using chat_api（使用共用 API 設定的模型）
        messages <- list(
          list(role = "system", content = "You are a marketing analyst expert specializing in customer preference analysis."),
          list(role = "user", content = filled_prompt)
        )
        response <- chat_api(
          messages = messages,
          model = cfg_ai_model
        )

        return(response)
      }, error = function(e) {
        message("⚠️ AI customer analysis error: ", e$message)
        return(NULL)
      })
    }

    # AI Market Opportunities Analysis
    generate_opportunities_analysis_ai <- function(segments_data, current_lang) {
      tryCatch({
        # Load prompts
        app_name <- Sys.getenv("APP_TYPE", unset = "brandedge")
        prompts_df <- load_prompts(language = current_lang, app_name = app_name)

        # Get opportunities prompt
        opportunities_prompt <- get_prompt("market_opportunities_analysis", prompts_df, language = current_lang)

        # Calculate opportunity metrics
        market_data <- segments_data %>%
          group_by(segment) %>%
          summarise(
            size = n(),
            avg_score = mean(avg_score, na.rm = TRUE),
            growth_potential = mean(competitiveness_score, na.rm = TRUE),
            position = first(market_position),
            .groups = 'drop'
          ) %>%
          mutate(
            opportunity_score = case_when(
              size <= 2 & avg_score >= 4.0 ~ "高潛力小眾市場",
              size >= 5 & avg_score < 3.5 ~ "大眾待改善市場",
              growth_potential >= 1.2 ~ "成長機會區隔",
              TRUE ~ "一般市場"
            )
          )

        # Convert to text
        market_text <- market_data %>%
          mutate(
            desc = paste0("區隔", segment, "：", size, "個品牌，評分", round(avg_score, 2),
                         "，", opportunity_score)
          ) %>%
          pull(desc) %>%
          paste(collapse = "\n")

        # Fill prompt
        filled_prompt <- gsub("\\{market_data\\}", market_text, opportunities_prompt)

        # Call OpenAI API using chat_api（使用共用 API 設定的模型）
        messages <- list(
          list(role = "system", content = "You are a marketing analyst expert specializing in market opportunities identification."),
          list(role = "user", content = filled_prompt)
        )
        response <- chat_api(
          messages = messages,
          model = cfg_ai_model
        )

        return(response)
      }, error = function(e) {
        message("⚠️ AI opportunities analysis error: ", e$message)
        return(NULL)
      })
    }

    # ========== K-means Market Segmentation Analysis ==========
    market_segments <- reactive({
      req(data())
      df <- data()

      # 取得所有屬性欄位（只選擇數值型欄位，排除 Variation）
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(numeric_cols, c("Variation"))

      # 如果沒有數值欄位（還沒評分），返回 NULL
      if (length(attr_cols) == 0) {
        return(NULL)
      }

      cat("\n🔍 [Market Profile] Processing brand data...\n")
      cat("  - Input rows:", nrow(df), "(should be brand-level from scoring)\n")
      cat("  - Attribute columns:", length(attr_cols), "\n")
      cat("  - Brands:", paste(df$Variation, collapse=", "), "\n\n")

      # ✅ Data should already be brand-aggregated from scoring module
      # Calculate metrics for each brand
      segments <- df %>%
        mutate(
          avg_score = rowMeans(select(., all_of(attr_cols)), na.rm = TRUE),
          top_attributes = rowSums(select(., all_of(attr_cols)) >= cfg_high_score, na.rm = TRUE),
          consistency = apply(select(., all_of(attr_cols)), 1, sd, na.rm = TRUE)
        )

      # K-means clustering (if enough data)
      if (nrow(segments) >= 3 && input$segmentation_method == "kmeans") {
        # Prepare clustering data
        cluster_data <- segments %>%
          select(avg_score, top_attributes, consistency) %>%
          scale()

        # K-means clustering with user-selected number of segments
        set.seed(123)
        n_clusters <- min(input$num_segments, max(2, floor(nrow(segments)/2)))
        km <- kmeans(cluster_data, centers = n_clusters, nstart = 25)
        segments$cluster <- km$cluster
      } else {
        # Fallback: simple segmentation
        segments$cluster <- 1:nrow(segments)
      }

      # Calculate market position classification
      market_avg <- mean(segments$avg_score, na.rm = TRUE)
      segments <- segments %>%
        mutate(
          performance_ratio = avg_score / market_avg,
          competitiveness_score = performance_ratio * (top_attributes / max(top_attributes, na.rm = TRUE)),
          market_position = case_when(
            competitiveness_score >= 1.5 ~ get_lang_text("positions.leader", "市場領導者"),
            competitiveness_score >= 1.2 ~ get_lang_text("positions.strong", "強勢競爭者"),
            competitiveness_score >= 0.8 ~ get_lang_text("positions.mainstream", "主流品牌"),
            TRUE ~ get_lang_text("positions.challenger", "挑戰者")
          )
        )

      # Generate segment names for each cluster
      segments <- segments %>%
        group_by(cluster) %>%
        mutate(
          cluster_avg_score = mean(avg_score),
          cluster_top_attrs = mean(top_attributes),
          cluster_size = n(),
          brands_in_cluster = paste(Variation, collapse = ", ")
        ) %>%
        ungroup()

      # Create descriptive segment names (AI naming can be added in Phase 2)
      segments <- segments %>%
        group_by(cluster) %>%
        mutate(
          segment = paste0(
            market_position[1], " - ",
            get_lang_text("labels.avg_prefix", "平均"), round(mean(avg_score), 1), get_lang_text("labels.score_suffix", "分"),
            if(mean(top_attributes) >= median(segments$top_attributes))
              paste0(" (", get_lang_text("labels.multi_advantage", "多優勢"), ")")
            else
              paste0(" (", get_lang_text("labels.specific_advantage", "特定優勢"), ")")
          )
        ) %>%
        ungroup()

      segments
    })


    # ========== OUTPUT RENDERERS FOR TABS ==========

    # Tab 1: Segment Summary Cards
    output$segment_summary_cards <- renderUI({
      segments <- market_segments()
      req(segments)

      segment_summary <- segments %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          avg_score = round(mean(avg_score), 2),
          .groups = 'drop'
        )

      cards <- lapply(1:nrow(segment_summary), function(i) {
        segment_data <- segment_summary[i, ]
        color_class <- c("primary", "success", "warning", "info")[i %% 4 + 1]

        column(6,  # Changed from 3 to 6 for better text fit
          div(class = paste0("info-box bg-", color_class),
              style = "min-height: 120px; display: flex; align-items: center;",  # Added styling
              div(class = "info-box-content",
                  style = "width: 100%; padding: 15px;",  # Added width and padding
                  span(class = "info-box-text",
                       style = "white-space: normal; word-wrap: break-word; font-size: 14px;",  # Allow text wrapping
                       segment_data$segment),
                  span(class = "info-box-number",
                       style = "font-size: 18px; margin-top: 8px; display: block;",
                       paste0(segment_data$count, " ", get_lang_text("labels.brands", "個品牌"))),
                  div(class = "progress",
                      div(class = "progress-bar",
                          style = paste0("width: ", segment_data$avg_score * 20, "%"))),
                  span(class = "progress-description",
                       style = "font-size: 13px;",
                       paste0(get_lang_text("labels.avg_score", "平均評分"), ": ", segment_data$avg_score))
              )
          )
        )
      })

      fluidRow(cards)
    })

    # Tab 1: Segment Distribution Pie Chart
    output$segment_distribution_plot <- renderPlotly({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(plot_ly() %>%
          add_annotations(
            text = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          ))
      }

      segment_summary <- segments %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          avg_score = round(mean(avg_score), 2),
          .groups = 'drop'
        )

      plot_ly(segment_summary,
              labels = ~segment,
              values = ~count,
              type = 'pie',
              textposition = 'inside',
              textinfo = 'label+percent',
              hovertemplate = '%{label}<br>品牌數: %{value}<br>平均評分: %{customdata}<extra></extra>',
              customdata = ~avg_score,
              marker = list(colors = c('#3498db', '#2ecc71', '#f39c12', '#e74c3c', '#9b59b6', '#1abc9c'))) %>%
        layout(title = list(text = get_lang_text("charts.segment_distribution", "市場區隔分布")),
               showlegend = TRUE)
    })

    # Tab 1: Segment Characteristics Scatter Plot
    output$segment_characteristics_plot <- renderPlotly({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(plot_ly() %>%
          add_annotations(
            text = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          ))
      }

      plot_data <- segments %>%
        select(Variation, segment, avg_score, top_attributes) %>%
        arrange(segment, desc(avg_score))

      plot_ly(plot_data,
              x = ~avg_score,
              y = ~top_attributes,
              color = ~segment,
              text = ~Variation,
              type = 'scatter',
              mode = 'markers+text',
              textposition = 'top center',
              marker = list(size = 15)) %>%
        layout(title = list(text = get_lang_text("charts.segment_characteristics", "區隔特徵分布")),
               xaxis = list(title = get_lang_text("labels.avg_score", "平均評分")),
               yaxis = list(title = get_lang_text("labels.top_attributes", "優勢屬性數")),
               showlegend = TRUE)
    })

    # Tab 2: Segment Summary Table
    output$segment_summary_table <- renderDT({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(datatable(data.frame(Message = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"))))
      }

      df <- data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(numeric_cols, c("Variation"))

      # Aggregate segment data
      segment_summary <- segments %>%
        group_by(segment) %>%
        summarise(
          col_companies = n(),
          brands = paste(Variation, collapse = ", "),
          .groups = 'drop'
        )

      # Calculate key characteristics (top attributes per segment)
      key_characteristics <- segments %>%
        select(segment, all_of(attr_cols)) %>%
        group_by(segment) %>%
        summarise(across(all_of(attr_cols), mean, na.rm = TRUE), .groups = 'drop') %>%
        pivot_longer(cols = all_of(attr_cols), names_to = "attribute", values_to = "avg_score") %>%
        group_by(segment) %>%
        arrange(desc(avg_score)) %>%
        slice_head(n = 5) %>%
        summarise(
          col_key_characteristics = paste(attribute[avg_score >= 3.5], collapse = "、"),
          .groups = 'drop'
        ) %>%
        mutate(
          col_key_characteristics = ifelse(
            nchar(col_key_characteristics) == 0,
            get_lang_text("labels.developing", "尚在發展中"),
            col_key_characteristics
          )
        )

      # Merge data
      final_table <- segment_summary %>%
        left_join(key_characteristics, by = "segment") %>%
        mutate(
          col_segment = segment,
          col_companies = paste0(col_companies, " (", get_lang_text("labels.brands", "品牌"), ": ",
                           substr(brands, 1, 50),
                           ifelse(nchar(brands) > 50, "...", ""), ")")
        ) %>%
        select(col_segment, col_companies, col_key_characteristics)

      # Set language-aware column names
      colnames(final_table) <- c(
        get_lang_text("table.columns.segment", "Segment"),
        get_lang_text("table.columns.companies", "Companies"),
        get_lang_text("table.columns.key_characteristics", "Key Characteristics")
      )

      datatable(final_table,
                options = list(
                  pageLength = 10,
                  dom = 't',
                  ordering = FALSE,
                  columnDefs = list(
                    list(className = 'dt-left', targets = c(0, 2)),
                    list(className = 'dt-center', targets = c(1))
                  )
                ),
                rownames = FALSE,
                escape = FALSE)
    })

    # Tab 3: Preference Heatmap
    output$preference_heatmap <- renderPlotly({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(plot_ly() %>%
          add_annotations(
            text = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          ))
      }

      df <- data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(numeric_cols, c("Variation"))

      # Calculate average score per segment per attribute
      heatmap_data <- segments %>%
        select(segment, all_of(attr_cols)) %>%
        group_by(segment) %>%
        summarise(across(all_of(attr_cols), mean, na.rm = TRUE), .groups = 'drop') %>%
        pivot_longer(cols = -segment, names_to = "attribute", values_to = "score")

      # Convert to matrix format
      heatmap_matrix <- heatmap_data %>%
        pivot_wider(names_from = attribute, values_from = score) %>%
        column_to_rownames("segment") %>%
        as.matrix()

      plot_ly(
        z = heatmap_matrix,
        x = colnames(heatmap_matrix),
        y = rownames(heatmap_matrix),
        type = "heatmap",
        colorscale = "Viridis",
        hovertemplate = paste0(get_lang_text("labels.segment", "區隔"), ": %{y}<br>",
                              get_lang_text("labels.attribute", "屬性"), ": %{x}<br>",
                              get_lang_text("labels.score", "評分"), ": %{z:.2f}<extra></extra>")
      ) %>%
        layout(
          title = list(text = get_lang_text("charts.preference_heatmap", "客群偏好熱圖")),
          xaxis = list(title = get_lang_text("labels.product_attributes", "產品屬性"), tickangle = 45),
          yaxis = list(title = get_lang_text("labels.market_segment", "市場區隔"))
        )
    })

    # Tab 3: Preference Radar Chart
    output$preference_radar <- renderPlotly({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(plot_ly() %>%
          add_annotations(
            text = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          ))
      }

      df <- data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(numeric_cols, c("Variation"))

      # Select top 8 most important attributes
      top_attrs <- df %>%
        select(all_of(attr_cols)) %>%
        summarise(across(everything(), mean, na.rm = TRUE)) %>%
        pivot_longer(everything()) %>%
        arrange(desc(value)) %>%
        slice_head(n = min(8, length(attr_cols))) %>%
        pull(name)

      # Prepare radar chart data
      radar_data <- segments %>%
        group_by(segment) %>%
        summarise(across(all_of(top_attrs), mean, na.rm = TRUE), .groups = 'drop')

      # Create radar chart
      fig <- plot_ly(type = 'scatterpolar', fill = 'toself')

      for(i in 1:nrow(radar_data)) {
        fig <- fig %>%
          add_trace(
            r = as.numeric(radar_data[i, -1]),
            theta = top_attrs,
            name = radar_data$segment[i]
          )
      }

      fig %>%
        layout(
          title = list(text = get_lang_text("charts.preference_radar", "區隔偏好雷達圖")),
          polar = list(
            radialaxis = list(
              visible = TRUE,
              range = c(0, 5)
            )
          ),
          showlegend = TRUE
        )
    })

    # Tab 3: AI Customer Insights (Preferences Tab)
    output$ai_customer_insights <- renderUI({
      req(ai_results$customer_analysis)

      tagList(
        h4(get_lang_text("labels.ai_insights", "AI客群洞察"),
           style = "margin-top: 30px;"),
        wellPanel(
          style = "background-color: #f8f9fa; border-left: 4px solid #28a745; padding: 20px;",
          HTML(markdown::renderMarkdown(text = ai_results$customer_analysis))
        )
      )
    })

    # Tab 4: Opportunity Matrix
    output$opportunity_matrix <- renderPlotly({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(plot_ly() %>%
          add_annotations(
            text = get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分"),
            showarrow = FALSE,
            xref = "paper",
            yref = "paper",
            x = 0.5,
            y = 0.5
          ))
      }

      # Calculate opportunity scores
      # Use brand count as proxy for market size (segments with more brands = larger market)
      segment_counts <- segments %>% count(segment, name = "seg_count")
      opportunity_data <- segments %>%
        left_join(segment_counts, by = "segment") %>%
        mutate(
          market_size = seg_count / max(seg_count) * 100,  # Normalize to percentage
          growth_potential = 5 - avg_score,  # Lower score = higher growth potential
          competition = 1 / (top_attributes + 1),  # Fewer advantages = more competition
          opportunity_score = (market_size * 0.3 + growth_potential * 20 * 0.5 + (1-competition) * 100 * 0.2)
        )

      plot_ly(opportunity_data,
              x = ~growth_potential,
              y = ~market_size,
              size = ~opportunity_score,
              color = ~segment,
              text = ~Variation,
              type = 'scatter',
              mode = 'markers+text',
              textposition = 'top center',
              textfont = list(size = 9),
              marker = list(sizemode = 'diameter', sizeref = 0.5)) %>%
        layout(
          title = list(text = get_lang_text("charts.opportunity_matrix", "市場機會矩陣")),
          xaxis = list(title = get_lang_text("labels.growth_potential", "成長潛力")),
          yaxis = list(title = get_lang_text("labels.market_size", "市場規模")),
          showlegend = TRUE,
          annotations = list(
            list(x = 0.8, y = 0.9, xref = "paper", yref = "paper",
                 text = get_lang_text("quadrants.high_potential_large", "高潛力大市場"),
                 showarrow = FALSE,
                 font = list(color = "green", size = 18, family = "Arial Black")),
            list(x = 0.2, y = 0.9, xref = "paper", yref = "paper",
                 text = get_lang_text("quadrants.mature_large", "成熟大市場"),
                 showarrow = FALSE,
                 font = list(color = "blue", size = 18, family = "Arial Black")),
            list(x = 0.8, y = 0.1, xref = "paper", yref = "paper",
                 text = get_lang_text("quadrants.emerging_small", "新興小市場"),
                 showarrow = FALSE,
                 font = list(color = "orange", size = 18, family = "Arial Black")),
            list(x = 0.2, y = 0.1, xref = "paper", yref = "paper",
                 text = get_lang_text("quadrants.saturated_small", "飽和小市場"),
                 showarrow = FALSE,
                 font = list(color = "gray", size = 18, family = "Arial Black"))
          )
        )
    })

    # Tab 4: AI Opportunities Insights
    output$ai_opportunities_insights <- renderUI({
      req(ai_results$opportunities_analysis)

      tagList(
        h4(get_lang_text("labels.ai_opportunities", "AI市場機會分析"),
           style = "margin-top: 30px;"),
        wellPanel(
          style = "background-color: #fff3cd; border-left: 4px solid #ffc107; padding: 20px;",
          HTML(markdown::renderMarkdown(text = ai_results$opportunities_analysis))
        )
      )
    })

    # Tab 5: Analysis Summary Report
    output$analysis_summary_report <- renderUI({
      segments <- market_segments()

      if (is.null(segments) || nrow(segments) == 0) {
        return(div(
          class = "alert alert-info",
          get_lang_text("messages.no_data", "請先上傳資料並執行屬性評分")
        ))
      }

      df <- data()
      numeric_cols <- names(df)[sapply(df, is.numeric)]
      attr_cols <- setdiff(numeric_cols, c("Variation"))

      # Calculate segment statistics
      segment_stats <- segments %>%
        group_by(segment) %>%
        summarise(
          count = n(),
          avg_score = round(mean(avg_score, na.rm = TRUE), 2),
          brands = paste(Variation, collapse = ", "),
          .groups = 'drop'
        )

      # Calculate key attributes per segment
      segment_attrs <- segments %>%
        select(segment, all_of(attr_cols)) %>%
        group_by(segment) %>%
        summarise(across(all_of(attr_cols), mean, na.rm = TRUE), .groups = 'drop') %>%
        pivot_longer(cols = all_of(attr_cols), names_to = "attribute", values_to = "score") %>%
        group_by(segment) %>%
        arrange(desc(score)) %>%
        slice_head(n = 5) %>%
        filter(score >= 3.5) %>%
        summarise(
          key_attrs = paste(attribute, collapse = "、"),
          .groups = 'drop'
        )

      # Merge data
      report_data <- segment_stats %>%
        left_join(segment_attrs, by = "segment")

      # Generate dynamic HTML report
      segment_list <- lapply(1:nrow(report_data), function(i) {
        row <- report_data[i,]
        paste0(
          '<li><strong>', row$segment, '：</strong>',
          get_lang_text("report.has", "擁有"), row$count, get_lang_text("report.companies", '家廠商，'),
          get_lang_text("report.avg_score", '平均評分為'), row$avg_score, '。',
          if(!is.na(row$key_attrs) && nchar(row$key_attrs) > 0) {
            paste0(get_lang_text("report.features", '其特色為'), row$key_attrs, '。')
          } else {
            get_lang_text("report.developing", '尚在發展中。')
          },
          '</li>'
        )
      })

      report_html <- HTML(paste0(
        '<div class="ai-report-section" style="background: #f8f9fa; padding: 20px; border-radius: 8px;">',
        '<h3>', icon("chart-pie"), ' ', get_lang_text("report.segmentation_title", "市場區隔輪廓分析"), '</h3>',
        '<ul>',
        paste(segment_list, collapse = ""),
        '</ul>',

        '<h3>', icon("users"), ' ', get_lang_text("report.target_customer_title", "目標客群偏好分析"), '</h3>',
        '<p>',
        if(nrow(report_data) > 0) {
          best_segment <- report_data[which.max(report_data$avg_score),]
          paste0(
            best_segment$segment, get_lang_text("report.highest_score", '具有最高平均評分（'), best_segment$avg_score, get_lang_text("report.score_close", '），'),
            get_lang_text("report.should_enhance", '應強化'), if(!is.na(best_segment$key_attrs)) best_segment$key_attrs else get_lang_text("report.core_advantages", '核心優勢'),
            get_lang_text("report.attract_target", '來吸引目標客群。')
          )
        } else {
          get_lang_text("report.run_ai_analysis", '請執行AI分析以生成客群偏好分析。')
        },
        '</p>',

        '<h3>', icon("lightbulb"), ' ', get_lang_text("report.market_opportunity_title", "潛在市場機會分析"), '</h3>',
        '<p>',
        if(nrow(report_data) > 1) {
          # Find segment with lowest avg_score (more potential for improvement)
          low_score <- report_data[which.min(report_data$avg_score),]
          paste0(
            get_lang_text("report.considering", '考慮到'), low_score$segment,
            get_lang_text("report.lower_avg", '平均評分較低（'), low_score$avg_score, '），',
            get_lang_text("report.indicates_opportunity", '這表示潛在市場機會。'),
            get_lang_text("report.suggest_enhance", '建議強化該區隔的市場策略。')
          )
        } else {
          get_lang_text("report.run_ai_for_opportunities", '請執行AI分析以生成市場機會分析。')
        },
        '</p>',
        '</div>'
      ))

      report_html
    })

    # ========== AI Analysis Button Handler ==========
    observeEvent(input$run_ai_analysis, {
      req(market_segments())

      segments <- market_segments()
      current_lang <- get_current_language()

      # Show progress notification
      showNotification(
        get_lang_text("labels.ai_loading", "🤖 AI分析啟動中..."),
        type = "message",
        duration = 3
      )

      # Set loading state
      ai_results$is_loading <- TRUE

      # Run AI analyses with progress
      withProgress(message = get_lang_text("labels.ai_loading", 'AI分析中...'), value = 0, {
        # 1. Segment Naming (25%)
        setProgress(value = 0.25, detail = get_lang_text("labels.ai_progress_segment_naming", "生成區隔名稱..."))
        ai_results$segment_names <- generate_segment_names_ai(segments, current_lang)

        # 2. Customer Preference Analysis (50%)
        setProgress(value = 0.50, detail = get_lang_text("labels.ai_progress_customer_analysis", "分析客群偏好..."))
        ai_results$customer_analysis <- generate_customer_analysis_ai(segments, current_lang)

        # 3. Market Opportunities (75%)
        setProgress(value = 0.75, detail = get_lang_text("labels.ai_progress_market_opportunities", "識別市場機會..."))
        ai_results$opportunities_analysis <- generate_opportunities_analysis_ai(segments, current_lang)

        # 4. Complete (100%)
        setProgress(value = 1.0, detail = get_lang_text("labels.ai_progress_complete", "完成..."))

        ai_results$is_loading <- FALSE
        ai_results$last_updated <- Sys.time()
      })

      # Success notification
      showNotification(
        get_lang_text("labels.ai_complete", "✅ AI分析完成！"),
        type = "message",
        duration = 5
      )
    }, ignoreInit = TRUE)

    # Return reactive values
    return(list(
      segments = market_segments
    ))
  })
}

message("✅ BrandEdge Market Profile Module Phase 1 載入完成")

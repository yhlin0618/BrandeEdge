# DNA 分析行銷建議工具函數
# 從 module_dna_multi.R 抽取的行銷建議相關函數

library(dplyr)

# ============================================================================
# 預設行銷建議函數
# ============================================================================

#' 根據指標和分群獲取預設行銷建議
#' @param metric 指標類型 (R, F, M, CAI, PCV, CRI, NES)
#' @param segment 分群標籤
#' @return 行銷建議列表
get_default_marketing_recommendations <- function(metric, segment) {
  recommendations <- list()
  
  if (metric == "R") {
    # 最近購買時間建議
    recommendations <- switch(
      as.character(segment),
      "高活躍" = list(
        strategy = "深化經營",
        actions = c(
          "交叉銷售相關產品",
          "提供VIP專屬優惠",
          "邀請參加新品體驗"
        )
      ),
      "中活躍" = list(
        strategy = "維持互動",
        actions = c(
          "定期推送內容",
          "季節性促銷",
          "會員積分獎勵"
        )
      ),
      "低活躍" = list(
        strategy = "召回激活",
        actions = c(
          "發送個人化召回郵件",
          "提供限時優惠券",
          "推送懷舊產品"
        )
      ),
      list(
        strategy = "標準維護",
        actions = c("定期關懷", "產品更新通知", "會員福利")
      )
    )
    
  } else if (metric == "F") {
    # 購買頻率建議
    recommendations <- switch(
      as.character(segment),
      "高頻" = list(
        strategy = "忠誠度深化",
        actions = c(
          "VIP會員計劃",
          "批量採購優惠",
          "專屬客服"
        )
      ),
      "中頻" = list(
        strategy = "穩定維護",
        actions = c(
          "累積消費獎勵",
          "生日優惠",
          "定期關懷"
        )
      ),
      "低頻" = list(
        strategy = "頻率提升",
        actions = c(
          "首購後跟進",
          "試用品贈送",
          "購買提醒"
        )
      ),
      list(
        strategy = "基礎維護",
        actions = c("定期促銷", "產品推薦", "滿意度調查")
      )
    )
    
  } else if (metric == "M") {
    # 購買金額建議
    recommendations <- switch(
      as.character(segment),
      "高價值" = list(
        strategy = "價值最大化",
        actions = c(
          "高端產品推薦",
          "客製化服務",
          "尊榮體驗"
        )
      ),
      "低價值" = list(
        strategy = "價值提升",
        actions = c(
          "入門產品推薦",
          "組合優惠",
          "分期付款"
        )
      ),
      list(
        strategy = "向上銷售",
        actions = c("升級產品推薦", "捆綁銷售", "會員升級")
      )
    )
    
  } else if (metric == "CAI") {
    # 活躍度建議
    recommendations <- switch(
      as.character(segment),
      "漸趨活躍" = list(
        strategy = "把握成長動能",
        actions = c(
          "推薦新品",
          "加強互動頻率",
          "提供升級方案"
        )
      ),
      "穩定" = list(
        strategy = "穩定維護",
        actions = c(
          "定期關懷",
          "季節性活動",
          "維持現有服務"
        )
      ),
      "漸趨靜止" = list(
        strategy = "緊急挽回",
        actions = c(
          "個人化關懷",
          "特殊優惠",
          "了解流失原因"
        )
      ),
      list(
        strategy = "標準服務",
        actions = c("基礎維護", "產品更新", "客戶調查")
      )
    )
    
  } else if (metric == "PCV") {
    # 過去價值建議
    recommendations <- switch(
      as.character(segment),
      "高價值" = list(
        strategy = "價值最大化",
        actions = c(
          "VIP專屬服務",
          "提供高端產品",
          "客製化方案"
        )
      ),
      "低價值" = list(
        strategy = "價值培育",
        actions = c(
          "入門優惠",
          "教育內容",
          "小額試用"
        )
      ),
      list(
        strategy = "價值提升",
        actions = c("升級引導", "組合優惠", "忠誠度計劃")
      )
    )
    
  } else if (metric == "CRI") {
    # 參與度建議
    recommendations <- switch(
      as.character(segment),
      "高參與" = list(
        strategy = "核心客戶經營",
        actions = c(
          "優先服務",
          "專屬活動",
          "共創價值"
        )
      ),
      "中參與" = list(
        strategy = "潛力開發",
        actions = c(
          "目標設定",
          "階段獎勵",
          "社群互動"
        )
      ),
      "低參與" = list(
        strategy = "參與度提升",
        actions = c(
          "互動激勵",
          "簡化流程",
          "新手引導"
        )
      ),
      list(
        strategy = "基礎互動",
        actions = c("定期溝通", "意見收集", "服務改善")
      )
    )
    
  } else if (metric == "NES") {
    # 客戶狀態建議
    recommendations <- switch(
      as.character(segment),
      "新客戶" = list(
        strategy = "新客培育",
        actions = c(
          "歡迎禮包",
          "新手引導",
          "首購優惠"
        )
      ),
      "主力客戶" = list(
        strategy = "核心維護",
        actions = c(
          "VIP服務",
          "忠誠獎勵",
          "專屬活動"
        )
      ),
      "風險客戶" = list(
        strategy = "風險管理",
        actions = c(
          "預警關懷",
          "滿意度調查",
          "挽留方案"
        )
      ),
      "流失客戶" = list(
        strategy = "召回策略",
        actions = c(
          "喚醒活動",
          "特殊優惠",
          "重新定位"
        )
      ),
      list(
        strategy = "標準服務",
        actions = c("基礎維護", "定期溝通", "產品更新")
      )
    )
    
  } else {
    # 預設建議
    recommendations <- list(
      strategy = "標準維護",
      actions = c(
        "定期關懷維護",
        "提供個人化服務",
        "建立長期關係"
      )
    )
  }
  
  return(recommendations)
}

# ============================================================================
# 分群行銷建議生成函數
# ============================================================================

#' 為所有分群生成行銷建議
#' @param data 包含分群資訊的資料框架
#' @param metric 當前選擇的指標
#' @param use_ai 是否使用 AI 生成（需要 API）
#' @return 行銷建議列表
generate_segment_recommendations <- function(data, metric, use_ai = FALSE) {
  if (is.null(data) || nrow(data) == 0) {
    return(list())
  }
  
  # 取得唯一的分群
  segments <- unique(data$segment)
  segments <- segments[!is.na(segments)]
  
  recommendations <- list()
  
  for (seg in segments) {
    # 計算分群統計
    seg_data <- data[data$segment == seg & !is.na(data$segment), ]
    
    if (nrow(seg_data) > 0) {
      seg_stats <- list(
        count = nrow(seg_data),
        pct = round(nrow(seg_data) / nrow(data) * 100, 1)
      )
      
      # 計算可用的統計指標
      if ("r_value" %in% names(seg_data)) {
        seg_stats$avg_r <- round(mean(seg_data$r_value, na.rm = TRUE), 1)
      }
      if ("f_value" %in% names(seg_data)) {
        seg_stats$avg_f <- round(mean(seg_data$f_value, na.rm = TRUE), 1)
      }
      if ("m_value" %in% names(seg_data)) {
        seg_stats$avg_m <- round(mean(seg_data$m_value, na.rm = TRUE), 2)
      }
      
      # 獲取建議
      if (use_ai && exists("execute_gpt_request")) {
        # AI 生成建議（需要實作）
        recommendations[[seg]] <- get_default_marketing_recommendations(metric, seg)
      } else {
        # 使用預設建議
        recommendations[[seg]] <- get_default_marketing_recommendations(metric, seg)
      }
      
      # 添加統計資訊
      recommendations[[seg]]$stats <- seg_stats
    }
  }
  
  return(recommendations)
}

# ============================================================================
# 行銷建議 HTML 生成函數
# ============================================================================

#' 生成行銷建議的 HTML 內容
#' @param recommendations 行銷建議列表
#' @param metric 當前指標
#' @return HTML tagList
generate_recommendations_html <- function(recommendations, metric = NULL) {
  if (length(recommendations) == 0) {
    return(p("無分群數據或建議"))
  }
  
  # 指標標題
  metric_titles <- list(
    "R" = "最近購買時間行銷策略",
    "F" = "購買頻率行銷策略",
    "M" = "購買金額行銷策略",
    "CAI" = "顧客活躍度行銷策略",
    "PCV" = "過去價值行銷策略",
    "CRI" = "參與度行銷策略",
    "NES" = "顧客狀態行銷策略"
  )
  
  title <- metric_titles[[metric]]
  if (is.null(title)) title <- "行銷策略建議"
  
  # 生成 HTML
  content <- tagList(
    h5(paste("🎯", title)),
    lapply(names(recommendations), function(seg) {
      rec <- recommendations[[seg]]
      
      # 分群卡片
      div(
        class = "mb-3 p-3",
        style = "background: #f8f9fa; border-radius: 8px; border-left: 4px solid #007bff;",
        
        # 標題
        h6(
          paste0("🔸 ", seg),
          if (!is.null(rec$stats)) {
            tags$small(
              class = "text-muted ml-2",
              paste0("(", rec$stats$count, "人, ", rec$stats$pct, "%)")
            )
          },
          style = "color: #2c3e50; font-weight: bold; margin-bottom: 10px;"
        ),
        
        # 策略
        if (!is.null(rec$strategy)) {
          p(
            tags$b("策略："),
            rec$strategy,
            style = "margin-bottom: 8px;"
          )
        },
        
        # 行動建議
        if (!is.null(rec$actions) && length(rec$actions) > 0) {
          tags$ul(
            style = "margin: 0; padding-left: 20px;",
            lapply(rec$actions, function(action) {
              tags$li(action, style = "margin: 3px 0; color: #495057;")
            })
          )
        }
      )
    })
  )
  
  return(content)
}

# ============================================================================
# 分群摘要生成函數
# ============================================================================

#' 生成分群摘要表格
#' @param data 包含分群的資料框架
#' @param metric 當前指標
#' @return HTML table
generate_segment_summary_table <- function(data, metric) {
  if (is.null(data) || nrow(data) == 0 || !"segment" %in% names(data)) {
    return(p("無分群數據"))
  }
  
  # 計算統計
  available_cols <- names(data)
  segment_summary <- data %>%
    filter(!is.na(segment)) %>%
    group_by(segment) %>%
    summarise(
      count = n(),
      pct = round(n() / nrow(data) * 100, 1),
      .groups = "drop"
    )
  
  # 添加可用的統計欄位
  if ("r_value" %in% available_cols) {
    segment_summary <- segment_summary %>%
      left_join(
        data %>%
          group_by(segment) %>%
          summarise(avg_r = round(mean(r_value, na.rm = TRUE), 1), .groups = "drop"),
        by = "segment"
      )
  }
  
  if ("f_value" %in% available_cols) {
    segment_summary <- segment_summary %>%
      left_join(
        data %>%
          group_by(segment) %>%
          summarise(avg_f = round(mean(f_value, na.rm = TRUE), 1), .groups = "drop"),
        by = "segment"
      )
  }
  
  if ("m_value" %in% available_cols) {
    segment_summary <- segment_summary %>%
      left_join(
        data %>%
          group_by(segment) %>%
          summarise(avg_m = round(mean(m_value, na.rm = TRUE), 2), .groups = "drop"),
        by = "segment"
      )
  }
  
  # 排序
  segment_summary <- segment_summary %>%
    arrange(desc(count))
  
  # 指標標籤
  metric_labels <- list(
    "R" = "最近購買",
    "F" = "購買頻率",
    "M" = "購買金額",
    "CAI" = "活躍度",
    "PCV" = "過去價值",
    "CRI" = "參與度",
    "NES" = "顧客狀態"
  )
  
  metric_label <- metric_labels[[metric]]
  if (is.null(metric_label)) metric_label <- "指標"
  
  # 生成表格
  tags$table(
    class = "table table-sm table-hover",
    tags$thead(
      tags$tr(
        tags$th(paste0(metric_label, "分群")),
        tags$th("人數"),
        tags$th("佔比"),
        if ("avg_r" %in% names(segment_summary)) tags$th("R(天)") else NULL,
        if ("avg_f" %in% names(segment_summary)) tags$th("F(次)") else NULL,
        if ("avg_m" %in% names(segment_summary)) tags$th("M($)") else NULL
      )
    ),
    tags$tbody(
      lapply(1:nrow(segment_summary), function(i) {
        seg <- segment_summary[i, ]
        tags$tr(
          tags$td(seg$segment),
          tags$td(seg$count),
          tags$td(paste0(seg$pct, "%")),
          if ("avg_r" %in% names(segment_summary)) {
            tags$td(if (!is.na(seg$avg_r)) seg$avg_r else "-")
          } else NULL,
          if ("avg_f" %in% names(segment_summary)) {
            tags$td(if (!is.na(seg$avg_f)) seg$avg_f else "-")
          } else NULL,
          if ("avg_m" %in% names(segment_summary)) {
            tags$td(if (!is.na(seg$avg_m)) paste0("$", seg$avg_m) else "-")
          } else NULL
        )
      })
    )
  )
}
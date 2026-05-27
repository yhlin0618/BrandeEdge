# BrandEdge Key Factor Module (Simplified Version)
# 關鍵因素分析模組 - 簡化版
# Version: 2.0
# Last Updated: 2025-10-10
# Based on: BrandEdge_premium/modules/module_wo_b.R lines 108-169
#
# Features:
#   • Display key factors list
#   • Show factor importance table
#   • Support multi-language interface
#
# Note: This is the SIMPLE version matching the original BrandEdge implementation.
#       NO KFE analysis, NO sensitivity slider, NO complex features.

library(shiny)
library(DT)
library(dplyr)
library(tidyr)

# NULL coalescing operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

#' Key Factor Module - UI
#'
#' @description
#' Simple key factor display and importance analysis
#'
#' @param id Module namespace ID
#' @param module_config Module configuration (optional)
#' @param lang_texts Language texts reactive or list (optional)
#'
#' @return tagList containing UI elements
keyFactorModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  # Get language content
  module_lang_content <- tryCatch(
    if (is.function(lang_texts)) lang_texts() else lang_texts,
    error = function(e) NULL
  )

  # Helper function to get translated text
  get_text <- function(path, default) {
    if (is.null(module_lang_content)) return(default)

    # Navigate nested structure using dot notation
    keys <- strsplit(path, "\\.")[[1]]
    value <- module_lang_content

    for (key in keys) {
      if (is.list(value) && key %in% names(value)) {
        value <- value[[key]]
      } else {
        return(default)
      }
    }

    if (is.character(value)) return(value) else return(default)
  }

  # UI Layout
  tagList(
    # Key factors display box
    box(
      title = get_text("ui.key_factors.title", "關鍵成功因素識別"),
      status = "info",
      width = 12,
      solidHeader = TRUE,
      textOutput(ns("key_factors_text"))
    ),

    # Importance table box
    box(
      title = get_text("ui.importance.title", "關鍵因素重要性分析"),
      status = "success",
      width = 12,
      solidHeader = TRUE,
      DTOutput(ns("factor_importance"))
    )
  )
}

#' Key Factor Module - Server
#'
#' @description
#' Simple key factor analysis logic
#' Core features:
#' 1. Display list of key factors
#' 2. Calculate and display importance metrics
#'
#' @param id Module namespace ID
#' @param data Reactive expression returning scored data
#' @param key_vars Reactive expression returning vector of key attribute names
#' @param lang_texts Reactive expression returning language texts (optional)
#' @param module_config Module configuration (optional)
#'
#' @return Module server function
keyFactorModuleServer <- function(id, data, key_vars, lang_texts = reactive(NULL), module_config = NULL) {
  moduleServer(id, function(input, output, session) {

    # ========== Language Helper ==========
    get_lang_text <- function(path, default = "") {
      texts <- tryCatch({
        if (!is.null(lang_texts) && is.reactive(lang_texts) && is.function(lang_texts)) {
          lang_texts()
        } else {
          lang_texts
        }
      }, error = function(e) NULL)

      if (is.null(texts)) return(default)

      # Parse path (e.g., "messages.no_data")
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

    # ========== Display Key Factors ==========
    output$key_factors_text <- renderText({
      keys <- tryCatch(key_vars(), error = function(e) character(0))

      if (length(keys) == 0) {
        return(get_lang_text("messages.no_key_factors", "尚未識別關鍵因素"))
      }

      label <- get_lang_text("labels.key_factors", "關鍵因素：")
      paste(label, paste(keys, collapse = "、"))
    })

    # ========== Calculate Factor Importance ==========
    output$factor_importance <- renderDT({
      df <- data()
      attrs <- tryCatch(key_vars(), error = function(e) character(0))

      # Validate data
      req(length(attrs) > 0)
      req(nrow(df) > 0)

      # Get column names from language
      col_attr <- get_lang_text("tables.columns.attribute", "屬性")
      col_mean <- get_lang_text("tables.columns.mean", "平均分")
      col_sd <- get_lang_text("tables.columns.std_dev", "標準差")
      col_max <- get_lang_text("tables.columns.max", "最大值")
      col_min <- get_lang_text("tables.columns.min", "最小值")
      col_importance <- get_lang_text("tables.columns.importance_index", "重要性指數")
      col_rank <- get_lang_text("tables.columns.rank", "排名")

      # Calculate importance metrics for each key factor
      importance_data <- df %>%
        select(all_of(attrs)) %>%
        summarise_all(list(
          mean = ~mean(., na.rm = TRUE),
          sd = ~sd(., na.rm = TRUE),
          max = ~max(., na.rm = TRUE),
          min = ~min(., na.rm = TRUE)
        )) %>%
        pivot_longer(everything(),
                     names_to = "metric_attr",
                     values_to = "value") %>%
        separate(metric_attr, c("attribute", "metric"), sep = "_", fill = "right", extra = "merge") %>%
        pivot_wider(names_from = metric, values_from = value) %>%
        mutate(
          mean = as.numeric(mean),
          sd = as.numeric(sd),
          sd = ifelse(is.na(sd) | !is.finite(sd), 0, sd),
          importance_index = round(mean * (1 + sd), 2),
          rank = rank(-importance_index)
        ) %>%
        arrange(rank)

      # Rename columns to match language
      importance_data <- importance_data %>%
        rename(
          !!col_attr := attribute,
          !!col_mean := mean,
          !!col_sd := sd,
          !!col_max := max,
          !!col_min := min,
          !!col_importance := importance_index,
          !!col_rank := rank
        )

      # Display table
      DT::datatable(importance_data,
                    rownames = FALSE,
                    options = list(
                      pageLength = 10,
                      searching = FALSE,
                      dom = 't'
                    )) %>%
        formatRound(columns = c(col_mean, col_sd, col_importance), digits = 2)
    })
  })
}

message("✅ BrandEdge Key Factor Module loaded (Simplified Version)")

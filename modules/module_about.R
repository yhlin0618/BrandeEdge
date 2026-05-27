# ==========================================
# About Us Module - Simple Version
# ==========================================
# Simplified version that just loads markdown
# Compatible with InsightForge renderUI pattern
# Version: 2.1
# Last Updated: 2025-10-15

library(shiny)
library(bs4Dash)
library(markdown)

# ==========================================
# Module UI
# ==========================================
aboutModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)

  cat("[About Module UI] Called with id:", id, "\n")

  tagList(
    uiOutput(ns("about_content"))
  )
}

# ==========================================
# Module Server
# ==========================================
aboutModuleServer <- function(id, module_config = NULL, lang_texts = NULL) {
  moduleServer(id, function(input, output, session) {

    cat("[About Module Server] Initialized for id:", id, "\n")

    # Determine language directory - READS DIRECTLY FROM GLOBAL STATE
    get_language_dir <- function() {
      # ⚡ FIXED: Read directly from global_language_state instead of lang_texts
      # lang_texts is static and doesn't update when language changes!
      if (exists("global_language_state", envir = .GlobalEnv)) {
        lang_state <- get("global_language_state", envir = .GlobalEnv)
        if (!is.null(lang_state$language_content) && !is.null(lang_state$language_content$language)) {
          lang_code <- lang_state$language_content$language

          cat("[About Module] Reading language from global_language_state:", lang_code, "\n")

          # Map language code to directory
          lang_map <- c(
            "zh_TW" = "chinese",
            "en_US" = "english",
            "ja_JP" = "japanese"
          )

          return(lang_map[lang_code] %||% "chinese")
        }
      }

      # Fallback: try lang_texts parameter (for backward compatibility)
      if (is.reactive(lang_texts)) {
        texts <- tryCatch(lang_texts(), error = function(e) NULL)
      } else {
        texts <- lang_texts
      }

      if (!is.null(texts) && !is.null(texts$language)) {
        lang_code <- texts$language
        lang_map <- c(
          "zh_TW" = "chinese",
          "en_US" = "english",
          "ja_JP" = "japanese"
        )
        return(lang_map[lang_code] %||% "chinese")
      }

      # Default to chinese
      cat("[About Module] No language found, defaulting to chinese\n")
      return("chinese")
    }

    # Render about content - REACTIVE to language changes
    output$about_content <- renderUI({
      # ⚡ Make this reactive to global_language_state changes
      # DO NOT use isolate() - we WANT this to be reactive!
      if (exists("global_language_state", envir = .GlobalEnv)) {
        # Access update_trigger to create reactive dependency
        trigger_value <- get("global_language_state", envir = .GlobalEnv)$update_trigger
        cat("[About Module] Reactive trigger value:", trigger_value, "\n")
      }

      lang_dir <- get_language_dir()

      # Construct markdown file path
      md_path <- file.path("database/content", lang_dir, "markdown/about.md")

      cat("[About Module Server] Loading markdown from:", md_path, "\n")
      cat("[About Module Server] Language directory:", lang_dir, "\n")

      if (file.exists(md_path)) {
        tryCatch({
          # Read markdown file
          md_content <- paste(readLines(md_path, warn = FALSE), collapse = "\n")
          cat("[About Module Server] Loaded", nchar(md_content), "characters\n")

          # Render markdown to HTML
          html_content <- markdownToHTML(
            text = md_content,
            fragment.only = TRUE,
            options = c("use_xhtml", "smartypants", "base64_images", "mathjax")
          )

          cat("[About Module Server] Rendered HTML:", nchar(html_content), "characters\n")

          # Wrap in a card without title (page already has title header)
          bs4Card(
            title = NULL,
            status = "primary",
            solidHeader = FALSE,
            collapsible = FALSE,
            width = 12,
            HTML(html_content)
          )
        }, error = function(e) {
          cat("[About Module Server] Error:", e$message, "\n")
          # Error rendering markdown
          bs4Card(
            title = NULL,
            status = "danger",
            solidHeader = FALSE,
            width = 12,
            div(
              class = "alert alert-danger",
              h4("Markdown Rendering Error"),
              p(paste0("Error: ", e$message)),
              p(paste0("File: ", md_path))
            )
          )
        })
      } else {
        cat("[About Module Server] File not found:", md_path, "\n")
        # File not found
        bs4Card(
          title = NULL,
          status = "warning",
          solidHeader = FALSE,
          width = 12,
          div(
            class = "alert alert-warning",
            h4("Content Not Found"),
            p(paste0("Could not find markdown file: ", md_path)),
            p("Please ensure the about.md file exists in the correct language directory."),
            tags$ul(
              tags$li("Chinese: database/content/chinese/markdown/about.md"),
              tags$li("English: database/content/english/markdown/about.md"),
              tags$li("Japanese: database/content/japanese/markdown/about.md")
            )
          )
        )
      }
    })
  })
}

# NULL coalesce operator
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (is.character(x) && !nzchar(x))) y else x
}

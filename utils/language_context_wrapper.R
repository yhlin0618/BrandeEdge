
# Language Context Wrapper for Modules
# This ensures global_lang_content is accessible within module contexts

#' Create a module environment with global_lang_content
#'
#' @param module_server The module server function
#' @param ... Arguments to pass to the module server
#' @return The result of the module server call
with_language_context <- function(module_server, ...) {
  # Get global_lang_content from the app environment
  app_env <- parent.frame()

  # Check if global_lang_content exists in app environment
  if (exists("global_lang_content", envir = app_env)) {
    # Get the reactive value
    glc <- get("global_lang_content", envir = app_env)

    # Create a new environment with global_lang_content
    module_env <- new.env(parent = app_env)
    assign("global_lang_content", glc, envir = module_env)

    # Call the module server in this environment
    environment(module_server) <- module_env
  }

  # Call the module server
  module_server(...)
}

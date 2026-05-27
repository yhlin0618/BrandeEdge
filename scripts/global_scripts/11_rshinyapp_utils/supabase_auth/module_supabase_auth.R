# ==============================================================================
# Supabase Authentication Module
# File: modules/shared/module_supabase_auth.R
# Purpose: R functions to call Supabase REST API for authentication
# Date: 2025-01-19
# ==============================================================================

library(httr)
library(jsonlite)

# ==============================================================================
# Configuration
# ==============================================================================

#' Get Supabase configuration from environment variables
#' @return list with url and anon_key
get_supabase_config <- function() {
  list(
    url = Sys.getenv("SUPABASE_URL", "https://oziernubrqgqthjksbii.supabase.co"),
    anon_key = Sys.getenv("SUPABASE_ANON_KEY", "")
  )
}

# ==============================================================================
# Core Authentication Functions
# ==============================================================================

#' Authenticate user with username and password via Supabase RPC
#'
#' @param username Character. The username to authenticate.
#' @param password Character. The password to verify.
#' @return list with success (boolean), user_id, role, error (if failed)
#'
#' @examples
#' result <- authenticate_user("admin", "12345")
#' if (result$success) {
#'   cat("Logged in as:", result$role)
#' }
authenticate_user <- function(username, password) {
  message(sprintf("[authenticate_user] Authenticating user '%s'", username))
  config <- get_supabase_config()
  message(sprintf("[authenticate_user] Supabase URL: %s, Key configured: %s",
                  config$url, nchar(config$anon_key) > 0))

  if (config$anon_key == "") {
    message("[authenticate_user] ERROR: SUPABASE_ANON_KEY not configured")
    return(list(
      success = FALSE,
      error = "SUPABASE_ANON_KEY not configured"
    ))
  }

  tryCatch({
    # Call Supabase RPC function
    res <- POST(
      url = paste0(config$url, "/rest/v1/rpc/verify_password"),
      add_headers(
        "Content-Type" = "application/json",
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      ),
      body = list(
        p_username = username,
        p_password = password
      ),
      encode = "json"
    )

    # Check HTTP status
    if (status_code(res) != 200) {
      return(list(
        success = FALSE,
        error = paste("API error:", status_code(res))
      ))
    }

    # Parse response
    result <- content(res, as = "parsed", type = "application/json")

    # Return result (already structured as expected)
    return(result)

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Connection error:", e$message)
    ))
  })
}

#' Check login limit and increment counter
#'
#' @param user_id Character. The user's UUID from authentication.
#' @param app_name Character. App name: "brandedge", "insightforge", "tagpilot", "vitalsigns"
#' @return list with allowed (boolean), remaining (int), is_admin (boolean), error (if failed)
#'
#' @examples
#' limit_check <- check_login_limit("uuid-here", "brandedge")
#' if (limit_check$allowed) {
#'   cat("Remaining logins:", limit_check$remaining)
#' }
check_login_limit <- function(user_id, app_name) {
  config <- get_supabase_config()

  if (config$anon_key == "") {
    return(list(
      allowed = FALSE,
      error = "SUPABASE_ANON_KEY not configured"
    ))
  }

  # Validate app_name
  valid_apps <- c("brandedge", "insightforge", "tagpilot", "vitalsigns")
  if (!tolower(app_name) %in% valid_apps) {
    return(list(
      allowed = FALSE,
      error = paste("Invalid app_name. Must be one of:", paste(valid_apps, collapse = ", "))
    ))
  }

  tryCatch({
    res <- POST(
      url = paste0(config$url, "/rest/v1/rpc/check_and_increment_login"),
      add_headers(
        "Content-Type" = "application/json",
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      ),
      body = list(
        p_user_id = user_id,
        p_app_name = tolower(app_name)
      ),
      encode = "json"
    )

    if (status_code(res) != 200) {
      return(list(
        allowed = FALSE,
        error = paste("API error:", status_code(res))
      ))
    }

    result <- content(res, as = "parsed", type = "application/json")
    return(result)

  }, error = function(e) {
    return(list(
      allowed = FALSE,
      error = paste("Connection error:", e$message)
    ))
  })
}

#' Get user profile by ID
#'
#' @param user_id Character. The user's UUID.
#' @return list with user profile data or error
#'
#' @examples
#' profile <- get_user_profile("uuid-here")
#' if (profile$success) {
#'   cat("Display name:", profile$display_name)
#' }
get_user_profile <- function(user_id) {
  config <- get_supabase_config()

  if (config$anon_key == "") {
    return(list(
      success = FALSE,
      error = "SUPABASE_ANON_KEY not configured"
    ))
  }

  tryCatch({
    res <- POST(
      url = paste0(config$url, "/rest/v1/rpc/get_user_profile"),
      add_headers(
        "Content-Type" = "application/json",
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      ),
      body = list(
        p_user_id = user_id
      ),
      encode = "json"
    )

    if (status_code(res) != 200) {
      return(list(
        success = FALSE,
        error = paste("API error:", status_code(res))
      ))
    }

    result <- content(res, as = "parsed", type = "application/json")
    return(result)

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Connection error:", e$message)
    ))
  })
}

#' Create a new user (for migration or admin use)
#'
#' @param username Character. Unique username.
#' @param password Character. Plain text password (will be hashed by Supabase).
#' @param role Character. "admin" or "user" (default: "user").
#' @param display_name Character. Display name (optional).
#' @param company Character. Company name (optional).
#' @return list with success, user_id, or error
#'
#' @examples
#' result <- create_user("newuser", "password123", role = "user")
create_user <- function(username, password, role = "user",
                        display_name = NULL, company = NULL) {
  config <- get_supabase_config()

  if (config$anon_key == "") {
    return(list(
      success = FALSE,
      error = "SUPABASE_ANON_KEY not configured"
    ))
  }

  tryCatch({
    body_params <- list(
      p_username = username,
      p_password = password,
      p_role = role
    )

    if (!is.null(display_name)) {
      body_params$p_display_name <- display_name
    }
    if (!is.null(company)) {
      body_params$p_company <- company
    }

    res <- POST(
      url = paste0(config$url, "/rest/v1/rpc/create_user"),
      add_headers(
        "Content-Type" = "application/json",
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      ),
      body = body_params,
      encode = "json"
    )

    if (status_code(res) != 200) {
      return(list(
        success = FALSE,
        error = paste("API error:", status_code(res))
      ))
    }

    result <- content(res, as = "parsed", type = "application/json")
    return(result)

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Connection error:", e$message)
    ))
  })
}

#' Reset login count for a user (admin function)
#'
#' @param user_id Character. The user's UUID.
#' @param app_name Character. App name or NULL to reset all apps.
#' @return list with success and affected_rows
reset_login_count <- function(user_id, app_name = NULL) {
  config <- get_supabase_config()

  if (config$anon_key == "") {
    return(list(
      success = FALSE,
      error = "SUPABASE_ANON_KEY not configured"
    ))
  }

  tryCatch({
    body_params <- list(p_user_id = user_id)
    if (!is.null(app_name)) {
      body_params$p_app_name <- tolower(app_name)
    }

    res <- POST(
      url = paste0(config$url, "/rest/v1/rpc/reset_login_count"),
      add_headers(
        "Content-Type" = "application/json",
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      ),
      body = body_params,
      encode = "json"
    )

    if (status_code(res) != 200) {
      return(list(
        success = FALSE,
        error = paste("API error:", status_code(res))
      ))
    }

    result <- content(res, as = "parsed", type = "application/json")
    return(result)

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Connection error:", e$message)
    ))
  })
}

# ==============================================================================
# Convenience Functions
# ==============================================================================

#' Complete login flow: authenticate then check limit
#'
#' @param username Character. Username.
#' @param password Character. Password.
#' @param app_name Character. App name for login limit tracking.
#' @return list with complete login result
#'
#' @examples
#' login_result <- login_user("admin", "12345", "brandedge")
#' if (login_result$success && login_result$allowed) {
#'   # User is authenticated and within login limit
#' }
login_user <- function(username, password, app_name) {
  message(sprintf("[login_user] Starting for user '%s', app '%s'", username, app_name))

  # Step 1: Authenticate
  auth_result <- authenticate_user(username, password)
  message(sprintf("[login_user] authenticate_user result: success=%s", isTRUE(auth_result$success)))

  if (!isTRUE(auth_result$success)) {
    return(list(
      success = FALSE,
      allowed = FALSE,
      error = auth_result$error %||% "Authentication failed"
    ))
  }

  # Step 2: Check login limit
  message(sprintf("[login_user] Checking login limit for user_id=%s", auth_result$user_id))
  limit_result <- check_login_limit(auth_result$user_id, app_name)
  message(sprintf("[login_user] check_login_limit result: allowed=%s", isTRUE(limit_result$allowed)))

  if (!isTRUE(limit_result$allowed)) {
    return(list(
      success = TRUE,  # Authentication succeeded
      allowed = FALSE,  # But login limit exceeded
      user_id = auth_result$user_id,
      role = auth_result$role,
      error = limit_result$error %||% "Login limit exceeded"
    ))
  }

  # Success: authenticated and within limit
  return(list(
    success = TRUE,
    allowed = TRUE,
    user_id = auth_result$user_id,
    username = username,
    role = auth_result$role,
    display_name = auth_result$display_name,
    company = auth_result$company,
    remaining_logins = limit_result$remaining,
    is_admin = isTRUE(limit_result$is_admin)
  ))
}

# ==============================================================================
# Test Function (for development)
# ==============================================================================

#' Test Supabase connection
#' @return TRUE if connection works, FALSE otherwise
test_supabase_connection <- function() {
  config <- get_supabase_config()

  cat("Supabase URL:", config$url, "\n")
  cat("API Key configured:", ifelse(config$anon_key != "", "Yes", "No"), "\n")

  if (config$anon_key == "") {
    cat("ERROR: SUPABASE_ANON_KEY not set\n")
    return(FALSE)
  }

  # Try a simple request
  tryCatch({
    res <- GET(
      url = paste0(config$url, "/rest/v1/"),
      add_headers(
        "apikey" = config$anon_key,
        "Authorization" = paste("Bearer", config$anon_key)
      )
    )

    if (status_code(res) == 200) {
      cat("SUCCESS: Supabase connection working\n")
      return(TRUE)
    } else {
      cat("ERROR: HTTP status", status_code(res), "\n")
      return(FALSE)
    }
  }, error = function(e) {
    cat("ERROR:", e$message, "\n")
    return(FALSE)
  })
}

# Legacy compatibility wrapper for analyze_customer_dynamics_new()
if (!exists("analyze_customer_dynamics_new", mode = "function")) {
  source("utils/tagpilot_customer_dynamics.R")
}

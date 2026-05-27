# Legacy compatibility wrapper for calculate_customer_tags.R
if (!exists("calculate_base_value_tags", mode = "function")) {
  source("utils/tagpilot_calculate_tags.R")
}

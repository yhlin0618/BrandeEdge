# BrandEdge 旗艦版模組 - Main Module Loader
# 支援10-30個屬性評分、市場賽道分析、品牌識別度策略等進階功能
# 最多支援20個品牌，1000則評論
# Version: 2.0 - Modularized Structure
# Last Updated: 2025-10-06

library(httr2)
library(jsonlite)
library(stringr)
library(markdown)

# ==========================================
# Load All Sub-modules
# ==========================================

# 1. Load shared functions first
source("modules/module_brandedge_shared.R")
message("✅ 已載入 BrandEdge 共用函數")

# 1.5 Load scoring module (core functionality - attribute extraction & scoring)
source("modules/module_brandedge_scoring.R")
message("✅ 已載入 BrandEdge 屬性評分模組")

# 2. Load market profile module
source("modules/module_brandedge_market_profile.R")
message("✅ 已載入市場輪廓分析模組")

# 3. Load market track module
source("modules/module_brandedge_market_track.R")
message("✅ 已載入市場賽道分析模組")

# 4. Load advanced DNA module
source("modules/module_brandedge_advanced_dna.R")
message("✅ 已載入進階DNA比較模組")

# 5. Load brand identity module
source("modules/module_brandedge_brand_identity.R")
message("✅ 已載入品牌識別度策略模組")

# 6. Load ideal point module
source("modules/module_brandedge_ideal_point.R")
message("✅ 已載入理想點分析模組")

# 7. Load positioning strategy module
source("modules/module_brandedge_positioning_strategy.R")
message("✅ 已載入定位策略分析模組")

# 8. Load legacy modules (for backward compatibility if needed)
# source("modules/module_brandedge_legacy.R")
# message("✅ 已載入舊版模組（向下相容）")

message("✅ BrandEdge 旗艦版所有模組載入完成")
# ==========================================
# Main Wrapper Module UI/Server
# ==========================================
# This wrapper provides a placeholder UI for the brandedge_flagship page
# The actual analysis modules are accessed through other workflows

brandedgeFlagshipModuleUI <- function(id, module_config = NULL, lang_texts = NULL) {
  ns <- NS(id)
  tagList(
    div(
      class = "brandedge-flagship-container",
      h3(get_lang_text("ui.welcome.banner", default = "BrandEdge 品牌印記引擎")),
      p(get_lang_text("ui.welcome.description", default = "品牌分析與策略規劃主控台")),
      hr(),
      div(
        class = "info-message",
        icon("info-circle"),
        span("分析功能已整合至資料處理流程中，請從資料上傳開始操作。")
      )
    )
  )
}

brandedgeFlagshipModuleServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # This is a placeholder server
    # Actual analysis logic is in the submodules
    message("✅ BrandEdge Flagship 主模組伺服器已啟動")
  })
}

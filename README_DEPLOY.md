# BrandEdge Minimal GitHub Package

Main app: `app_brandedge.R`

This package contains only the files needed to run the BrandEdge Shiny app, plus selected shared scripts used by the app.

## Before running

1. Install required R packages listed in `app_brandedge.R` / `manifest.json`.
2. Copy `env/brandedge/.env.example` to `env/brandedge/.env` locally, or configure the same variables in your deployment platform.
3. Do not commit real `.env`, database files, downloads, logs, or Chrome profiles.
4. For Amazon review scraping, install Python Selenium in the Python used by `AMAZON_REVIEW_PYTHON`.

## Included major paths

- `app_brandedge.R`
- `config/`
- `modules/` BrandEdge runtime modules
- `utils/`
- `database/content/` language/content files
- `scripts/python/` Amazon BSR/review scrapers
- selected `scripts/global_scripts/` files required by login, OpenAI, assets, and upload sanitization

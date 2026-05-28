FROM rocker/shiny:4.5.0

# 安裝系統套件、Python、Chrome
RUN apt-get update && apt-get install -y \
    python3 python3-pip python3-venv \
    chromium chromium-driver \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    libpq-dev libmariadb-dev \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# 安裝 Python 套件（使用 venv 避免 PEP 668 限制）
RUN python3 -m venv /opt/venv && \
    /opt/venv/bin/pip install selenium

ENV PATH="/opt/venv/bin:$PATH"

# 設定 ChromeDriver 路徑（Debian/Ubuntu 上 chromium-driver 路徑）
ENV CHROME_BIN=/usr/bin/chromium
ENV CHROMEDRIVER_PATH=/usr/bin/chromedriver

# 安裝 R 套件
RUN Rscript -e "install.packages(c( \
    'shiny', 'bs4Dash', 'shinyjs', 'shinyWidgets', \
    'DT', 'DBI', 'RSQLite', 'RPostgres', \
    'RMariaDB', 'duckdb', \
    'dplyr', 'tidyr', 'ggplot2', 'plotly', 'GGally', \
    'tidyverse', 'stringr', 'tibble', 'purrr', \
    'httr', 'httr2', 'jsonlite', \
    'bcrypt', 'processx', 'future', 'furrr', \
    'yaml', 'readxl', 'writexl', 'pool', 'waiter', \
    'dotenv', 'bslib', 'markdown' \
), repos='https://cran.rstudio.com/')"

# 覆蓋 Shiny Server 預設設定檔（必須在 COPY . 之前單獨複製）
COPY shiny-server.conf /etc/shiny-server/shiny-server.conf

# 複製 app 到 Shiny Server 目錄
COPY . /srv/shiny-server/app/

# 開放 port
EXPOSE 10000

CMD ["Rscript", "-e", "shiny::runApp('/srv/shiny-server/app', host = '0.0.0.0', port = as.integer(Sys.getenv('PORT', '10000')))"]
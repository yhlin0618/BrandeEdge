FROM rocker/shiny:4.5.0

# 安裝系統套件、Python
RUN apt-get update && apt-get install -y \
    python3 python3-pip python3-venv \
    wget gnupg ca-certificates \
    libssl-dev libcurl4-openssl-dev libxml2-dev \
    libpq-dev libmariadb-dev \
    libfontconfig1-dev libharfbuzz-dev libfribidi-dev \
    libfreetype6-dev libpng-dev libtiff5-dev libjpeg-dev \
    && rm -rf /var/lib/apt/lists/*

# 安裝 Google Chrome stable（官方 .deb，比 Ubuntu apt 的 chromium 更穩定）
RUN wget -q https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb \
    && apt-get update \
    && apt-get install -y --no-install-recommends ./google-chrome-stable_current_amd64.deb \
    && rm google-chrome-stable_current_amd64.deb \
    && rm -rf /var/lib/apt/lists/*

# 安裝 Python 套件（使用 venv 避免 PEP 668 限制）
RUN python3 -m venv /opt/venv && \
    /opt/venv/bin/pip install selenium

# 預熱 Selenium Manager，讓它在 build 期間下載對應版本的 ChromeDriver
RUN /opt/venv/bin/python -c " \
from selenium import webdriver; \
from selenium.webdriver.chrome.options import Options; \
o = Options(); \
o.add_argument('--headless=new'); \
o.add_argument('--no-sandbox'); \
o.add_argument('--disable-setuid-sandbox'); \
o.add_argument('--disable-dev-shm-usage'); \
o.binary_location = '/usr/bin/google-chrome-stable'; \
d = webdriver.Chrome(options=o); \
d.quit(); \
print('Selenium Manager warmup OK') \
" 2>&1 | tail -5 || echo "Chrome warmup skipped (non-fatal)"

ENV PATH="/opt/venv/bin:$PATH"

# Google Chrome stable 的 binary 路徑
ENV CHROME_BIN=/usr/bin/google-chrome-stable
# 不設定 CHROMEDRIVER_PATH，讓 Selenium Manager 自動管理

# 安裝 R 套件
RUN Rscript -e "install.packages(c( \
    'shiny', 'bs4Dash', 'shinyjs', 'shinyWidgets', \
    'DT', 'DBI', 'RSQLite', 'RPostgres', \
    'RMariaDB', \
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
#!/usr/bin/env python3
"""Fetch Amazon review preview rows for ASINs with Selenium.

Input JSON is read from the first command-line argument path, or stdin:
  {
    "asins": ["B09X18QWLT"],
    "reviews_per_asin": 40,
    "marketplace": "US",
    "random_sample": true,
    "max_pages": 10
  }

Output JSON is written to stdout only. Diagnostic logs are written to stderr.

Dependencies:
  pip install selenium
  Chrome or Chromium must be available for Selenium Manager.
"""

from __future__ import annotations

import datetime as dt
import http.cookies
import json
import os
import random
import re
import sys
import time
import traceback
import urllib.parse
from pathlib import Path
from typing import Any

try:
    from selenium import webdriver
    from selenium.common.exceptions import TimeoutException, WebDriverException
    from selenium.webdriver.chrome.options import Options
    from selenium.webdriver.common.by import By
    from selenium.webdriver.remote.webdriver import WebDriver
    from selenium.webdriver.remote.webelement import WebElement
    from selenium.webdriver.support import expected_conditions as EC
    from selenium.webdriver.support.ui import WebDriverWait
except Exception as exc:  # noqa: BLE001 - keep CLI JSON contract when selenium is missing.
    webdriver = None  # type: ignore[assignment]
    TimeoutException = Exception  # type: ignore[misc,assignment]
    WebDriverException = Exception  # type: ignore[misc,assignment]
    Options = None  # type: ignore[assignment]
    By = None  # type: ignore[assignment]
    EC = None  # type: ignore[assignment]
    WebDriverWait = None  # type: ignore[assignment]
    SELENIUM_IMPORT_ERROR: Exception | None = exc
else:
    SELENIUM_IMPORT_ERROR = None


AMAZON_BASE_URLS = {
    "US": "https://www.amazon.com",
}
SOURCE_NAME = "amazon_selenium_scraper"
DEFAULT_DEBUG_LOG_PATH = "logs/amazon_review_selenium_scraper_debug.log"
USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/125.0.0.0 Safari/537.36"
)
BLOCKED_RESPONSE_TYPES = {"continue_shopping", "sign_in_required", "robot_check", "captcha"}


def now_iso() -> str:
    return dt.datetime.now(dt.timezone.utc).isoformat()


def emit(payload: dict[str, Any]) -> None:
    print(json.dumps(payload, ensure_ascii=False))


def debug_log_path() -> Path:
    raw_path = os.getenv("AMAZON_REVIEW_DEBUG_LOG", DEFAULT_DEBUG_LOG_PATH).strip()
    path = Path(raw_path).expanduser()
    if not path.is_absolute():
        path = Path.cwd() / path
    return path


def log(message: str) -> None:
    timestamped_message = f"[{now_iso()}] {message}"
    print(timestamped_message, file=sys.stderr, flush=True)
    try:
        path = debug_log_path()
        path.parent.mkdir(parents=True, exist_ok=True)
        with path.open("a", encoding="utf-8") as log_file:
            log_file.write(timestamped_message + "\n")
    except Exception:
        # Logging must never break stdout JSON contract.
        pass


def log_driver_state(driver: WebDriver, label: str) -> None:
    try:
        log(f"{label}: current_url={driver.current_url}, title={driver.title!r}")
    except Exception as exc:  # noqa: BLE001 - diagnostics should not interrupt scraping.
        log(f"{label}: unable to read driver state: {exc}")


def log_exception(label: str, exc: BaseException) -> None:
    log(f"{label}: {type(exc).__name__}: {exc}")
    log(traceback.format_exc().strip())


def clean_text(value: str | None) -> str:
    if value is None:
        return ""
    return re.sub(r"\s+", " ", value).strip()


def parse_bool_env(name: str, default: bool = True) -> bool:
    raw_value = os.getenv(name, "").strip().lower()
    if not raw_value:
        return default
    return raw_value not in {"0", "false", "no", "off"}


def parse_star_text(value: str) -> float | None:
    match = re.search(r"([1-5](?:\.\d)?)\s+out\s+of\s+5", value, flags=re.I)
    return float(match.group(1)) if match else None


def clean_review_title(title: str) -> str:
    title = clean_text(title)
    title = re.sub(r"^\s*[1-5](?:\.\d)?\s+out\s+of\s+5\s+stars\s*", "", title, flags=re.I)
    title = re.sub(r"^\s*[1-5](?:\.\d)?\s+out\s+of\s+5\s*", "", title, flags=re.I)
    return clean_text(title)


def parse_review_date(date_text: str) -> str:
    cleaned = re.sub(r"^Reviewed in .*? on ", "", date_text).strip()
    return clean_text(cleaned)


def fallback_review_content(element_text: str) -> str:
    lines = [clean_text(line) for line in element_text.splitlines()]
    lines = [line for line in lines if line]
    if not lines:
        return ""

    filtered_lines = []
    skip_patterns = (
        r"^[1-5](?:\.\d)?\s+out\s+of\s+5\s+stars$",
        r"^reviewed in .* on .*$",
        r"^verified purchase$",
        r"^helpful$",
        r"^\d+\s+people found this helpful$",
    )
    for line in lines:
        lowered = line.lower()
        if any(re.search(pattern, lowered, flags=re.I) for pattern in skip_patterns):
            continue
        if lowered in {"read more", "report", "show more"}:
            continue
        filtered_lines.append(line)

    if not filtered_lines:
        return ""

    # Prefer a longer line because Amazon review bodies are often the longest text block.
    return max(filtered_lines, key=len)


def product_page_url(base_url: str, asin: str) -> str:
    return urllib.parse.urljoin(base_url, f"/dp/{asin}")


def review_page_url(base_url: str, asin: str, page_number: int) -> str:
    query = urllib.parse.urlencode(
        {
            "ie": "UTF8",
            "reviewerType": "all_reviews",
            "sortBy": "recent",
            "pageNumber": page_number,
        }
    )
    return urllib.parse.urljoin(base_url, f"/product-reviews/{asin}/?{query}")


def mobile_review_page_url(base_url: str, asin: str, page_number: int) -> str:
    query = urllib.parse.urlencode(
        {
            "ie": "UTF8",
            "reviewerType": "all_reviews",
            "sortBy": "recent",
            "pageNumber": page_number,
        }
    )
    return urllib.parse.urljoin(base_url, f"/gp/aw/reviews/{asin}/?{query}")


def classify_amazon_response(page_html: str, title: str = "") -> str:
    lowered = page_html.lower()
    text = clean_text(re.sub(r"<[^>]+>", " ", page_html)).lower()
    page_title = title.lower()

    if 'data-hook="review"' in lowered or "data-hook='review'" in lowered or "customer_review-" in lowered:
        return "reviews"
    if "amazon sign-in" in text or "sign in" in page_title and "amazon" in page_title:
        return "sign_in_required"
    if "click the button below to continue shopping" in text:
        return "continue_shopping"
    if "robot check" in text or "enter the characters you see below" in text:
        return "robot_check"
    if "captcha" in lowered and ("validatecaptcha" in lowered or "captchacharacters" in lowered):
        return "captcha"
    return "unknown"


def normalize_response_type(response_type: str, page_type: str) -> str:
    if response_type == "continue_shopping":
        return "amazon_blocked_continue_shopping"
    if response_type in {"robot_check", "captcha"}:
        return "amazon_robot_check"
    if response_type == "sign_in_required":
        return "sign_in_required"
    if response_type == "http_404":
        return "product_unavailable" if page_type == "product" else "review_page_not_found"
    return response_type


def friendly_status_message(scrape_status: str, error_code: str = "") -> str:
    if scrape_status in {"ok", "featured_only", "partial"}:
        return "已取得評論"
    if scrape_status == "blocked":
        return "Amazon 暫時阻擋請求，請稍後再試"
    if scrape_status == "no_reviews":
        return "此 ASIN 暫無可抓取評論"
    if error_code in {"product_unavailable", "review_page_not_found"}:
        return "商品頁或評論頁暫時不可訪問"
    if error_code in {"amazon_blocked_continue_shopping", "sign_in_required", "amazon_robot_check"}:
        return "Amazon 暫時阻擋請求，請稍後再試"
    return "評論抓取失敗，請稍後再試"


def build_driver() -> WebDriver:
    if SELENIUM_IMPORT_ERROR is not None or webdriver is None or Options is None:
        raise RuntimeError(f"Selenium is unavailable: {SELENIUM_IMPORT_ERROR}")

    log("Building Chrome Selenium driver.")
    options = Options()
    if parse_bool_env("AMAZON_REVIEW_HEADLESS", default=True):
        options.add_argument("--headless=new")
    options.add_argument("--window-size=1440,1400")
    options.add_argument("--disable-gpu")
    options.add_argument("--no-sandbox")
    options.add_argument("--disable-dev-shm-usage")
    options.add_argument("--disable-blink-features=AutomationControlled")
    options.add_argument(f"--user-agent={USER_AGENT}")
    options.add_experimental_option("excludeSwitches", ["enable-automation"])
    options.add_experimental_option("useAutomationExtension", False)

    driver = webdriver.Chrome(options=options)
    driver.set_page_load_timeout(45)
    # Keep implicit waits low because review parsing probes several fallback selectors.
    # Missing selectors are common on Amazon pages and otherwise add seconds per block.
    driver.implicitly_wait(0)
    log("Chrome Selenium driver started.")
    return driver


def apply_cookie_header(driver: WebDriver, base_url: str) -> None:
    cookie_header = os.getenv("AMAZON_REVIEW_COOKIE", "").strip()
    if not cookie_header:
        log("AMAZON_REVIEW_COOKIE not set; continuing without injected cookies.")
        return

    try:
        log("Injecting AMAZON_REVIEW_COOKIE into Selenium session.")
        driver.get(base_url + "/")
        cookie = http.cookies.SimpleCookie()
        cookie.load(cookie_header)
        injected_count = 0
        for key, morsel in cookie.items():
            try:
                driver.add_cookie(
                    {
                        "name": key,
                        "value": morsel.value,
                        "domain": ".amazon.com",
                        "path": morsel["path"] or "/",
                    }
                )
                injected_count += 1
            except WebDriverException as exc:
                log(f"Skipping Amazon cookie {key}: {exc}")
        driver.get(base_url + "/")
        log(f"Cookie injection complete. injected_count={injected_count}")
    except Exception as exc:  # noqa: BLE001 - cookie injection is best-effort.
        log_exception("Amazon cookie injection failed", exc)


def wait_for_page(driver: WebDriver, timeout: int = 10) -> None:
    if WebDriverWait is None or EC is None or By is None:
        return
    try:
        WebDriverWait(driver, timeout).until(EC.presence_of_element_located((By.TAG_NAME, "body")))
    except TimeoutException:
        log(f"Timed out waiting for body tag after {timeout}s.")
        return


def safe_find_text(element: WebElement, selectors: list[str]) -> str:
    if By is None:
        return ""
    for selector in selectors:
        try:
            text = element.find_element(By.CSS_SELECTOR, selector).text
            text = clean_text(text)
            if text:
                return text
        except Exception:
            continue
    return ""


def safe_find_attr(element: WebElement, selectors: list[str], attr_name: str) -> str:
    if By is None:
        return ""
    for selector in selectors:
        try:
            value = element.find_element(By.CSS_SELECTOR, selector).get_attribute(attr_name)
            value = clean_text(value)
            if value:
                return value
        except Exception:
            continue
    return ""


def parse_review_element(element: WebElement, asin: str) -> dict[str, Any]:
    title = clean_review_title(safe_find_text(element, ['[data-hook="review-title"]']))
    content = safe_find_text(element, ['[data-hook="review-body"]'])
    if not content:
        content = fallback_review_content(element.text)
    author = safe_find_text(
        element,
        [
            ".a-profile-name",
            '[data-hook="genome-widget"]',
            '[data-hook="review-author"]',
        ],
    )
    date_text = safe_find_text(element, ['[data-hook="review-date"]'])
    verified_text = safe_find_text(element, ['[data-hook="avp-badge"]'])
    star_text = safe_find_text(
        element,
        [
            '[data-hook="review-star-rating"]',
            '[data-hook="cmps-review-star-rating"]',
        ],
    )
    aria_label = safe_find_attr(
        element,
        [
            '[data-hook="review-star-rating"]',
            '[data-hook="cmps-review-star-rating"]',
            ".a-icon-alt",
        ],
        "aria-label",
    )
    star = parse_star_text(star_text) or parse_star_text(aria_label)

    return {
        "asin": asin,
        "title": title,
        "content": content,
        "star": star,
        "review_date": parse_review_date(date_text),
        "author": author,
        "verified": bool(re.search(r"verified purchase", verified_text, flags=re.I)),
        "source": SOURCE_NAME,
    }


def parse_reviews_from_dom(driver: WebDriver, asin: str) -> list[dict[str, Any]]:
    if By is None:
        return []
    review_elements = driver.find_elements(By.CSS_SELECTOR, 'div[data-hook="review"], [id^="customer_review-"]')
    log(f"{asin}: found {len(review_elements)} review DOM blocks.")
    reviews = [parse_review_element(element, asin) for element in review_elements]
    parsed_reviews = [review for review in reviews if review.get("content") or review.get("title")]
    log(f"{asin}: parsed {len(parsed_reviews)} non-empty reviews from DOM.")
    return parsed_reviews


def dedupe_reviews(reviews: list[dict[str, Any]]) -> list[dict[str, Any]]:
    deduped: list[dict[str, Any]] = []
    seen: set[tuple[str, str, str]] = set()
    for review in reviews:
        key = (
            str(review.get("asin", "")),
            str(review.get("title", "")),
            str(review.get("content", ""))[:200],
        )
        if key in seen:
            continue
        seen.add(key)
        deduped.append(review)
    return deduped


def scrape_page(driver: WebDriver, url: str, asin: str) -> tuple[list[dict[str, Any]], str]:
    started = time.perf_counter()
    log(f"{asin}: loading review URL: {url}")
    driver.get(url)
    wait_for_page(driver)
    time.sleep(0.8 + random.random() * 1.2)
    response_type = classify_amazon_response(driver.page_source, driver.title)
    elapsed = round(time.perf_counter() - started, 3)
    log(
        f"{asin}: review URL loaded. response_type={response_type}, "
        f"elapsed={elapsed}s, title={driver.title!r}"
    )
    if response_type != "reviews":
        log_driver_state(driver, f"{asin}: non-review response")
        return [], response_type
    reviews = parse_reviews_from_dom(driver, asin)
    log(f"{asin}: review URL produced {len(reviews)} reviews.")
    return reviews, response_type


def scrape_reviews_for_asin(
    driver: WebDriver,
    asin: str,
    base_url: str,
    reviews_per_asin: int,
    max_pages: int,
    random_sample: bool,
) -> dict[str, Any]:
    started = time.perf_counter()
    del random_sample  # Selenium mode uses top-k reviews to reduce page loads and blocks.
    candidates: list[dict[str, Any]] = []
    error_message = ""
    scrape_source = "selenium_review_page"
    last_response_type = "not_requested"

    log(
        f"{asin}: scrape start. reviews_per_asin={reviews_per_asin}, "
        f"max_pages={max_pages}, base_url={base_url}"
    )
    try:
        product_url = product_page_url(base_url, asin)
        log(f"{asin}: loading product URL: {product_url}")
        driver.get(product_url)
        wait_for_page(driver)
        time.sleep(0.7 + random.random())
        product_response_type = classify_amazon_response(driver.page_source, driver.title)
        log(f"{asin}: product response_type={product_response_type}, title={driver.title!r}")
        if product_response_type in BLOCKED_RESPONSE_TYPES:
            error_message = normalize_response_type(product_response_type, "product")
            log(f"{asin}: product page blocked. error_message={error_message}")
        else:
            product_reviews = parse_reviews_from_dom(driver, asin)
            log(f"{asin}: product page produced {len(product_reviews)} candidate reviews.")
            if product_reviews:
                candidates.extend(product_reviews)
                candidates = dedupe_reviews(candidates)
                scrape_source = "selenium_product_page_featured_reviews"
    except Exception as exc:  # noqa: BLE001 - product warm-up is best-effort.
        error_message = str(exc)
        log_exception(f"{asin}: product warm-up failed", exc)

    if len(candidates) < reviews_per_asin:
        scrape_source = "selenium_review_page"
        for page_number in range(1, max_pages + 1):
            page_reviews: list[dict[str, Any]] = []
            log(f"{asin}: starting review page_number={page_number}, candidates={len(candidates)}")
            for current_url in (
                review_page_url(base_url, asin, page_number),
                mobile_review_page_url(base_url, asin, page_number),
            ):
                try:
                    page_reviews, response_type = scrape_page(driver, current_url, asin)
                    last_response_type = response_type
                    if response_type in BLOCKED_RESPONSE_TYPES:
                        error_message = normalize_response_type(response_type, "review")
                        log(
                            f"{asin}: blocked on page_number={page_number}, "
                            f"response_type={response_type}, error_message={error_message}"
                        )
                        break
                    if page_reviews:
                        error_message = ""
                        log(
                            f"{asin}: accepted page_number={page_number}, "
                            f"page_reviews={len(page_reviews)}"
                        )
                        break
                except Exception as exc:  # noqa: BLE001 - try the alternate URL before giving up.
                    error_message = str(exc)
                    log_exception(f"{asin}: review page failed ({current_url})", exc)
                    continue

            if last_response_type in BLOCKED_RESPONSE_TYPES:
                log(f"{asin}: stopping due to blocked response. candidates={len(candidates)}")
                break
            if not page_reviews:
                log(
                    f"{asin}: stopping because page_number={page_number} returned no reviews. "
                    f"last_response_type={last_response_type}, candidates={len(candidates)}"
                )
                break

            previous_candidate_count = len(candidates)
            candidates.extend(page_reviews)
            candidates = dedupe_reviews(candidates)
            new_candidate_count = len(candidates) - previous_candidate_count
            log(
                f"{asin}: candidates after page_number={page_number}: {len(candidates)} "
                f"(new_unique={new_candidate_count})"
            )
            if previous_candidate_count > 0 and new_candidate_count <= 0:
                log(
                    f"{asin}: stopping because page_number={page_number} added no unique reviews. "
                    "Amazon may be returning the same review page for pagination."
                )
                break
            if len(candidates) >= reviews_per_asin:
                log(f"{asin}: reached top-k target. candidates={len(candidates)}")
                break

    sampled_reviews = candidates[:reviews_per_asin]

    elapsed = round(time.perf_counter() - started, 3)
    reviews_found = len(sampled_reviews)
    if last_response_type in BLOCKED_RESPONSE_TYPES:
        status = "blocked"
    elif error_message and reviews_found == 0:
        status = "error"
    elif error_message:
        status = "partial"
    elif reviews_found > 0:
        status = "featured_only" if scrape_source == "selenium_product_page_featured_reviews" else "ok"
    else:
        status = "no_reviews"

    status_message = friendly_status_message(status, error_message)
    log(
        f"{asin}: scrape complete. status={status}, reviews_found={reviews_found}, "
        f"candidates={len(candidates)}, error_message={error_message!r}, elapsed={elapsed}s"
    )

    return {
        "reviews": sampled_reviews,
        "summary": {
            "asin": asin,
            "reviews_requested": reviews_per_asin,
            "reviews_found": reviews_found,
            "candidate_reviews_collected": len(candidates),
            "scrape_status": status,
            "error_message": error_message,
            "status_message": status_message,
            "scrape_source": scrape_source,
            "elapsed_seconds": elapsed,
            "reviews_per_second": round(reviews_found / elapsed, 3) if elapsed > 0 else reviews_found,
        },
    }


def parse_payload() -> dict[str, Any]:
    if len(sys.argv) > 1:
        with open(sys.argv[1], "r", encoding="utf-8") as payload_file:
            raw_payload = payload_file.read()
    else:
        raw_payload = sys.stdin.read()
    return json.loads(raw_payload or "{}")


def main() -> None:
    started_at = now_iso()
    start_time = time.perf_counter()
    all_reviews: list[dict[str, Any]] = []
    summaries: list[dict[str, Any]] = []
    error_message = ""
    driver: WebDriver | None = None

    try:
        payload = parse_payload()
        asins = payload.get("asins") or []
        if isinstance(asins, str):
            asins = [asins]
        asins = [str(asin).strip().upper() for asin in asins if str(asin).strip()]

        reviews_per_asin = int(payload.get("reviews_per_asin") or 40)
        reviews_per_asin = max(1, min(100, reviews_per_asin))
        max_pages = max(1, min(20, int(payload.get("max_pages") or 10)))
        marketplace = str(payload.get("marketplace") or "US").upper()
        random_sample = bool(payload.get("random_sample", True))
        base_url = AMAZON_BASE_URLS.get(marketplace)

        if not base_url:
            raise RuntimeError(f"Unsupported Amazon marketplace: {marketplace}")
        if not asins:
            raise RuntimeError("請提供至少一個 ASIN。")

        log(
            "Amazon Selenium review scraper started. "
            f"asins={len(asins)}, reviews_per_asin={reviews_per_asin}, "
            f"max_pages={max_pages}, marketplace={marketplace}, "
            f"headless={parse_bool_env('AMAZON_REVIEW_HEADLESS', default=True)}, "
            f"debug_log_path={debug_log_path()}"
        )
        driver = build_driver()
        apply_cookie_header(driver, base_url)

        for index, asin in enumerate(asins, start=1):
            log(f"Batch progress: {index}/{len(asins)} ASIN={asin}")
            result = scrape_reviews_for_asin(
                driver=driver,
                asin=asin,
                base_url=base_url,
                reviews_per_asin=reviews_per_asin,
                max_pages=max_pages,
                random_sample=random_sample,
            )
            all_reviews.extend(result["reviews"])
            summaries.append(result["summary"])
    except Exception as exc:  # noqa: BLE001 - never emit non-JSON from this script.
        error_message = str(exc)
        log_exception("Amazon Selenium review scraper failed", exc)
    finally:
        if driver is not None:
            try:
                log("Quitting Selenium driver.")
                driver.quit()
            except Exception as exc:  # noqa: BLE001 - shutdown should not hide scrape output.
                log_exception("Selenium driver quit failed", exc)

    elapsed_seconds = round(time.perf_counter() - start_time, 3)
    ended_at = now_iso()
    asin_count = len(summaries)
    total_reviews = len(all_reviews)
    timing = {
        "started_at": started_at,
        "ended_at": ended_at,
        "elapsed_seconds": elapsed_seconds,
        "asin_count": asin_count,
        "total_reviews_found": total_reviews,
        "avg_seconds_per_asin": round(elapsed_seconds / asin_count, 3) if asin_count else elapsed_seconds,
        "reviews_per_second": round(total_reviews / elapsed_seconds, 3) if elapsed_seconds > 0 else total_reviews,
        "debug_log_path": str(debug_log_path()),
    }

    log(
        "Amazon Selenium review scraper finished. "
        f"asin_count={asin_count}, total_reviews={total_reviews}, "
        f"elapsed_seconds={elapsed_seconds}, error_message={error_message!r}"
    )

    emit(
        {
            "status": "success" if all_reviews or summaries else "error",
            "results": all_reviews,
            "summary": summaries,
            "timing": timing,
            "error_message": error_message,
        }
    )


if __name__ == "__main__":
    main()

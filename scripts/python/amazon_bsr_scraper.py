#!/usr/bin/env python3
"""Fetch Amazon Best Sellers Rank and related category Top 10 ASINs.

Input JSON is read from the first command-line argument path, or stdin:
  {"asins": ["B09X18QWLT"]}

Output JSON is written to stdout:
  {
    "status": "success" | "error",
    "results": [...],
    "error_message": ""
  }
"""

from __future__ import annotations

import html
import json
import random
import re
import sys
import urllib.error
import urllib.parse
import urllib.request
from typing import Any


AMAZON_BASE_URL = "https://www.amazon.com"
USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
    "AppleWebKit/537.36 (KHTML, like Gecko) "
    "Chrome/125.0.0.0 Safari/537.36"
)


def emit(status: str, results: list[dict[str, Any]] | None = None, error_message: str = "") -> None:
    payload = {
        "status": status,
        "results": results or [],
        "error_message": error_message,
    }
    print(json.dumps(payload, ensure_ascii=False))


def fetch_html(url: str) -> str:
    request = urllib.request.Request(
        url,
        headers={
            "User-Agent": USER_AGENT,
            "Accept-Language": "en-US,en;q=0.9",
            "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        },
    )

    try:
        with urllib.request.urlopen(request, timeout=25) as response:
            status_code = getattr(response, "status", 200)
            body = response.read().decode("utf-8", errors="replace")
    except urllib.error.HTTPError as exc:
        raise RuntimeError(
            "無法讀取 Amazon 商品頁，可能是頁面暫時無法存取或 Amazon 阻擋爬蟲。"
            f" HTTP {exc.code}"
        ) from exc
    except urllib.error.URLError as exc:
        raise RuntimeError(
            "無法讀取 Amazon 商品頁，可能是頁面暫時無法存取或 Amazon 阻擋爬蟲。"
            f" {exc.reason}"
        ) from exc

    if status_code >= 400:
        raise RuntimeError(
            "無法讀取 Amazon 商品頁，可能是頁面暫時無法存取或 Amazon 阻擋爬蟲。"
            f" HTTP {status_code}"
        )

    lowered = body.lower()
    if "captcha" in lowered or "robot check" in lowered:
        raise RuntimeError("無法讀取 Amazon 商品頁，Amazon 可能阻擋爬蟲或要求驗證。")

    return body


def strip_tags(fragment: str) -> str:
    text = re.sub(r"<script\b[^>]*>.*?</script>", " ", fragment, flags=re.I | re.S)
    text = re.sub(r"<style\b[^>]*>.*?</style>", " ", text, flags=re.I | re.S)
    text = re.sub(r"<[^>]+>", " ", text)
    text = html.unescape(text)
    return re.sub(r"\s+", " ", text).strip()


def find_bsr_fragment(product_html: str) -> str:
    marker = re.search(r"Best\s+Sellers\s+Rank", product_html, flags=re.I)
    if not marker:
        raise RuntimeError("找不到此商品的 Best Sellers Rank。")

    start = max(0, marker.start() - 500)
    end = min(len(product_html), marker.end() + 6000)
    return product_html[start:end]


def parse_rank_entries(product_html: str) -> list[dict[str, Any]]:
    fragment = find_bsr_fragment(product_html)
    text = strip_tags(fragment)
    rank_pattern = re.compile(
        r"#\s*([\d,]+)\s+in\s+(.+?)(?=\s+#\s*[\d,]+\s+in\s+|\s+Customer Reviews|\s+Date First Available|\s*$)",
        flags=re.I,
    )

    entries: list[dict[str, Any]] = []
    for match in rank_pattern.finditer(text):
        rank = int(match.group(1).replace(",", ""))
        category = re.sub(r"\s*\(.*$", "", match.group(2)).strip()
        category = re.sub(r"\s+See Top 100.*$", "", category, flags=re.I).strip()
        category = re.sub(r"\s+", " ", category)
        if category:
            entries.append({"rank": rank, "category": category, "category_url": None})

    if not entries:
        raise RuntimeError("找不到此商品的 Best Sellers Rank。")

    anchor_pattern = re.compile(r"""<a[^>]+href=(["'])(.*?)\1[^>]*>(.*?)</a>""", flags=re.I | re.S)
    anchors = [
        {
            "href": urllib.parse.urljoin(AMAZON_BASE_URL, html.unescape(match.group(2))),
            "text": strip_tags(match.group(3)).lower(),
        }
        for match in anchor_pattern.finditer(fragment)
    ]

    for entry in entries:
        category_lower = entry["category"].lower()
        matching_anchor = next(
            (
                anchor
                for anchor in anchors
                if "bestsellers" in anchor["href"].lower()
                and (category_lower in anchor["text"] or anchor["text"] in category_lower)
            ),
            None,
        )
        if matching_anchor:
            entry["category_url"] = matching_anchor["href"]

    return entries


def extract_ranked_asins(category_html: str, limit: int = 100, start_rank: int = 1) -> list[dict[str, Any]]:
    seen: set[str] = set()
    candidates = re.findall(r'data-asin="([A-Z0-9]{10})"', category_html)
    candidates.extend(re.findall(r"/dp/([A-Z0-9]{10})(?:[/?]|$)", category_html))

    results: list[dict[str, Any]] = []
    for candidate in candidates:
        if candidate in seen:
            continue
        seen.add(candidate)
        results.append(
            {
                "top_rank": start_rank + len(results),
                "top_asin": candidate,
                "product_url": urllib.parse.urljoin(AMAZON_BASE_URL, f"/dp/{candidate}"),
            }
        )
        if len(results) >= limit:
            break

    if not results:
        raise RuntimeError("找不到該類別的商品 ASIN。")

    return results


def page_url(base_url: str, page_number: int) -> str:
    if page_number <= 1:
        return base_url

    parsed = urllib.parse.urlparse(base_url)
    query_pairs = urllib.parse.parse_qsl(parsed.query, keep_blank_values=True)
    query_pairs = [(key, value) for key, value in query_pairs if key.lower() != "pg"]
    query_pairs.append(("pg", str(page_number)))
    return urllib.parse.urlunparse(parsed._replace(query=urllib.parse.urlencode(query_pairs)))


def fetch_ranked_list(base_url: str, limit: int = 100, max_pages: int = 4) -> list[dict[str, Any]]:
    ranked_items: list[dict[str, Any]] = []
    seen_asins: set[str] = set()

    for page_number in range(1, max_pages + 1):
        try:
            current_html = fetch_html(page_url(base_url, page_number))
            page_items = extract_ranked_asins(
                current_html,
                limit=limit,
                start_rank=len(ranked_items) + 1,
            )
        except Exception:
            if page_number == 1 and not ranked_items:
                raise
            break

        new_items = []
        for item in page_items:
            if item["top_asin"] in seen_asins:
                continue
            seen_asins.add(item["top_asin"])
            item["top_rank"] = len(ranked_items) + len(new_items) + 1
            new_items.append(item)

        if not new_items:
            break

        ranked_items.extend(new_items)
        if len(ranked_items) >= limit:
            break

    if not ranked_items:
        raise RuntimeError("找不到該類別的商品 ASIN。")

    return ranked_items[:limit]


def build_category_urls(category_url: str) -> dict[str, str | None]:
    parsed = urllib.parse.urlparse(category_url)
    match = re.search(r"/gp/bestsellers/([^/]+)/([^/?]+)", parsed.path)
    if not match:
        return {"bestsellers": category_url, "new_releases": None, "movers": None}

    main_slug, sub_id = match.groups()
    return {
        "bestsellers": urllib.parse.urljoin(AMAZON_BASE_URL, f"/gp/bestsellers/{main_slug}/{sub_id}/"),
        "new_releases": urllib.parse.urljoin(AMAZON_BASE_URL, f"/gp/new-releases/{main_slug}/{sub_id}/"),
        "movers": urllib.parse.urljoin(AMAZON_BASE_URL, f"/gp/movers-and-shakers/{main_slug}/{sub_id}/"),
    }


def sample_items(items: list[dict[str, Any]], sample_size: int, seed: str) -> list[dict[str, Any]]:
    if len(items) <= sample_size:
        return items

    rng = random.Random(seed)
    return sorted(rng.sample(items, sample_size), key=lambda item: item["top_rank"])


def with_segment(items: list[dict[str, Any]], segment: str) -> list[dict[str, Any]]:
    return [{**item, "source_segment": segment} for item in items]


def segment_for_rank(rank: int) -> str:
    if 11 <= rank <= 30:
        return "對手"
    if 31 <= rank <= 100:
        return "中段"
    return "補樣"


def ensure_minimum_bsr_samples(
    selected_items: list[dict[str, Any]],
    fill_candidates: list[dict[str, Any]],
    minimum_count: int,
    seed: str,
) -> list[dict[str, Any]]:
    if len(selected_items) >= minimum_count:
        return selected_items

    seen_asins = {item["top_asin"] for item in selected_items}
    available_candidates = [
        item
        for item in fill_candidates
        if item["top_asin"] not in seen_asins
    ]
    needed_count = minimum_count - len(selected_items)
    sampled_items = sample_items(available_candidates, needed_count, seed)

    selected_items.extend(
        {
            **item,
            "source_segment": segment_for_rank(item["top_rank"]),
        }
        for item in sampled_items
    )
    return selected_items


def cap_results_to_limit(items: list[dict[str, Any]], limit: int = 40) -> list[dict[str, Any]]:
    if len(items) <= limit:
        return items

    remaining_items = list(items)
    removal_priority = ["New Releases Top 100", "對手", "中段"]
    for segment in removal_priority:
        if len(remaining_items) <= limit:
            break

        segment_items = [item for item in remaining_items if item.get("source_segment") == segment]
        removable_count = min(len(remaining_items) - limit, len(segment_items))
        if removable_count <= 0:
            continue

        # Keep better-ranked items in the same segment; remove lower-priority tail rows first.
        removable_asins = {
            item["top_asin"]
            for item in sorted(segment_items, key=lambda row: row.get("top_rank") or 10**9, reverse=True)[:removable_count]
        }
        remaining_items = [
            item
            for item in remaining_items
            if not (item.get("source_segment") == segment and item["top_asin"] in removable_asins)
        ]

    return remaining_items[:limit]


def select_bsr_competitors(bsr_items: list[dict[str, Any]], input_asin: str, source_rank: int) -> list[dict[str, Any]]:
    input_asin = input_asin.upper()
    competitor_items = [item for item in bsr_items if item["top_asin"].upper() != input_asin]
    leading_items = [item for item in competitor_items if item["top_rank"] <= 10]
    opponent_items = [item for item in competitor_items if 11 <= item["top_rank"] <= 30]
    mid_items = [item for item in competitor_items if 31 <= item["top_rank"] <= 100]

    has_same_tier = source_rank <= 100
    same_tier_items = []
    if has_same_tier:
        same_tier_items = [
            item
            for item in competitor_items
            if abs(item["top_rank"] - source_rank) <= 5
        ]

    opponent_sample_size = 8 + (5 if not has_same_tier else 0)
    mid_sample_size = 10 + (5 if not has_same_tier else 0)

    selected = []
    selected.extend(with_segment(leading_items, "領先群"))
    selected.extend(with_segment(sample_items(opponent_items, opponent_sample_size, f"{input_asin}:opponent"), "對手"))
    selected.extend(with_segment(same_tier_items, "同段位"))
    selected.extend(with_segment(sample_items(mid_items, mid_sample_size, f"{input_asin}:mid"), "中段"))

    deduped: list[dict[str, Any]] = []
    seen_asins: set[str] = set()
    for item in selected:
        if item["top_asin"] in seen_asins:
            continue
        seen_asins.add(item["top_asin"])
        deduped.append(item)

    fill_candidates = opponent_items + mid_items
    return ensure_minimum_bsr_samples(
        selected_items=deduped,
        fill_candidates=fill_candidates,
        minimum_count=40,
        seed=f"{input_asin}:fill-to-40",
    )


def append_external_list_items(
    selected_items: list[dict[str, Any]],
    category_urls: dict[str, str | None],
    bsr_items: list[dict[str, Any]],
) -> list[dict[str, Any]]:
    bsr_asins = {item["top_asin"] for item in bsr_items}
    external_sources = [
        ("new_releases", "New Releases Top 100"),
        ("movers", "Movers & Shakers Top 100"),
    ]

    extra_items: list[dict[str, Any]] = []
    selected_asins = {item["top_asin"] for item in selected_items}
    for url_key, segment in external_sources:
        url = category_urls.get(url_key)
        if not url:
            continue
        try:
            ranked_items = fetch_ranked_list(url, limit=100)
        except Exception:
            continue

        for item in ranked_items:
            if item["top_asin"] in bsr_asins or item["top_asin"] in selected_asins:
                continue
            selected_asins.add(item["top_asin"])
            extra_items.append({**item, "source_segment": segment})

    selected_items.extend(extra_items)
    return selected_items


def scrape_for_asin(input_asin: str) -> list[dict[str, Any]]:
    product_url = urllib.parse.urljoin(AMAZON_BASE_URL, f"/dp/{input_asin}")
    product_html = fetch_html(product_url)
    rank_entries = parse_rank_entries(product_html)
    best_entry = min(rank_entries, key=lambda item: item["rank"])

    if not best_entry.get("category_url"):
        raise RuntimeError("找不到該 Best Sellers Rank 類別的排行榜連結。")

    category_urls = build_category_urls(best_entry["category_url"])
    bsr_items = fetch_ranked_list(category_urls["bestsellers"] or best_entry["category_url"], limit=100)
    selected_items = select_bsr_competitors(
        bsr_items,
        input_asin=input_asin,
        source_rank=best_entry["rank"],
    )
    selected_items = append_external_list_items(selected_items, category_urls, bsr_items)
    selected_items = cap_results_to_limit(selected_items, limit=40)

    return [
        {
            "input_asin": input_asin,
            "source_category": best_entry["category"],
            "source_category_rank": best_entry["rank"],
            "source_segment": item["source_segment"],
            "top_rank": item["top_rank"],
            "top_asin": item["top_asin"],
            "product_url": item["product_url"],
            "scrape_status": "success",
            "error_message": "",
        }
        for item in selected_items
    ]


def main() -> None:
    try:
        if len(sys.argv) > 1:
            with open(sys.argv[1], "r", encoding="utf-8") as payload_file:
                raw_payload = payload_file.read()
        else:
            raw_payload = sys.stdin.read()

        payload = json.loads(raw_payload or "{}")
        asins = payload.get("asins") or []
        if isinstance(asins, str):
            asins = [asins]
        asins = [str(asin).strip().upper() for asin in asins if str(asin).strip()]

        if not asins:
            emit("error", error_message="請提供至少一個 ASIN。")
            return

        all_results: list[dict[str, Any]] = []
        errors: list[str] = []
        for asin in asins:
            try:
                all_results.extend(scrape_for_asin(asin))
            except Exception as exc:  # noqa: BLE001 - CLI boundary should return JSON for all failures.
                errors.append(f"{asin}: {exc}")

        if all_results:
            emit("success", results=all_results, error_message="; ".join(errors))
        else:
            emit("error", results=[], error_message="; ".join(errors) or "Amazon 查詢失敗。")
    except Exception as exc:  # noqa: BLE001 - never emit non-JSON from this script.
        emit("error", error_message=str(exc))


if __name__ == "__main__":
    main()

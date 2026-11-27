#!/usr/bin/env python3
"""Turn the inventory's sure matches into links in clutter's collection log.

    scripts/jellyfin-inventory.py --offline      # or a fresh run
    scripts/import-inventory-links.py [--dry-run]

Reads cache/jellyfin-inventory/link-candidates.tsv and appends one line per
match to ~/Sync/clutter/collection.jsonl ($CLUTTER_COLLECTION):

    {"ts", "op": "link", "key": "D1365907", "version": "J32aa…",
     "via": "inventory:sure", "artist", "title"}

Taken: grade "sure" (artist and title agree), not flagged (one clutter album
only), the file album is in the Jellyfin album store, and it is not linked in
the log already. A match on a Tidal album that a Discogs release names in its
Location field goes to that release. Everything else stays in the TSV for
review. Append-only; run it again any time. The server reads the log at start.

A wrong link is taken off on the album page (×), which appends an "unlink".
"""

import argparse

import collection_log
import csv
import datetime
import glob
import json
import os
import sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--candidates", default=os.path.join(ROOT, "cache", "jellyfin-inventory", "link-candidates.tsv"))
    ap.add_argument("--cache", default=os.path.join(ROOT, "cache"))
    ap.add_argument("--store", default=os.path.expanduser(os.environ.get("CLUTTER_JELLYFIN_ALBUMS", "~/Sync/clutter/jellyfin-albums.json")))
    ap.add_argument("--log", default=os.path.expanduser(os.environ.get("CLUTTER_COLLECTION", "~/Sync/clutter/collection.jsonl")))
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()
    if collection_log.has_format(args.log):
        # link lines are not read in the album format: the review (Unsure matches) and
        # "Same album as…" on the album page write version lines instead
        print(f"import-inventory-links: {args.log} is in the album format; link lines would not be read", file=sys.stderr)
        sys.exit(1)

    try:
        with open(args.candidates, newline="") as f:
            rows = list(csv.DictReader(f, delimiter="\t"))
        with open(args.store) as f:
            files = set(json.load(f).get("albums", {}))
    except (OSError, json.JSONDecodeError) as e:
        print(f"import-inventory-links: {e}", file=sys.stderr)
        sys.exit(1)

    # file albums the log has linked or unlinked already (never linked again), and the
    # Tidal album -> release map, both from clutter's collection log
    log = collection_log.read(args.log)
    seen = set(log["touched"])
    tidal_to_release = {"T" + v["T"]: k for k, v in log["streams"].items() if "T" in v}
    in_location: set = set()

    now = datetime.datetime.now().astimezone().isoformat(timespec="seconds")
    out, skipped = [], {"not sure": 0, "several albums": 0, "not in the store": 0, "linked already": 0}
    for r in rows:
        j = r["id"].lower()
        if r["grade"] != "sure":
            skipped["not sure"] += 1
        elif r["flag"]:
            skipped["several albums"] += 1
        elif j not in files:
            skipped["not in the store"] += 1
        elif j in seen or j in in_location:
            skipped["linked already"] += 1
        else:
            seen.add(j)
            out.append(
                {
                    "ts": now,
                    "op": "link",
                    "key": tidal_to_release.get(r["match"], r["match"]),
                    "version": "J" + j,
                    "via": "inventory:sure",
                    "artist": r["match_artist"],
                    "title": r["match_title"],
                }
            )

    kinds: dict = {}
    for e in out:
        kinds[e["key"][0]] = kinds.get(e["key"][0], 0) + 1
    print(f"{len(out)} links ({', '.join(f'{v} to {k} albums' for k, v in sorted(kinds.items()))}); skipped: " + ", ".join(f"{v} {k}" for k, v in skipped.items()))
    if args.dry_run or not out:
        return
    torn = False
    try:
        with open(args.log, "rb") as f:
            f.seek(0, os.SEEK_END)
            if f.tell() > 0:
                f.seek(-1, os.SEEK_END)
                torn = f.read(1) != b"\n"
    except OSError:
        pass
    os.makedirs(os.path.dirname(args.log), exist_ok=True)
    with open(args.log, "a") as f:
        f.write(("\n" if torn else "") + "".join(json.dumps(e, ensure_ascii=False) + "\n" for e in out))
    print(f"appended to {args.log}")


if __name__ == "__main__":
    main()

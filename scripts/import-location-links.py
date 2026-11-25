#!/usr/bin/env python3
"""Bring the ids in the Discogs Location field into clutter's collection log, once.

    scripts/import-location-links.py [--dry-run]

Until 2026-11-25 a release's Tidal, Apple Music and Qobuz album and its file album were
written into the Location field of the Discogs collection ("T12571482 A206349046 J32aa…").
clutter now keeps these links in its own log, ~/Sync/clutter/collection.jsonl
($CLUTTER_COLLECTION), and does not read them from Discogs any more. This script reads
cache/draw*.json and appends one line per id:

    {"ts", "op": "link", "key": "D1365907", "version": "T121047473",
     "via": "discogs-location", "artist", "title"}

Taken: T<digits>, A<digits>, Q<Qobuz id>, the tidal.com / music.apple.com / play.qobuz.com
links, and J<32 hex> where the file album is in the Jellyfin album store. Left out: Apple
library ids (Al.…), a kind of stream the log already has for that release, a file album
the log has linked or unlinked already. Append-only and repeatable. Discogs is not
written to. The server reads the log at start.
"""

import argparse
import datetime
import glob
import json
import os
import re
import sys

import collection_log

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def ids_in(value: str) -> list:
    """[(kind, id)] in a Location value, in order"""
    out = []
    for w in value.split():
        if w.startswith("/"):  # an old file-server path, always the end of the field
            break
        if re.fullmatch(r"T\d+", w):
            out.append(("T", w[1:]))
        elif re.fullmatch(r"A\d+", w):
            out.append(("A", w[1:]))
        elif re.fullmatch(r"J[0-9a-fA-F]{32}", w):
            out.append(("J", w[1:].lower()))
        elif m := re.match(r"https://(?:listen\.)?tidal\.com/(?:browse/)?album/(\d+)", w):
            out.append(("T", m.group(1)))
        elif m := re.match(r"https://music\.apple\.com/[a-z]{2}/album/(?:[^/]+/)?(?:id)?(\d+)", w):
            out.append(("A", m.group(1)))
        elif m := re.match(r"https://play\.qobuz\.com/album/([A-Za-z0-9_-]+)", w):
            out.append(("Q", m.group(1)))
        elif re.fullmatch(r"Q[a-z0-9]{10,}", w):  # a Qobuz album id; not "Q1234", not "Quartets"
            out.append(("Q", w[1:]))
    return out


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--cache", default=os.path.join(ROOT, "cache"))
    ap.add_argument("--store", default=os.path.expanduser(os.environ.get("CLUTTER_JELLYFIN_ALBUMS", "~/Sync/clutter/jellyfin-albums.json")))
    ap.add_argument("--log", default=collection_log.log_path())
    ap.add_argument("--dry-run", action="store_true")
    args = ap.parse_args()

    files_known = set()
    try:
        with open(args.store) as f:
            files_known = set(json.load(f).get("albums", {}))
    except (OSError, json.JSONDecodeError):
        pass
    log = collection_log.read(args.log)
    have = {k: dict(v) for k, v in log["streams"].items()}
    files_seen = set(log["touched"])

    draws = sorted(glob.glob(os.path.join(args.cache, "draw*.json")))
    if not draws:
        print(f"import-location-links: no draw*.json in {args.cache}", file=sys.stderr)
        sys.exit(1)
    now = datetime.datetime.now().astimezone().isoformat(timespec="seconds")
    out, count, skipped, done = [], {}, {"in the log already": 0, "file album not in the store": 0}, set()
    for p in draws:
        with open(p) as f:
            for r in json.load(f).get("releases", []):
                key = f"D{r['id']}"
                if key in done:  # a second copy of the release
                    continue
                done.add(key)
                b = r["basic_information"]
                value = " ".join(n.get("value", "") for n in r.get("notes") or [] if n.get("field_id") == 4)
                for kind, vid in ids_in(value):
                    if kind == "J":
                        if vid in files_seen:
                            skipped["in the log already"] += 1
                            continue
                        if vid not in files_known:
                            skipped["file album not in the store"] += 1
                            continue
                        files_seen.add(vid)
                    else:
                        if kind in have.get(key, {}):
                            skipped["in the log already"] += 1
                            continue
                        have.setdefault(key, {})[kind] = vid
                    count[kind] = count.get(kind, 0) + 1
                    out.append(
                        {
                            "ts": now,
                            "op": "link",
                            "key": key,
                            "version": kind + vid,
                            "via": "discogs-location",
                            "artist": ", ".join(re.sub(r" \(\d+\)$", "", a["name"]) for a in b.get("artists", [])),
                            "title": b.get("title", ""),
                        }
                    )

    names = {"T": "Tidal", "A": "Apple Music", "Q": "Qobuz", "J": "file albums"}
    print(f"{len(out)} links from {len(done)} releases: " + ", ".join(f"{count.get(k, 0)} {names[k]}" for k in "TAQJ") + "; skipped: " + ", ".join(f"{v} {k}" for k, v in skipped.items()))
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

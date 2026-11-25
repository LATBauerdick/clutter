#!/usr/bin/env python3
#
# Playing times for the Discogs releases, for the length.* tags and the album page's Length row.
#
# The server knows the length of every Tidal album in the album store (tidal-albums.json), and
# so of a Discogs release whose Tidal album (Location field, T<id>) is in that store. This
# script looks up the rest and keeps them in ~/Sync/clutter/album-lengths.json
# (CLUTTER_ALBUM_LENGTHS overrides), by Discogs release id:
#
#   - a release that names a Tidal album which is not in the store: that album's duration and
#     track count from Tidal (public web token, no login);
#   - a release without a Tidal album, or whose Tidal album is gone: the sum of the track
#     durations of the Discogs release, if Discogs has one for every track. Many vinyl entries
#     have none: those are stored with "secs": null so they are not asked for again.
#
# Only releases not yet in the store are asked for; --recheck asks again for those without a
# length. Discogs allows 60 calls a minute, so a first fill of ~1,200 releases takes ~25
# minutes; the store is written every 50 releases, and a second run picks up where it stopped.
# The server reads the store at start and on "Update from Tidal".
#
#   scripts/update-lengths.py [--dry-run] [--limit N] [--recheck] [--tidal-only | --discogs-only]
#
import collection_log
import argparse
import datetime as dt
import glob
import json
import os
import re
import sys
import time
import urllib.error
import urllib.request

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HOME = os.path.expanduser("~")
STORE = os.environ.get("CLUTTER_ALBUM_LENGTHS") or os.path.join(HOME, "Sync", "clutter", "album-lengths.json")
TIDAL_STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
CACHE = os.path.join(HERE, "cache")
# Tidal's public web-client token: enough to read an album record
WEB_TOKEN = "CzET4vdadNUFQ5JU"


def read_collection():
    coll = {}
    for fn in glob.glob(os.path.join(CACHE, "draw*.json")):
        try:
            for r in json.load(open(fn))["releases"]:
                coll[r["id"]] = r
        except (OSError, ValueError, KeyError):
            pass
    return coll


_TIDAL_OF = None


def tidal_of(release):
    """the Tidal album of the release, from clutter's collection log (until 2026-11-25:
    from the Location field in Discogs), if any"""
    global _TIDAL_OF
    if _TIDAL_OF is None:
        _TIDAL_OF = collection_log.tidal_of_release(collection_log.read())
    return _TIDAL_OF.get(release.get("id"))


def tidal_store_lengths():
    """Tidal album ids whose length the server already has from the album store"""
    try:
        albums = json.load(open(TIDAL_STORE)).get("albums", {})
    except (OSError, ValueError):
        return set()
    return {int(k) for k, e in albums.items()
            if k.isdigit() and ((e.get("album") or {}).get("duration") or 0) > 0}


def discogs_token():
    """the Discogs token from tok.json; never printed"""
    try:
        return json.load(open(os.path.join(HERE, "tok.json"))).get("tok") or None
    except (OSError, ValueError):
        return None


def fetch_tidal(tid):
    """(secs, tracks), or "gone" (404), or None on a failure worth retrying later"""
    req = urllib.request.Request(f"https://api.tidal.com/v1/albums/{tid}?countryCode=US",
                                 headers={"x-tidal-token": WEB_TOKEN, "User-Agent": "clutter"})
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            a = json.load(r)
    except urllib.error.HTTPError as e:
        return "gone" if e.code == 404 else None
    except (urllib.error.URLError, TimeoutError, ValueError):
        return None
    secs = a.get("duration")
    return (secs if isinstance(secs, int) and secs > 0 else None, a.get("numberOfTracks"))


def seconds(text):
    """"3:44" or "1:02:03" -> seconds; anything else -> None"""
    parts = (text or "").strip().split(":")
    if not 2 <= len(parts) <= 3:
        return None
    try:
        nums = [int(p) for p in parts]
    except ValueError:
        return None
    total = 0
    for n in nums:
        total = total * 60 + n
    return total or None


def track_durations(tracklist):
    """one duration (or None) per track; an index entry counts by its own duration, else by its sub-tracks"""
    out = []
    for e in tracklist or []:
        typ = e.get("type_", "track")
        if typ == "heading":
            continue
        own = seconds(e.get("duration"))
        subs = e.get("sub_tracks") or []
        if typ == "index" and own is None and subs:
            out.extend(track_durations(subs))
        else:
            out.append(own)
    return out


def fetch_discogs(rid, token):
    """(secs or None, tracks), or None on a failure worth retrying later; waits out the rate limit"""
    headers = {"User-Agent": "clutter/0.1"}
    if token:
        headers["Authorization"] = "Discogs token=" + token
    for attempt in range(3):
        req = urllib.request.Request(f"https://api.discogs.com/releases/{rid}", headers=headers)
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                remaining = r.headers.get("X-Discogs-Ratelimit-Remaining")
                rel = json.load(r)
        except urllib.error.HTTPError as e:
            if e.code == 429:
                time.sleep(30 * (attempt + 1))
                continue
            if e.code == 404:
                return (None, None)
            return None
        except (urllib.error.URLError, TimeoutError, ValueError):
            return None
        # stay under 60 a minute; slow down further when the window is nearly used up
        time.sleep(3.0 if remaining is not None and remaining.isdigit() and int(remaining) < 5 else 1.05)
        ds = track_durations(rel.get("tracklist"))
        if ds and all(d is not None for d in ds):
            return (sum(ds), len(ds))
        return (None, len(ds) or None)
    return None


def read_store():
    try:
        s = json.load(open(STORE))
        if isinstance(s.get("releases"), dict):
            return s
    except (OSError, ValueError):
        pass
    return {"releases": {}}


def write_store(store):
    os.makedirs(os.path.dirname(STORE), exist_ok=True)
    tmp = STORE + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(store, f, ensure_ascii=False, indent=0, sort_keys=True)
    os.replace(tmp, STORE)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="report what would be asked, write nothing")
    ap.add_argument("--limit", type=int, default=0, help="ask for at most N releases")
    ap.add_argument("--recheck", action="store_true", help="ask again for releases stored without a length")
    g = ap.add_mutually_exclusive_group()
    g.add_argument("--tidal-only", action="store_true", help="only releases that name a Tidal album")
    g.add_argument("--discogs-only", action="store_true", help="only releases without a Tidal album")
    args = ap.parse_args()

    coll = read_collection()
    if not coll:
        sys.exit("no Discogs collection in cache/draw*.json -- run ./update-json first")
    known_tidal = tidal_store_lengths()
    store = read_store()
    rel = store["releases"]
    today = dt.date.today().isoformat()

    todo = []  # (release id, Tidal id or None)
    covered = 0
    for rid, r in sorted(coll.items()):
        tid = tidal_of(r)
        if tid in known_tidal:
            covered += 1  # the server has it from the Tidal album store
            continue
        old = rel.get(str(rid))
        if old and (old.get("secs") or not args.recheck):
            continue
        if (args.tidal_only and tid is None) or (args.discogs_only and tid is not None):
            continue
        todo.append((rid, tid))
    if args.limit:
        todo = todo[:args.limit]

    print(f"{len(coll)} releases: {covered} have a length from the Tidal album store, "
          f"{sum(1 for e in rel.values() if e.get('secs'))} from this store, "
          f"{sum(1 for e in rel.values() if not e.get('secs'))} stored without one; asking for {len(todo)} "
          f"({sum(1 for _, t in todo if t)} via Tidal)")
    if args.dry_run or not todo:
        return

    token = discogs_token()
    got = failed = 0
    for n, (rid, tid) in enumerate(todo, 1):
        entry = None
        if tid is not None:
            t = fetch_tidal(tid)
            time.sleep(0.25)
            if t is None:
                failed += 1
                continue
            if t != "gone" and t[0]:
                entry = {"secs": t[0], "tracks": t[1], "source": f"tidal:{tid}", "checked": today}
        if entry is None:
            d = fetch_discogs(rid, token)
            if d is None:
                failed += 1
                continue
            entry = {"secs": d[0], "tracks": d[1], "source": "discogs", "checked": today}
        rel[str(rid)] = entry
        got += 1 if entry["secs"] else 0
        if n % 50 == 0:
            write_store(store)
            print(f"  {n}/{len(todo)}: {got} with a length, {failed} failed", flush=True)
    write_store(store)
    print(f"done: asked {len(todo)}, {got} with a length, {len(todo) - got - failed} without, {failed} failed "
          f"(a failed one is asked again next time)")


if __name__ == "__main__":
    main()

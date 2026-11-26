#!/usr/bin/env python3
"""Fill clutter's Jellyfin album store from Jellyfin's API (read-only there).

    scripts/update-jellyfin-albums.py [--full] [--url http://host:8096] [--web URL]

Without --full, and once a full run has been made: only the albums Jellyfin has
added since the last run (seconds). Albums that left, moved or were retagged
need --full (minutes: Jellyfin takes about 40 ms per album); the quick run
notices by Jellyfin's album count that one is due and says so ("stale" in the
store).

Writes ~/Sync/clutter/jellyfin-albums.json ($CLUTTER_JELLYFIN_ALBUMS), a
snapshot that can always be fetched again:

    {"updated": "...", "api": URL, "web": URL,
     "libraries": [{"name", "locations"}],
     "albums": {"<32-hex id>": {title, artist, artists, year, added, secs,
                                path, library, storage, image, mbid}}}

The server reads it at start (src/Jellyfin.hs): every album becomes a J<id>
album with the tags provider.jellyfin and storage.<storage>.

URLs: --url is where this script reaches the API, --web what a browser opens
(covers, "Open in Jellyfin"); each defaults to the other, then to the value in
the store from the last run, then to $CLUTTER_JELLYFIN. The API key comes from
$JELLYFIN_API_KEY, else "jky" in tok.json.

Storage locations: ~/Sync/clutter/storage-map.json ($CLUTTER_STORAGE_MAP), a
list of {"prefix": "/media/music/Music/_Classical", "name": "classical"}. The
longest prefix that holds an album's folder names its storage location; with
no match it is the Jellyfin library's name. Albums outside every library
folder (leftovers of an old path) are left out.
"""

import argparse
import datetime
import json
import os
import re
import sys
import urllib.error
import urllib.parse
import urllib.request

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PAGE = 500
QUICK_PAGE = 50


def fail(msg: str) -> None:
    print(f"update-jellyfin-albums: {msg}", file=sys.stderr)
    sys.exit(1)


def api(url: str, key: str, path: str, params: dict | None = None):
    q = "?" + urllib.parse.urlencode(params) if params else ""
    req = urllib.request.Request(
        url.rstrip("/") + path + q,
        headers={"Authorization": f'MediaBrowser Token="{key}"', "Accept": "application/json"},
    )
    try:
        with urllib.request.urlopen(req, timeout=120) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        fail(f"{path}: HTTP {e.code}" + (" (API key refused)" if e.code == 401 else ""))
    except urllib.error.URLError as e:
        fail(f"{path}: {e.reason}")


def slug(name: str) -> str:
    return re.sub(r"[^a-z0-9]+", "-", name.lower()).strip("-") or "unknown"


def under(path: str, prefix: str) -> bool:
    prefix = prefix.rstrip("/")
    return path == prefix or path.startswith(prefix + "/")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--url", default="", help="Jellyfin as this script reaches it")
    ap.add_argument("--web", default="", help="Jellyfin as a browser reaches it")
    ap.add_argument("--tok", default=os.path.join(ROOT, "tok.json"))
    ap.add_argument("--store", default=os.path.expanduser(os.environ.get("CLUTTER_JELLYFIN_ALBUMS", "~/Sync/clutter/jellyfin-albums.json")))
    ap.add_argument("--full", action="store_true", help="ask for every album (minutes), not only the new ones")
    ap.add_argument("--map", default=os.path.expanduser(os.environ.get("CLUTTER_STORAGE_MAP", "~/Sync/clutter/storage-map.json")))
    args = ap.parse_args()

    old = {}
    try:
        with open(args.store) as f:
            old = json.load(f)
    except (OSError, json.JSONDecodeError):
        pass
    env_url = os.environ.get("CLUTTER_JELLYFIN", "")
    url = args.url or old.get("api") or args.web or old.get("web") or env_url
    web = args.web or old.get("web") or args.url or env_url or url
    if not url:
        fail("no Jellyfin URL: --url, or $CLUTTER_JELLYFIN")

    key = os.environ.get("JELLYFIN_API_KEY", "")
    if not key:
        try:
            with open(args.tok) as f:
                key = json.load(f).get("jky", "")
        except (OSError, json.JSONDecodeError):
            pass
    if not key:
        fail('no API key: set $JELLYFIN_API_KEY or "jky" in tok.json')

    storage_map = []
    try:
        with open(args.map) as f:
            storage_map = [(m["prefix"], m["name"]) for m in json.load(f)]
    except OSError:
        pass
    except (json.JSONDecodeError, KeyError, TypeError) as e:
        fail(f"{args.map}: {e}")

    libraries = [
        {"name": lib.get("Name", ""), "locations": lib.get("Locations", [])}
        for lib in api(url, key, "/Library/VirtualFolders")
        if lib.get("CollectionType") == "music"
    ]
    query = {
        "IncludeItemTypes": "MusicAlbum",
        "Recursive": "true",
        "Fields": "Path,ProviderIds,DateCreated",
        "EnableUserData": "false",
    }
    known = old.get("albums", {})
    # Jellyfin (10.11) takes about 40 ms per album it lists, whatever is asked for: all
    # 9,000 are six minutes and more. So, unless --full: only the albums added since the
    # last run -- newest first, until a page holds nothing new -- and Jellyfin's album
    # count as the check that nothing else changed.
    quick = not args.full and bool(known) and isinstance(old.get("listed"), int)
    stale = ""
    if quick:
        fresh, start, total = [], 0, 0
        seen = set(known) | set(old.get("outside_ids", []))
        while True:
            page = api(url, key, "/Items", {**query, "SortBy": "DateCreated", "SortOrder": "Descending", "StartIndex": start, "Limit": QUICK_PAGE})
            items = page.get("Items", [])
            total = page.get("TotalRecordCount", 0)
            unseen = [a for a in items if a.get("Id", "").lower() not in seen]
            fresh.extend(unseen)
            seen.update(a.get("Id", "").lower() for a in unseen)
            start += len(items)
            if not unseen or not items or start >= total:
                break
        raw = fresh
        expected = old["listed"] + len(fresh)
        if total != expected:
            # albums left, or came with an old date (a folder moved back in): only the
            # whole list tells which
            stale = f"Jellyfin lists {total} albums, {expected} were expected: run a full update"
    else:
        # Pages are cut from a list Jellyfin sorts anew for each request: while it scans, an
        # album can move between pages and be skipped (ten were on 2026-11-25, and reported
        # as gone). So count the ids, and go through the pages again where some are missing.
        # (The whole list in one request is no way out: it takes longer than any timeout.)
        byid, total = {}, 0
        for attempt in range(3):
            start = 0
            while True:
                page = api(url, key, "/Items", {**query, "SortBy": "SortName", "StartIndex": start, "Limit": PAGE})
                items = page.get("Items", [])
                total = page.get("TotalRecordCount", 0)
                byid.update((a.get("Id", "").lower(), a) for a in items)
                start += len(items)
                if not items or start >= total:
                    break
            if len(byid) >= total:
                break
            print(f"update-jellyfin-albums: the pages gave {len(byid)} of {total} albums, going through them again", file=sys.stderr)
        raw = list(byid.values())
        if len(byid) < total:
            # still short: add what was found, but call nothing gone
            stale = f"Jellyfin lists {total} albums but gave {len(byid)}: nothing was taken off, run a full update again"

    albums, outside, outside_now = {}, 0, set()
    for a in raw:
        path = a.get("Path") or ""
        lib = max(
            ((l["name"], loc) for l in libraries for loc in l["locations"] if under(path, loc)),
            key=lambda x: len(x[1]),
            default=None,
        )
        if lib is None:
            outside += 1
            outside_now.add(a["Id"].lower())
            continue
        named = max(((p, n) for p, n in storage_map if under(path, p)), key=lambda x: len(x[0]), default=None)
        artists = [x.get("Name", "") for x in a.get("AlbumArtists") or [] if x.get("Name")]
        albums[a["Id"].lower()] = {
            "title": a.get("Name", ""),
            "artist": a.get("AlbumArtist") or ", ".join(artists),
            "artists": artists,
            "year": a.get("ProductionYear") or 0,
            "added": (a.get("DateCreated") or "")[:19],
            "secs": (a.get("RunTimeTicks") or 0) // 10_000_000,
            "path": path,
            "library": lib[0],
            "storage": slug(named[1] if named else lib[0]),
            "image": bool((a.get("ImageTags") or {}).get("Primary")),
            "mbid": (a.get("ProviderIds") or {}).get("MusicBrainzAlbum", ""),
        }

    outside_ids = sorted(outside_now)
    if quick or stale:
        # only additions: what the store had stays
        albums = {**known, **albums}
        outside_ids = sorted(set(old.get("outside_ids", [])) | outside_now)
    store = {
        "updated": datetime.datetime.now().astimezone().isoformat(timespec="seconds"),
        "api": url,
        "web": web,
        "libraries": libraries,
        # how many albums Jellyfin listed when the store last agreed with it, and those of
        # them outside every library: the quick update's check and its "seen before"
        "listed": total if not stale else (old["listed"] + len(raw) if quick else old.get("listed")),
        "outside_ids": outside_ids,
        "albums": albums,
    }
    if stale:
        store["stale"] = stale
    os.makedirs(os.path.dirname(args.store), exist_ok=True)
    with open(args.store + ".tmp", "w") as f:
        json.dump(store, f, indent=1, ensure_ascii=False, sort_keys=True)
    os.replace(args.store + ".tmp", args.store)

    count: dict = {}
    for a in albums.values():
        count[a["storage"]] = count.get(a["storage"], 0) + 1
    new = len(set(albums) - set(old.get("albums", {})))
    gone = len(set(old.get("albums", {})) - set(albums))
    print(f"{len(albums)} albums ({new} new, {gone} gone, {outside} outside every library left out{', quick' if quick else ''}) -> {args.store}")
    if stale:
        print("update-jellyfin-albums: " + stale, file=sys.stderr)
    print("storage: " + ", ".join(f"{k} {v}" for k, v in sorted(count.items(), key=lambda x: -x[1])))


if __name__ == "__main__":
    main()

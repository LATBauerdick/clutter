#!/usr/bin/env python3
"""Fill clutter's Jellyfin album store from Jellyfin's API (read-only there).

    scripts/update-jellyfin-albums.py [--url http://host:8096] [--web URL]

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
    raw, start = [], 0
    while True:
        page = api(
            url,
            key,
            "/Items",
            {
                "IncludeItemTypes": "MusicAlbum",
                "Recursive": "true",
                "Fields": "Path,ProviderIds,DateCreated",
                "SortBy": "SortName",
                "StartIndex": start,
                "Limit": PAGE,
            },
        )
        items = page.get("Items", [])
        raw.extend(items)
        start += len(items)
        if not items or start >= page.get("TotalRecordCount", 0):
            break

    albums, outside = {}, 0
    for a in raw:
        path = a.get("Path") or ""
        lib = max(
            ((l["name"], loc) for l in libraries for loc in l["locations"] if under(path, loc)),
            key=lambda x: len(x[1]),
            default=None,
        )
        if lib is None:
            outside += 1
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

    store = {
        "updated": datetime.datetime.now().astimezone().isoformat(timespec="seconds"),
        "api": url,
        "web": web,
        "libraries": libraries,
        "albums": albums,
    }
    os.makedirs(os.path.dirname(args.store), exist_ok=True)
    with open(args.store + ".tmp", "w") as f:
        json.dump(store, f, indent=1, ensure_ascii=False, sort_keys=True)
    os.replace(args.store + ".tmp", args.store)

    count: dict = {}
    for a in albums.values():
        count[a["storage"]] = count.get(a["storage"], 0) + 1
    new = len(set(albums) - set(old.get("albums", {})))
    gone = len(set(old.get("albums", {})) - set(albums))
    print(f"{len(albums)} albums ({new} new, {gone} gone, {outside} outside every library left out) -> {args.store}")
    print("storage: " + ", ".join(f"{k} {v}" for k, v in sorted(count.items(), key=lambda x: -x[1])))


if __name__ == "__main__":
    main()

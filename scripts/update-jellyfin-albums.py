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

Moved folders: Jellyfin gives a folder that was moved or renamed a new id. A
full run looks for every album that has gone and that clutter refers to (a link,
keep mark or skip in ~/Sync/clutter/collection.jsonl, a play in
~/Sync/clutter/listened/) among the albums that came since the last full run
and that clutter does not refer to yet: the same title and artist (narrowed by
folder name, then playing time, where several fit), else the same folder name
and playing time. Where exactly one fits, and no other gone album wants it, a
"moved" line goes to the collection log and the server reads the old id as the
new one (src/Collection.hs). --no-moves leaves the log alone.
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

import collection_log

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


def norm(s: str) -> str:
    return " ".join((s or "").casefold().split())


def folder(path: str) -> str:
    return norm(os.path.basename((path or "").rstrip("/")))


def play_refs() -> set:
    """the file albums the play log names (Jellyfin ids, lower case)"""
    d = os.path.expanduser(os.environ.get("CLUTTER_LISTENED", "~/Sync/clutter/listened"))
    refs = set()
    try:
        names = [n for n in os.listdir(d) if n.endswith(".jsonl")]
    except OSError:
        return refs
    for n in names:
        try:
            with open(os.path.join(d, n)) as f:
                for line in f:
                    try:
                        j = json.loads(line).get("jellyfin")
                    except (json.JSONDecodeError, AttributeError):
                        continue
                    if isinstance(j, str) and j:
                        refs.add(j.lower())
        except OSError:
            continue
    return refs


def same_secs(a: dict, b: dict) -> bool:
    x, y = a.get("secs") or 0, b.get("secs") or 0
    return x == 0 or y == 0 or abs(x - y) <= 2


def find_moves(gone: dict, arrived: dict) -> tuple[dict, list]:
    """gone: old id -> album; arrived: candidate id -> album. Returns ({old: new}, [old ids
    not found again]). One candidate per gone album, or none."""
    by_name: dict = {}
    by_folder: dict = {}
    for i, a in arrived.items():
        if norm(a.get("title")):
            by_name.setdefault((norm(a.get("title")), norm(a.get("artist"))), []).append(i)
        if folder(a.get("path")) and (a.get("secs") or 0) > 0:
            by_folder.setdefault((folder(a.get("path")), a["secs"]), []).append(i)
    found: dict = {}
    for j, a in gone.items():
        fits = by_name.get((norm(a.get("title")), norm(a.get("artist"))), []) if norm(a.get("title")) else []
        if len(fits) > 1:
            fits = [i for i in fits if folder(arrived[i].get("path")) == folder(a.get("path"))] or fits
        fits = [i for i in fits if same_secs(arrived[i], a)]
        if len(fits) > 1:
            fits = [i for i in fits if (arrived[i].get("secs") or 0) == (a.get("secs") or 0)]
        if not fits and (a.get("secs") or 0) > 0:
            fits = by_folder.get((folder(a.get("path")), a["secs"]), [])
        if len(fits) == 1:
            found[j] = fits[0]
    # a new album that two gone ones want is neither's
    wanted: dict = {}
    for n in found.values():
        wanted[n] = wanted.get(n, 0) + 1
    found = {j: n for j, n in found.items() if wanted[n] == 1}
    return found, [j for j in gone if j not in found]


def append_moves(path: str, found: dict, gone: dict, arrived: dict) -> None:
    os.makedirs(os.path.dirname(path), exist_ok=True)
    torn = False
    try:
        with open(path, "rb") as f:
            f.seek(0, os.SEEK_END)
            if f.tell() > 0:
                f.seek(-1, os.SEEK_END)
                torn = f.read(1) != b"\n"
    except OSError:
        pass
    ts = datetime.datetime.now().astimezone().isoformat(timespec="seconds")
    with open(path, "a") as f:
        if torn:
            f.write("\n")
        for j, n in found.items():
            a, b = gone[j], arrived[n]
            f.write(json.dumps({
                "ts": ts, "op": "moved", "from": "J" + j, "to": "J" + n, "via": "update-jellyfin-albums",
                "artist": b.get("artist", ""), "title": b.get("title", ""),
                "from_path": a.get("path", ""), "to_path": b.get("path", ""),
            }, ensure_ascii=False) + "\n")


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--url", default="", help="Jellyfin as this script reaches it")
    ap.add_argument("--web", default="", help="Jellyfin as a browser reaches it")
    ap.add_argument("--tok", default=os.path.join(ROOT, "tok.json"))
    ap.add_argument("--store", default=os.path.expanduser(os.environ.get("CLUTTER_JELLYFIN_ALBUMS", "~/Sync/clutter/jellyfin-albums.json")))
    ap.add_argument("--full", action="store_true", help="ask for every album (minutes), not only the new ones")
    ap.add_argument("--map", default=os.path.expanduser(os.environ.get("CLUTTER_STORAGE_MAP", "~/Sync/clutter/storage-map.json")))
    ap.add_argument("--log", default=collection_log.log_path(), help="clutter's collection log, for the moved folders")
    ap.add_argument("--no-moves", action="store_true", help="do not look for moved folders")
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
    # the albums that came since the last full run: where a moved folder is looked for
    since_full = sorted(set(albums) - set(known))
    if quick or stale:
        # only additions: what the store had stays
        since_full = sorted(set(old.get("since_full", [])) | set(since_full))
        albums = {**known, **albums}
        outside_ids = sorted(set(old.get("outside_ids", [])) | outside_now)
    found: dict = {}
    lost: list = []
    if not (quick or stale) and not args.no_moves:
        mv = collection_log.moves(args.log)
        refs = collection_log.file_refs(args.log) | {collection_log.follow(mv, j) for j in play_refs()}
        gone_refs = {j: a for j, a in known.items() if j not in albums and j in refs}
        if gone_refs:
            came = (set(albums) - set(known)) | set(old.get("since_full", []))
            arrived = {i: albums[i] for i in came if i in albums and i not in refs}
            found, lost = find_moves(gone_refs, arrived)
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
    if quick or stale:
        store["since_full"] = since_full
    if stale:
        store["stale"] = stale
    os.makedirs(os.path.dirname(args.store), exist_ok=True)
    with open(args.store + ".tmp", "w") as f:
        json.dump(store, f, indent=1, ensure_ascii=False, sort_keys=True)
    os.replace(args.store + ".tmp", args.store)
    # after the store: a move line names albums the store has
    if found:
        append_moves(args.log, found, gone_refs, arrived)

    count: dict = {}
    for a in albums.values():
        count[a["storage"]] = count.get(a["storage"], 0) + 1
    new = len(set(albums) - set(old.get("albums", {})))
    gone = len(set(old.get("albums", {})) - set(albums))
    print(f"{len(albums)} albums ({new} new, {gone} gone, {outside} outside every library left out{', quick' if quick else ''}) -> {args.store}")
    if found or lost:
        print(f"moved: {len(found)} found again, {len(lost)} that clutter refers to not found" + (f" -> {args.log}" if found else ""))
        for j in lost:
            a = known[j]
            print(f"  not found: J{j} {a.get('artist', '')} – {a.get('title', '')} ({a.get('path', '')})")
    if stale:
        print("update-jellyfin-albums: " + stale, file=sys.stderr)
    print("storage: " + ", ".join(f"{k} {v}" for k, v in sorted(count.items(), key=lambda x: -x[1])))


if __name__ == "__main__":
    main()

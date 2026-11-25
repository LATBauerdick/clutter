#!/usr/bin/env python3
"""Inventory: Jellyfin's music albums against clutter (read-only).

Reads Jellyfin's libraries and album list through its API, the Discogs
collection from cache/draw*.json, the Tidal album store and the play log, and
says for every Jellyfin album where it stands:

  linked     a Discogs release already names it (J<id> in the Location field)
  physical   matches a Discogs release on a shelf
  standin    matches a Discogs release in a stand-in folder (Streaming, Files)
  tidal      matches a Tidal album in the store
  unclaimed  nowhere in clutter

Nothing is written to Jellyfin, Discogs or clutter's stores. Output goes to
--out: the two API snapshots, albums.tsv (every album with its class),
link-candidates.tsv, broken-j.tsv and summary.md. See docs/jellyfin-plan.md.

    scripts/jellyfin-inventory.py --url http://usrv.local:8096
    scripts/jellyfin-inventory.py --offline        # reuse the snapshots

The API key comes from $JELLYFIN_API_KEY, else "jky" in tok.json; the URL from
--url, else $JELLYFIN_URL.
"""

import argparse

import collection_log
import csv
import glob
import json
import os
import re
import sys
import unicodedata
import urllib.error
import urllib.parse
import urllib.request
from collections import Counter, defaultdict

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
STANDIN_FOLDERS = {"Streaming", "Files"}
PAGE = 500


def fail(msg: str) -> None:
    print(f"jellyfin-inventory: {msg}", file=sys.stderr)
    sys.exit(1)


# ---------------------------------------------------------------- Jellyfin


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


def fetch_jellyfin(url: str, key: str) -> tuple[list, list]:
    libraries = api(url, key, "/Library/VirtualFolders")
    albums = []
    start = 0
    while True:
        page = api(
            url,
            key,
            "/Items",
            {
                "IncludeItemTypes": "MusicAlbum",
                "Recursive": "true",
                "Fields": "Path,ProviderIds,ChildCount,DateCreated,Genres",
                "SortBy": "SortName",
                "StartIndex": start,
                "Limit": PAGE,
            },
        )
        items = page.get("Items", [])
        albums.extend(items)
        start += len(items)
        print(f"  albums {start}/{page.get('TotalRecordCount', '?')}", file=sys.stderr)
        if not items or start >= page.get("TotalRecordCount", 0):
            break
    return libraries, albums


def library_of(path: str, libraries: list) -> str:
    best = ("", "")
    for lib in libraries:
        for loc in lib.get("Locations", []):
            if path and (path == loc or path.startswith(loc.rstrip("/") + "/")) and len(loc) > len(best[1]):
                best = (lib.get("Name", ""), loc)
    return best[0]


# ------------------------------------------------------------------ clutter


# release key -> its file albums, from clutter's collection log (until 2026-11-25: J ids in
# the Discogs Location field)
FILES_OF: dict = {}
for _jid, _key in collection_log.read()["files"].items():
    FILES_OF.setdefault(_key, []).append(_jid)


def load_discogs(cache: str) -> tuple[list, dict]:
    folders = {}
    try:
        with open(os.path.join(cache, "folders-raw.json")) as f:
            folders = {x["id"]: x["name"] for x in json.load(f)["folders"]}
    except (OSError, KeyError, json.JSONDecodeError):
        pass
    releases = []
    for p in sorted(glob.glob(os.path.join(cache, "draw*.json"))):
        with open(p) as f:
            for r in json.load(f).get("releases", []):
                b = r["basic_information"]
                loc = " ".join(n["value"] for n in r.get("notes") or [] if n.get("field_id") == 4)
                releases.append(
                    {
                        "key": f"D{r['id']}",
                        "artist": ", ".join(re.sub(r" \(\d+\)$", "", a["name"]) for a in b.get("artists", [])),
                        "title": b.get("title", ""),
                        "year": b.get("year") or 0,
                        "format": "+".join(dict.fromkeys(x["name"] for x in b.get("formats", []))),
                        "folder": folders.get(r.get("folder_id"), str(r.get("folder_id"))),
                        # the file albums linked to the release, from clutter's collection log
                        "jids": FILES_OF.get(f"D{r['id']}", []),
                    }
                )
    return releases, folders


def load_tidal(path: str) -> list:
    try:
        with open(path) as f:
            store = json.load(f).get("albums", {})
    except (OSError, json.JSONDecodeError):
        return []
    out = []
    for tid, e in store.items():
        a = e.get("album") or {}
        if e.get("dropped") or not a:
            continue
        out.append(
            {
                "key": f"T{tid}",
                "artist": ", ".join(x.get("name", "") for x in a.get("artists") or []),
                "title": a.get("title", ""),
                "year": int((a.get("releaseDate") or "0")[:4] or 0),
                "format": "Tidal",
                "folder": "",
            }
        )
    return out


def load_played(listened: str) -> set:
    played = set()
    for p in glob.glob(os.path.join(listened, "*.jsonl")):
        with open(p) as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                try:
                    e = json.loads(line)
                except json.JSONDecodeError:
                    continue
                for field, prefix in (("discogs", "D"), ("tidal", "T")):
                    if e.get(field):
                        played.add(f"{prefix}{e[field]}")
    return played


# ----------------------------------------------------------------- matching


def norm(s: str) -> str:
    s = unicodedata.normalize("NFKD", s or "")
    s = "".join(c for c in s if not unicodedata.combining(c)).lower()
    s = re.sub(r"[\(\[][^\)\]]*[\)\]]", " ", s)  # (Remastered), [Deluxe Edition]
    s = s.replace("&", " and ")
    s = re.sub(r"[^a-z0-9]+", " ", s)
    s = re.sub(r"^(the|a|an|der|die|das|le|la|les) ", "", s.strip())
    return " ".join(s.split())


def tokens(s: str) -> set:
    return {t for t in norm(s).split() if len(t) > 2 and t not in {"and", "the", "von", "van", "der"}}


def grade(j: dict, c: dict) -> str:
    """How well a clutter album c fits the Jellyfin album j; '' = not at all."""
    if norm(j["title"]) != norm(c["title"]) or not norm(j["title"]):
        return ""
    ja, ca = tokens(j["artist"]), tokens(c["artist"])
    if norm(j["artist"]) == norm(c["artist"]) or (ja and ja == ca):
        return "sure"
    if ja & ca:
        return "likely"
    return "ambiguous"


RANK = {"sure": 0, "likely": 1, "ambiguous": 2}


def best_matches(j: dict, by_title: dict) -> list:
    found = [(grade(j, c), c) for c in by_title.get(norm(j["title"]), [])]
    found = [(g, c) for g, c in found if g]
    if not found:
        return []
    top = min(RANK[g] for g, _ in found)
    return [(g, c) for g, c in found if RANK[g] == top]


# --------------------------------------------------------------------- main


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n\n")[0])
    ap.add_argument("--url", default=os.environ.get("JELLYFIN_URL", ""))
    ap.add_argument("--tok", default=os.path.join(ROOT, "tok.json"))
    ap.add_argument("--cache", default=os.path.join(ROOT, "cache"))
    ap.add_argument("--tidal", default=os.path.expanduser(os.environ.get("CLUTTER_TIDAL_ALBUMS", "~/Sync/clutter/tidal-albums.json")))
    ap.add_argument("--listened", default=os.path.expanduser("~/Sync/clutter/listened"))
    ap.add_argument("--out", default=None, help="default: <cache>/jellyfin-inventory")
    ap.add_argument("--offline", action="store_true", help="reuse the snapshots in --out")
    args = ap.parse_args()
    out = args.out or os.path.join(args.cache, "jellyfin-inventory")
    os.makedirs(out, exist_ok=True)
    snap_l, snap_a = os.path.join(out, "jellyfin-libraries.json"), os.path.join(out, "jellyfin-albums.json")

    if args.offline:
        try:
            with open(snap_l) as f:
                libraries = json.load(f)
            with open(snap_a) as f:
                raw = json.load(f)
        except OSError as e:
            fail(f"--offline: {e}")
    else:
        key = os.environ.get("JELLYFIN_API_KEY", "")
        if not key:
            try:
                with open(args.tok) as f:
                    key = json.load(f).get("jky", "")
            except (OSError, json.JSONDecodeError):
                pass
        if not key:
            fail('no API key: set $JELLYFIN_API_KEY or "jky" in tok.json (Jellyfin > Dashboard > API Keys)')
        if not args.url:
            fail("no Jellyfin URL: --url or $JELLYFIN_URL")
        libraries, raw = fetch_jellyfin(args.url, key)
        for path, data in ((snap_l, libraries), (snap_a, raw)):
            with open(path + ".tmp", "w") as f:
                json.dump(data, f, indent=1, ensure_ascii=False)
            os.replace(path + ".tmp", path)

    jalbums = [
        {
            "id": a["Id"].lower(),
            "artist": a.get("AlbumArtist") or ", ".join(x.get("Name", "") for x in a.get("AlbumArtists") or []),
            "title": a.get("Name", ""),
            "year": a.get("ProductionYear") or 0,
            "tracks": a.get("ChildCount") or 0,
            "mins": round((a.get("RunTimeTicks") or 0) / 600_000_000),
            "path": a.get("Path") or "",
            "library": library_of(a.get("Path") or "", libraries),
            "mbid": (a.get("ProviderIds") or {}).get("MusicBrainzAlbum", ""),
        }
        for a in raw
    ]

    releases, _ = load_discogs(args.cache)
    tidal = load_tidal(args.tidal)
    played = load_played(args.listened)
    if not releases:
        fail(f"no Discogs releases in {args.cache} (run ./update-json)")

    linked = {jid: r for r in releases for jid in r["jids"]}
    by_title_d, by_title_t = defaultdict(list), defaultdict(list)
    for r in releases:
        by_title_d[norm(r["title"])].append(r)
    for t in tidal:
        by_title_t[norm(t["title"])].append(t)

    rows, candidates = [], []
    for j in jalbums:
        cls, grade_, partner = "unclaimed", "", None
        if j["id"] in linked:
            cls, partner = "linked", [linked[j["id"]]]
        else:
            md = best_matches(j, by_title_d)
            mt = best_matches(j, by_title_t)
            # a Discogs match wins unless the Tidal one is clearly better
            if md and (not mt or RANK[md[0][0]] <= RANK[mt[0][0]]):
                grade_, partner = md[0][0], [c for _, c in md]
                shelf = [c for c in partner if c["folder"] not in STANDIN_FOLDERS]
                cls = "physical" if shelf else "standin"
                partner = shelf or partner
            elif mt:
                grade_, partner, cls = mt[0][0], [c for _, c in mt], "tidal"
        # several releases of one album: propose the CD, flag the row
        flag = ""
        if partner and len(partner) > 1:
            flag = f"{len(partner)} matches"
            partner = sorted(partner, key=lambda c: (c["key"] not in played, "CD" not in c["format"]))
        p = partner[0] if partner else None
        row = {
            **j,
            "class": cls,
            "grade": grade_,
            "match": p["key"] if p else "",
            "match_artist": p["artist"] if p else "",
            "match_title": p["title"] if p else "",
            "match_format": p["format"] if p else "",
            "match_folder": p["folder"] if p else "",
            "match_played": "played" if p and p["key"] in played else "",
            "flag": flag,
        }
        rows.append(row)
        if cls in ("physical", "standin", "tidal"):
            candidates.append(row)

    known = {j["id"] for j in jalbums}
    broken = [{"release": r["key"], "artist": r["artist"], "title": r["title"], "jid": jid} for jid, r in linked.items() if jid not in known]

    cols = ["class", "grade", "library", "artist", "title", "year", "tracks", "mins", "id", "match", "match_artist", "match_title", "match_format", "match_folder", "match_played", "flag", "mbid", "path"]

    def write_tsv(name: str, data: list, fields: list) -> None:
        with open(os.path.join(out, name), "w", newline="") as f:
            w = csv.DictWriter(f, fieldnames=fields, delimiter="\t", extrasaction="ignore")
            w.writeheader()
            w.writerows(data)

    order = {"linked": 0, "physical": 1, "standin": 2, "tidal": 3, "unclaimed": 4}
    rows.sort(key=lambda r: (order[r["class"]], RANK.get(r["grade"], 3), r["library"], norm(r["artist"]), norm(r["title"])))
    candidates.sort(key=lambda r: (RANK[r["grade"]], order[r["class"]], norm(r["artist"]), norm(r["title"])))
    write_tsv("albums.tsv", rows, cols)
    write_tsv("link-candidates.tsv", candidates, cols)
    write_tsv("broken-j.tsv", broken, ["release", "artist", "title", "jid"])

    libs = sorted({r["library"] for r in rows})
    classes = ["linked", "physical", "standin", "tidal", "unclaimed"]
    count = Counter((r["library"], r["class"]) for r in rows)
    grades = Counter((r["class"], r["grade"]) for r in candidates)
    lines = ["# Jellyfin inventory", ""]
    lines.append("Libraries: " + "; ".join(f"{l.get('Name')} ({l.get('CollectionType') or 'mixed'}): {', '.join(l.get('Locations', []))}" for l in libraries))
    lines += ["", "| library | " + " | ".join(classes) + " | all |", "|---|" + "---:|" * (len(classes) + 1)]
    for lib in libs:
        n = [count[(lib, c)] for c in classes]
        lines.append(f"| {lib or '(none)'} | " + " | ".join(map(str, n)) + f" | {sum(n)} |")
    n = [sum(count[(lib, c)] for lib in libs) for c in classes]
    lines.append("| **all** | " + " | ".join(map(str, n)) + f" | {sum(n)} |")
    lines += ["", "| match | sure | likely | ambiguous |", "|---|---:|---:|---:|"]
    for c in ("physical", "standin", "tidal"):
        lines.append(f"| {c} | " + " | ".join(str(grades[(c, g)]) for g in ("sure", "likely", "ambiguous")) + " |")
    lines += [
        "",
        f"clutter side: {len(releases)} Discogs releases ({sum(1 for r in releases if r['jids'])} with a J id), {len(tidal)} Tidal albums, {len(played)} played keys.",
        f"Existing J ids that Jellyfin no longer knows: {len(broken)} (broken-j.tsv).",
        f"Albums without a folder path: {sum(1 for r in rows if not r['path'])}; without a MusicBrainz id: {sum(1 for r in rows if not r['mbid'])}.",
    ]
    with open(os.path.join(out, "summary.md"), "w") as f:
        f.write("\n".join(lines) + "\n")
    print("\n".join(lines))
    print(f"\nwritten to {out}", file=sys.stderr)


if __name__ == "__main__":
    main()

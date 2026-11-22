#!/usr/bin/env python3
#
# Phases 1 and 2 of docs/wikidata-plan.md: Wikipedia and AllMusic links via Wikidata.
#
# Keeps ~/Sync/clutter/wikidata.json (CLUTTER_WIKIDATA overrides): for each Discogs master
# and artist of the collection (cache/draw*.json), and each album, its Apple Music album and
# its artists in the Tidal album store (~/Sync/clutter/tidal-albums.json), the Wikidata item, its
# English and German Wikipedia pages and its AllMusic id. A key that Wikidata does not know
# is kept as {checked} only. A regular run asks only new keys; rechecks are by hand
# (LATB, 2026-11-21), and every run reminds once misses are REMIND_DAYS old.
#
# Sends only public Discogs, Tidal and Apple ids to query.wikidata.org, 200 per query. Honours 429 with
# Retry-After (WDQS sometimes limits to 1 query a minute), and saves after every batch, so an
# interrupted run keeps what it got.
#
#   scripts/update-wikidata.py [--dry-run] [--limit N] [--recheck[=all]]
#
# --dry-run:     only count what is due, no network, write nothing
# --limit N:     ask at most N keys this run (for testing)
# --recheck:     also ask again every key Wikidata did not know (misses)
# --recheck=all: ask again every key, found or not (picks up new Wikipedia pages)
#
import argparse
import datetime as dt
import glob
import json
import os
import sys
import time
import urllib.error
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HOME = os.path.expanduser("~")
STORE = os.environ.get("CLUTTER_WIKIDATA") or os.path.join(HOME, "Sync", "clutter", "wikidata.json")
CACHE = os.path.join(HERE, "cache")
TIDAL_STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
SPARQL = "https://query.wikidata.org/sparql"
UA = "clutter/0.1 (personal music catalogue; stdlib urllib)"
BATCH = 200
REMIND_DAYS = 180
# Discogs placeholder artists: Various, Unknown Artist, No Artist
SKIP_ARTISTS = {0, 194, 355, 118760}

# key prefix -> (Wikidata property of the id, property of the AllMusic id, store section)
KINDS = {
    "dm": ("P1954", "P1729", "albums"),   # Discogs master -> AllMusic album
    "da": ("P1953", "P1728", "artists"),  # Discogs artist -> AllMusic artist
    "t": ("P4577", "P1729", "albums"),    # Tidal album -> AllMusic album
    "a": ("P2281", "P1729", "albums"),    # Apple Music album -> AllMusic album
    "ta": ("P4576", "P1728", "artists"),  # Tidal artist -> AllMusic artist
}


def log(msg):
    print("update-wikidata: " + msg, file=sys.stderr)


def read_collection():
    rel = []
    for f in sorted(glob.glob(os.path.join(CACHE, "draw*.json"))):
        try:
            rel += [r["basic_information"] for r in json.load(open(f, encoding="utf-8")).get("releases", [])]
        except (OSError, ValueError, KeyError) as e:
            log(f"{os.path.basename(f)}: {e}")
    return rel


def read_tidal_store():
    try:
        return json.load(open(TIDAL_STORE, encoding="utf-8")).get("albums", {})
    except (OSError, ValueError) as e:
        log(f"Tidal store: {e}")
        return {}


def wanted_keys(rel, tidal):
    keys = set()
    for tid, e in tidal.items():
        al = e.get("album")
        if not al:
            continue
        keys.add(f"t:{tid}")
        aid = (e.get("apple") or {}).get("id")
        if aid:
            keys.add(f"a:{aid}")
        for a in al.get("artists") or [al.get("artist") or {}]:
            if a.get("id"):
                keys.add(f"ta:{a['id']}")
    for b in rel:
        if b.get("master_id"):
            keys.add(f"dm:{b['master_id']}")
        for a in b.get("artists") or []:
            if a.get("id") and a["id"] not in SKIP_ARTISTS:
                keys.add(f"da:{a['id']}")
    return keys


def due(entry, recheck):
    if not entry or not entry.get("checked"):
        return True
    if recheck == "all":
        return True
    return recheck == "misses" and not entry.get("qid")


def age_days(entry, today):
    try:
        return (today - dt.date.fromisoformat(entry.get("checked", ""))).days
    except ValueError:
        return 0


def remind(store, keys, today):
    """one line when misses are REMIND_DAYS or older: rechecks are by hand"""
    old = [k for k in keys
           if not (e := store[KINDS[k.split(":")[0]][2]].get(k) or {}).get("qid")
           and e.get("checked") and age_days(e, today) >= REMIND_DAYS]
    if old:
        log(f"reminder: {len(old)} keys Wikidata did not know were last checked {REMIND_DAYS}+ days ago"
            f" -- recheck with scripts/update-wikidata.py --recheck")


def query(prop, am_prop, ids):
    values = " ".join(f'"{i}"' for i in ids)
    q = f"""SELECT ?id ?item ?enwiki ?dewiki ?am WHERE {{
  VALUES ?id {{ {values} }}
  ?item wdt:{prop} ?id .
  OPTIONAL {{ ?enwiki schema:about ?item; schema:isPartOf <https://en.wikipedia.org/> }}
  OPTIONAL {{ ?dewiki schema:about ?item; schema:isPartOf <https://de.wikipedia.org/> }}
  OPTIONAL {{ ?item wdt:{am_prop} ?am }}
}}"""
    data = urllib.parse.urlencode({"query": q}).encode()
    headers = {"User-Agent": UA, "Accept": "application/sparql-results+json"}
    for attempt in range(12):
        req = urllib.request.Request(SPARQL, data=data, headers=headers)
        try:
            with urllib.request.urlopen(req, timeout=120) as r:
                return json.load(r)["results"]["bindings"]
        except urllib.error.HTTPError as e:
            if e.code in (429, 502, 503, 504):
                wait = int(e.headers.get("Retry-After") or 0) or 65
                log(f"HTTP {e.code}, waiting {wait} s (attempt {attempt + 1})")
                time.sleep(min(wait, 600))
                continue
            raise
        except (urllib.error.URLError, TimeoutError) as e:
            log(f"{e}, waiting 30 s (attempt {attempt + 1})")
            time.sleep(30)
    raise RuntimeError("Wikidata did not answer after 12 attempts")


def entries_from(rows, today):
    """one entry per id; where Wikidata has several items, the one with most links wins"""
    best = {}
    for x in rows:
        i = x["id"]["value"]
        e = {"qid": x["item"]["value"].rsplit("/", 1)[-1], "checked": today}
        for k in ("enwiki", "dewiki"):
            if k in x:
                e[k] = x[k]["value"]
        if "am" in x:
            e["allmusic"] = x["am"]["value"]
        score = len(e)
        if i not in best or score > best[i][0]:
            best[i] = (score, e)
    return {i: e for i, (_, e) in best.items()}


def write_store(store, backup):
    if backup and os.path.exists(STORE):
        os.replace(STORE, STORE + ".bak")
    tmp = STORE + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(store, f, ensure_ascii=False, indent=0, sort_keys=True)
    os.replace(tmp, STORE)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="count what is due, write nothing")
    ap.add_argument("--limit", type=int, default=0, help="ask at most N keys this run")
    ap.add_argument("--recheck", nargs="?", const="misses", choices=["misses", "all"],
                    help="ask again: misses (default) or all keys")
    a = ap.parse_args()
    today = dt.date.today()
    tstr = today.isoformat()

    store = {"albums": {}, "artists": {}}
    if os.path.exists(STORE):
        store = json.load(open(STORE, encoding="utf-8"))
        for s in ("albums", "artists"):
            store.setdefault(s, {})

    rel = read_collection()
    tidal = read_tidal_store()
    if not rel and not tidal:
        log("no collection in cache/draw*.json and no Tidal store -- nothing to do")
        return
    keys = wanted_keys(rel, tidal)
    todo = sorted(k for k in keys if due(store[KINDS[k.split(":")[0]][2]].get(k), a.recheck))
    if a.limit:
        todo = todo[: a.limit]
    log(f"{len(rel)} releases, {len(tidal)} Tidal albums, {len(keys)} keys, {len(todo)} due")
    if a.dry_run or not todo:
        remind(store, keys, today)
        return

    first = True
    asked = found = 0
    for kind, (prop, am_prop, section) in KINDS.items():
        ids = [k.split(":")[1] for k in todo if k.startswith(kind + ":")]
        for n in range(0, len(ids), BATCH):
            batch = ids[n: n + BATCH]
            got = entries_from(query(prop, am_prop, batch), tstr)
            for i in batch:
                store[section][f"{kind}:{i}"] = got.get(i) or {"checked": tstr}
            asked += len(batch)
            found += len(got)
            store["updated"] = dt.datetime.now().astimezone().isoformat(timespec="seconds")
            write_store(store, first)
            first = False
            log(f"{kind}: {min(n + BATCH, len(ids))}/{len(ids)} asked, {len(got)} found in this batch")

    def count(section, kind, field=None):
        es = [e for k, e in store[section].items() if k.startswith(kind + ":") and k in keys]
        return sum(1 for e in es if (e.get(field) if field else e.get("qid")))

    log(f"asked {asked}, found {found}")
    for kind, (_, _, section) in KINDS.items():
        n = sum(1 for k in keys if k.startswith(kind + ":"))
        log(f"{section} ({kind}): {n} keys | Wikidata {count(section, kind)} | enwiki {count(section, kind, 'enwiki')}"
            f" | dewiki {count(section, kind, 'dewiki')} | AllMusic {count(section, kind, 'allmusic')}")
    remind(store, keys, today)


if __name__ == "__main__":
    main()

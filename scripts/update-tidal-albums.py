#!/usr/bin/env python3
#
# Step C of docs/listened-plan.md: clutter's Tidal album store.
#
# Keeps ~/Sync/clutter/tidal-albums.json (CLUTTER_TIDAL_ALBUMS overrides) with every Tidal
# album that was ever a favourite or was played, so albums can be deleted from the Tidal
# favourites without disappearing from clutter. Per album: the raw Tidal album record,
# favourite since/until, and the genre: Apple Music's, looked up by UPC (no login), or where
# Apple does not know the album, "classical" from a text rule.
#
# Run by update-json after it fetched cache/traw.json; also seeds itself from the snapshots
# in ~/Sync/clutter/tidal-favorites/. Only Tidal albums that were played but are neither a
# favourite nor linked from a Discogs release are fetched from Tidal (a handful).
#
#   scripts/update-tidal-albums.py [--dry-run]
#
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
STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
SNAPSHOTS = os.path.join(HOME, "Sync", "clutter", "tidal-favorites")
LISTENED = os.environ.get("CLUTTER_LISTENED") or os.path.join(HOME, "Sync", "clutter", "listened")
CACHE = os.path.join(HERE, "cache")
# Tidal's public web-client token: enough to read an album record
WEB_TOKEN = "CzET4vdadNUFQ5JU"

# ---------------------------------------------------------------- classical or popular

COMPOSERS = set("""bach mozart beethoven schubert brahms haydn handel händel vivaldi chopin liszt schumann
mendelssohn tchaikovsky tschaikowsky dvořák dvorak mahler bruckner wagner verdi puccini rossini debussy
ravel strauss stravinsky prokofiev prokofieff shostakovich schostakowitsch bartók bartok sibelius grieg
rachmaninov rachmaninoff mussorgsky berlioz bizet satie scarlatti telemann purcell monteverdi pärt glass
reich ligeti penderecki messiaen britten elgar holst palestrina dowland couperin rameau lully corelli
albinoni boccherini paganini saint-saëns fauré franck gluck weber bellini donizetti offenbach smetana
janáček janacek nielsen respighi hindemith schoenberg schönberg webern ives copland barber gershwin
poulenc honegger milhaud rzewski cage feldman xenakis stockhausen boulez""".split())
WORK = re.compile(r"\b(symphon\w*|sinfoni\w*|concert[oi]s?|konzert\w*|sonat\w*|quartet\w*|quartett\w*|"
                  r"trio|quintet\w*|requiem|mass|messe|missa|cantata|kantate\w*|oratori\w*|opera|oper|"
                  r"overture\w*|ouvert\w*|suite[sn]?|variation\w*|prelude\w*|préludes?|etudes?|études?|"
                  r"nocturne\w*|partita\w*|fugue?\w*|lieder|arias?|serenade|divertiment\w*|op\.|bwv|"
                  r"k\.\s?\d|kv\s?\d|d\.\s?\d|hob)\b")
ENSEMBLE = re.compile(r"\b(orchestr\w*|orchest\w*|philharmon\w*|chamber|kammer\w*|ensemble|choir|chor|"
                      r"chorus|consort|quartet|quartett|sinfonietta|staatskapelle|gewandhaus|"
                      r"concertgebouw|academy of|camerata|musicae?|baroque|barock)\b")
LABELS = re.compile(r"(deutsche grammophon|decca|ecm new series|harmonia mundi|naxos|\bbis\b|hyperion|"
                    r"chandos|sony classical|warner classics|erato|alpha classics|pentatone|archiv|philips|"
                    r"emi classics|teldec|virgin classics|nonesuch|ondine|capriccio|\bcpo\b|brilliant classics|"
                    r"orfeo|melodiya|supraphon|hänssler|channel classics|linn|signum|onyx|avie|outhere|"
                    r"ricercar|glossa|russian compact disc|sono luminus|bis records|cedille|delos|bridge records|"
                    r"navona|orchid classics|rubicon|resonus|evil penguin|accentus|berlin classics)")


def classical_performers(coll):
    """artists that appear only on LATB's classical Discogs releases"""
    cls, other = set(), set()
    for r in coll.values():
        names = {re.sub(r" \(\d+\)$", "", a["name"]).lower() for a in r["basic_information"]["artists"]}
        (cls if "Classical" in r["basic_information"].get("genres", []) else other).update(names)
    return {n for n in cls - other if len(n) > 3 and n not in ("various",)}


def is_classical(artists, title, label, performers):
    t = (" ".join(artists) + " " + title).lower()
    score = 0
    if any(re.search(r"\b" + re.escape(c) + r"\b", t) for c in COMPOSERS):
        score += 2
    if any(a.lower() in performers for a in artists):
        score += 2
    if WORK.search(t):
        score += 1
    if ENSEMBLE.search(t):
        score += 1
    if LABELS.search(label.lower()):
        score += 2
    return score >= 2


APPLE_CLASSICAL = {"Classical", "Opera", "Orchestral", "Chamber Music", "Baroque Era", "Classical Era",
                   "Romantic Era", "Modern Era", "Contemporary Era", "Renaissance", "Medieval Era", "Choral",
                   "Art Song", "Minimalism", "Impressionist", "High Classical"}
# Apple genres that say little about classical or not: there the text rule decides
APPLE_VAGUE = {"Holiday", "Soundtrack", "Original Score", "Piano", "Solo Instrumental", "Vocal",
               "Classical Crossover", "Instrumental", "Easy Listening"}


def genre_of(e, performers):
    """Apple's genre when Apple knows the album ("classical" for Classical and Opera),
    else "classical" if the text rule says so, else none"""
    g = (e.get("apple") or {}).get("genre")
    if g in APPLE_CLASSICAL:
        return "classical"
    if g and g not in APPLE_VAGUE:
        return g.casefold()
    album = e["album"]
    artists = [a["name"] for a in album.get("artists") or []]
    if is_classical(artists, album.get("title", ""), album.get("copyright") or "", performers):
        return "classical"
    return g.casefold() if g else None


def norm_title(t):
    t = re.sub(r"\s*[\(\[].*?[\)\]]", "", t.casefold())
    t = re.sub(r"\s+-\s+(ep|single)$", "", t)
    return re.sub(r"\W+", "", t)


def apple_lookup(entries, today):
    """Apple Music's genre for albums with a UPC, 100 per request; the answers carry no UPC,
    so they are matched back by title (and artist when titles repeat)"""
    asked = found = 0
    todo = [e for e in entries if e.get("album") and e["album"].get("upc") and "apple" not in e]
    for i in range(0, len(todo), 100):
        batch = todo[i:i + 100]
        url = "https://itunes.apple.com/lookup?entity=album&upc=" + ",".join(e["album"]["upc"] for e in batch)
        try:
            with urllib.request.urlopen(url, timeout=60) as r:
                res = [x for x in json.load(r).get("results", []) if x.get("wrapperType") == "collection"]
        except (urllib.error.URLError, TimeoutError, ValueError):
            continue  # asked again next run
        asked += len(batch)
        for e in batch:
            t = norm_title(e["album"]["title"])
            cands = [x for x in res if norm_title(x.get("collectionName", "")) == t]
            if not cands and len(t) >= 6:  # "Bach: Goldberg Variations" vs "Goldberg Variations"
                cands = [x for x in res if (n := norm_title(x.get("collectionName", ""))) and len(n) >= 6
                         and (n in t or t in n)]
            if len(cands) > 1:
                artist = ((e["album"].get("artist") or {}).get("name") or "").casefold()
                cands = [x for x in cands if artist and artist in x.get("artistName", "").casefold()] or cands[:1]
            if cands:
                x = cands[0]
                e["apple"] = {"genre": x.get("primaryGenreName"), "id": x.get("collectionId"), "checked": today}
                found += 1
            else:
                e["apple"] = {"checked": today}
        time.sleep(3)
    return asked, found


# ---------------------------------------------------------------- inputs

def read_favourites(fn):
    """{id: (created, album)} from a favourites dump, or None if the dump is not a complete one"""
    try:
        d = json.load(open(fn, encoding="utf-8"))
        items = d["items"]
        if len(items) != d.get("totalNumberOfItems", len(items)):
            return None
        return {it["item"]["id"]: (it["created"], it["item"]) for it in items}
    except (OSError, ValueError, KeyError, TypeError):
        return None


def read_collection():
    coll = {}
    for fn in glob.glob(os.path.join(CACHE, "draw*.json")):
        try:
            for r in json.load(open(fn))["releases"]:
                coll[r["id"]] = r
        except (OSError, ValueError, KeyError):
            pass
    return coll


def linked_tidal(coll):
    """Tidal id -> Discogs release id, from the Location field (T<id>)"""
    out = {}
    for r in coll.values():
        for n in r.get("notes") or []:
            if n.get("field_id") == 4:
                for t in re.findall(r"\bT(\d+)", n.get("value", "")):
                    out[int(t)] = r["id"]
    return out


def played_tidal():
    ids = set()
    for fn in glob.glob(os.path.join(LISTENED, "*.jsonl")):
        for line in open(fn, encoding="utf-8"):
            try:
                e = json.loads(line)
            except ValueError:
                continue
            if e.get("tidal"):
                ids.add(e["tidal"])
    return ids


def fetch_album(tid):
    req = urllib.request.Request(f"https://api.tidal.com/v1/albums/{tid}?countryCode=US",
                                 headers={"x-tidal-token": WEB_TOKEN, "User-Agent": "clutter"})
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            return json.load(r)
    except urllib.error.HTTPError as e:
        return {"_error": e.code}
    except (urllib.error.URLError, TimeoutError):
        return None


# ---------------------------------------------------------------- main

def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="report, write nothing")
    a = ap.parse_args()
    today = dt.date.today().isoformat()
    log = []

    store = {"albums": {}}
    if os.path.exists(STORE):
        store = json.load(open(STORE, encoding="utf-8"))
    albums = store["albums"]
    before = len(albums)

    coll = read_collection()
    performers = classical_performers(coll)
    linked = linked_tidal(coll)

    # favourites: the seed snapshots first, then the current dump
    snaps = sorted(glob.glob(os.path.join(SNAPSHOTS, "*.json")))
    current = read_favourites(os.path.join(CACHE, "traw.json"))
    if current is None:
        log.append("cache/traw.json is missing or incomplete: favourites not updated (stale Tidal token?)")
    for fn in snaps + ([os.path.join(CACHE, "traw.json")] if current else []):
        for tid, (created, album) in (read_favourites(fn) or {}).items():
            e = albums.setdefault(str(tid), {})
            e["album"] = album
            fav = e.get("favorite") or {"since": created, "until": None}
            fav["since"] = min(fav.get("since") or created, created)
            e["favorite"] = fav
    new_favs = unfavs = 0
    if current:
        for tid, e in albums.items():
            fav = e.get("favorite")
            if not fav:
                continue
            if int(tid) in current:
                if fav.get("until"):
                    fav["until"] = None  # favourite again
                    new_favs += 1
            elif not fav.get("until"):
                fav["until"] = today
                unfavs += 1

    # played Tidal albums clutter has no record of, and no Discogs release stands for
    missing = sorted(t for t in played_tidal() if str(t) not in albums and t not in linked)
    fetched = failed = 0
    for tid in missing:
        if a.dry_run:
            continue
        r = fetch_album(tid)
        if r is None:
            failed += 1
            continue
        e = albums.setdefault(str(tid), {"favorite": None})
        if "_error" in r:
            e["missing"] = r["_error"]  # gone from Tidal; the play keeps its own artist and title
            failed += 1
        else:
            e["album"] = r
            fetched += 1
        e["fetched"] = today

    # the genre: Apple Music's by UPC, else the text rule for classical
    asked, found = (0, 0) if a.dry_run else apple_lookup(albums.values(), today)
    for e in albums.values():
        if e.get("album"):
            e["genre"] = genre_of(e, performers)

    # how well the rule agrees with Discogs, on albums a Discogs release links to
    agree = total = 0
    for tid, e in albums.items():
        rid = linked.get(int(tid))
        if rid and e.get("album"):
            total += 1
            agree += (e["genre"] == "classical") == ("Classical" in coll[rid]["basic_information"].get("genres", []))

    store["updated"] = dt.datetime.now().astimezone().isoformat(timespec="seconds")
    nfav = sum(1 for e in albums.values() if (e.get("favorite") or {}).get("since") and not e["favorite"].get("until"))
    ncls = sum(1 for e in albums.values() if e.get("genre") == "classical")
    napple = sum(1 for e in albums.values() if (e.get("apple") or {}).get("genre"))
    log.append(f"{len(albums)} albums ({before} before): {nfav} favourites, "
               f"{sum(1 for e in albums.values() if (e.get('favorite') or {}).get('until'))} former favourites, "
               f"{sum(1 for e in albums.values() if not e.get('favorite'))} played only; "
               f"{new_favs} favourites again, {unfavs} newly unfavourited; "
               f"fetched {fetched} of {len(missing)} played albums ({failed} failed)")
    log.append(f"genre: Apple knows {napple} albums (asked {asked} new, found {found}); {ncls} classical; "
               f"classical agrees with Discogs on {agree} of {total} linked albums")
    if not a.dry_run:
        if os.path.exists(STORE):
            os.replace(STORE, STORE + ".bak")
        tmp = STORE + ".tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            json.dump(store, f, ensure_ascii=False, indent=0, sort_keys=True)
        os.replace(tmp, STORE)
    for line in log:
        print("update-tidal-albums: " + line, file=sys.stderr)


if __name__ == "__main__":
    main()

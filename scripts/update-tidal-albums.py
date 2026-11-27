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
# Also takes in Tidal's "Save for later" list (read with the token in tok.json): albums saved
# there enter the store as "later" since/until — clutter's inbox for albums not yet judged,
# which are not favourites. The server runs update-json --tidal for "Update from Tidal".
#
# gone: since when an album is known to be off Tidal. Tidal keeps a vanished album in the
# favourites, flagged not streamable (its album page answers 404); an album that leaves the
# lists is asked for once. --check-gone asks for every album that is on no list.
#
# dropped: removed from clutter by hand (--drop TID, the album page's "Remove from clutter";
# --undrop TID takes it back). The server leaves dropped albums out. Saving or favouriting
# the album again on Tidal after the drop brings it back.
#
# Run by update-json after it fetched cache/traw.json; also seeds itself from the snapshots
# in ~/Sync/clutter/tidal-favorites/. Only Tidal albums that were played but are neither a
# favourite nor linked from a Discogs release are fetched from Tidal (a handful).
#
#   scripts/update-tidal-albums.py [--dry-run]
#
# --apple TID=AID (repeatable) only sets the Apple Music album of one Tidal album, by hand,
# where the UPC lookup found none or the wrong one; ids or album links, as in
#   --apple 'https://tidal.com/album/283130328=https://music.apple.com/us/album/x/1556465220'
# The Apple genre comes along; the next regular run keeps the entry.
#
# --album TID (repeatable; album-layer slice 10, "Update this album") only asks Tidal for
# those albums again and rewrites their records: title, cover, quality, and gone (404, or
# Tidal marks it not streamable) or back. An album the store does not hold is not taken in.
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
STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
SNAPSHOTS = os.path.join(HOME, "Sync", "clutter", "tidal-favorites")
LISTENED = os.environ.get("CLUTTER_LISTENED") or os.path.join(HOME, "Sync", "clutter", "listened")
CACHE = os.path.join(HERE, "cache")
# Tidal's public web-client token: enough to read an album record
WEB_TOKEN = "CzET4vdadNUFQ5JU"
# Tidal's newer API, for the "Save for later" collection
OPENAPI = "https://openapi.tidal.com/v2"

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
    """Tidal id -> Discogs release id, from clutter's collection log (until 2026-11-25:
    from the Location field in Discogs); only releases still in the collection"""
    return collection_log.release_of_tidal(keep=lambda rid: rid in coll)


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


def read_later():
    """{album id: addedAt} of Tidal's "Save for later", or None if it could not be read"""
    try:
        tok = json.load(open(os.path.join(HERE, "tok.json"), encoding="utf-8"))["tdt"]
    except (OSError, ValueError, KeyError):
        return None
    out = {}
    url = OPENAPI + "/userCollectionSaveForLaters/me?include=items&countryCode=US"
    first = True
    for _ in range(200):  # pages
        req = urllib.request.Request(url, headers={"Authorization": "Bearer " + tok,
                                                   "Accept": "application/vnd.api+json"})
        try:
            with urllib.request.urlopen(req, timeout=30) as r:
                d = json.load(r)
            # the first answer is the collection, with its items as a relationship; the
            # pages after it are the relationship alone
            rel = d["data"]["relationships"]["items"] if first else d
            for it in rel.get("data") or []:
                if it.get("type") == "albums":
                    out[int(it["id"])] = (it.get("meta") or {}).get("addedAt") or ""
            nxt = (rel.get("links") or {}).get("next")
        except (urllib.error.URLError, TimeoutError, ValueError, KeyError, TypeError):
            return None  # never the token or the URL in a message
        if not nxt:
            return out
        url = OPENAPI + nxt if nxt.startswith("/") else nxt
        first = False
    return out


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


def fetch_apple(aid):
    try:
        with urllib.request.urlopen(f"https://itunes.apple.com/lookup?id={aid}&entity=album", timeout=30) as r:
            res = [x for x in json.load(r).get("results", []) if x.get("wrapperType") == "collection"]
    except (urllib.error.URLError, TimeoutError, ValueError):
        return None
    return res[0] if res else {}


def parse_pair(arg):
    """TID=AID, each an id or an album link; None if either id is missing"""
    t, _, a = arg.partition("=")
    mt = re.search(r"(?:album/)?(\d+)\D*$", t.split("?")[0])
    ma = re.search(r"album/(?:[^/?]+/)?(?:id)?(\d+)", a) or re.fullmatch(r"\s*(?:id)?(\d+)\s*", a)
    return (mt.group(1), int(ma.group(1))) if mt and ma else None


def write_store(store):
    if os.path.exists(STORE):
        os.replace(STORE, STORE + ".bak")
    tmp = STORE + ".tmp"
    with open(tmp, "w", encoding="utf-8") as f:
        json.dump(store, f, ensure_ascii=False, indent=0, sort_keys=True)
    os.replace(tmp, STORE)


def set_apple(store, pairs, dry_run, today):
    """--apple: the Apple album of each Tidal album, by hand; False if one failed"""
    albums = store["albums"]
    performers = classical_performers(read_collection())
    ok = True
    for arg in pairs:
        p = parse_pair(arg)
        if not p:
            print(f"update-tidal-albums: --apple {arg}: want TID=AID (ids or album links)", file=sys.stderr)
            ok = False
            continue
        tid, aid = p
        e = albums.get(tid)
        if not (e and e.get("album")):
            r = fetch_album(tid)
            if r is None or "_error" in r:
                print(f"update-tidal-albums: Tidal {tid}: not found ({(r or {}).get('_error', 'no answer')})",
                      file=sys.stderr)
                ok = False
                continue
            e = albums.setdefault(tid, {"favorite": None})
            e["album"], e["fetched"] = r, today
        x = fetch_apple(aid)
        if not x:
            print(f"update-tidal-albums: Apple {aid}: {'not found' if x == {} else 'no answer'}", file=sys.stderr)
            ok = False
            continue
        old = (e.get("apple") or {}).get("id")
        if old and old != x.get("collectionId"):
            print(f"Tidal {tid}: Apple {old} replaced")
        e["apple"] = {"genre": x.get("primaryGenreName"), "id": x.get("collectionId"), "checked": today,
                      "manual": True}
        e["genre"] = genre_of(e, performers)
        t = e["album"]
        print(f"Tidal {tid}: {(t.get('artist') or {}).get('name')} – {t.get('title')}"
              f" ({(t.get('releaseDate') or '')[:4]}, {t.get('numberOfTracks')} tracks)\n"
              f"Apple {aid}: {x.get('artistName')} – {x.get('collectionName')}"
              f" ({(x.get('releaseDate') or '')[:4]}, {x.get('trackCount')} tracks)\n"
              f"  genre {x.get('primaryGenreName')} -> {e['genre']}")
    if not dry_run:
        store["updated"] = dt.datetime.now().astimezone().isoformat(timespec="seconds")
        write_store(store)
    return ok


# ---------------------------------------------------------------- main

def update_albums(albums, ids, dry_run, today, store):
    """--album: ask Tidal for these held albums again; rewrite record, fetched and gone."""
    tids = [t[1:] if t[:1] in "Tt" else t for t in ids]
    notes, changed, ok = [], 0, True
    for tid in tids:
        e = albums.get(tid)
        if not tid.isdigit() or e is None:
            notes.append(f"T{tid}: not held by clutter (not taken in)")
            continue
        if dry_run:
            notes.append(f"T{tid}: would be asked")
            continue
        r = fetch_album(tid)
        if r is None:
            notes.append(f"T{tid}: Tidal did not answer")
            ok = False
            continue
        if r.get("_error") == 404:
            if "gone" not in e:
                e["gone"] = today
                changed += 1
            notes.append(f"T{tid}: gone from Tidal")
            continue
        if "_error" in r:
            notes.append(f"T{tid}: Tidal answered {r['_error']}")
            ok = False
            continue
        old = e.get("album") or {}
        diff = [k for k in ("title", "cover", "audioQuality", "mediaMetadata", "releaseDate", "numberOfTracks") if old.get(k) != r.get(k)]
        e["album"], e["fetched"] = r, today
        if r.get("streamReady") is False or r.get("allowStreaming") is False:
            e.setdefault("gone", today)
            notes.append(f"T{tid}: not streamable (gone)")
        elif e.pop("gone", None):
            notes.append(f"T{tid}: back on Tidal")
        else:
            notes.append(f"T{tid}: " + (", ".join(dict.fromkeys("quality" if k in ("audioQuality", "mediaMetadata") else k for k in diff)) if diff else "unchanged"))
        changed += 1
    if changed and not dry_run:
        write_store(store)
    for n in notes:
        print(n)
    print(f"{len(tids)} albums asked, {changed} rewritten" + (" — dry run" if dry_run else ""))
    return ok


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--dry-run", action="store_true", help="report, write nothing")
    ap.add_argument("--apple", action="append", metavar="TID=AID",
                    help="only set the Apple Music album of a Tidal album (ids or album links)")
    ap.add_argument("--drop", action="append", type=int, metavar="TID",
                    help="only mark a Tidal album as removed from clutter")
    ap.add_argument("--undrop", action="append", type=int, metavar="TID",
                    help="only take a removed Tidal album back")
    ap.add_argument("--check-gone", action="store_true",
                    help="also ask Tidal for every album that is on no list")
    ap.add_argument("--album", action="append", metavar="TID",
                    help="only ask Tidal for this album again (T<id> or <id>; repeatable) and rewrite its record")
    ap.add_argument("--take", metavar="FILE",
                    help="only take the Tidal albums in FILE (one id per line) into the store, as a played "
                         "non-favourite is (album layer slice 6: what a retired stand-in release stood for)")
    ap.add_argument("--found-only", action="store_true",
                    help="with --take: leave out an album Tidal does not have (an id typed in by hand), "
                         "instead of keeping it as missing; exit 1 if none was taken")
    a = ap.parse_args()
    today = dt.date.today().isoformat()
    log = []

    store = {"albums": {}}
    if os.path.exists(STORE):
        store = json.load(open(STORE, encoding="utf-8"))
    if a.apple:
        sys.exit(0 if set_apple(store, a.apple, a.dry_run, today) else 1)
    albums = store["albums"]
    if a.drop or a.undrop:
        ok = True
        now = dt.datetime.now(dt.timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ")
        for tid in a.drop or []:
            e = albums.get(str(tid))
            if e is None:
                print(f"update-tidal-albums: no album {tid} in the store", file=sys.stderr)
                ok = False
            else:
                e["dropped"] = now
        for tid in a.undrop or []:
            if albums.get(str(tid), {}).pop("dropped", None) is None:
                print(f"update-tidal-albums: album {tid} is not removed", file=sys.stderr)
                ok = False
        if ok and not a.dry_run:
            write_store(store)
        sys.exit(0 if ok else 1)
    if a.album:
        sys.exit(0 if update_albums(albums, a.album, a.dry_run, today, store) else 1)
    if a.take:
        # held from now on without a favourite or a play: the entry stays in the store like a
        # played one; "taken" says why. Albums the store has already are left as they are.
        tids = [w for w in open(a.take).read().split() if w.lstrip("T").isdigit()]
        new = [t.lstrip("T") for t in tids if t.lstrip("T") not in albums]
        fetched = failed = 0
        for tid in new:
            if a.dry_run:
                continue
            r = fetch_album(tid)
            if r is None or (a.found_only and "_error" in r):
                failed += 1  # no answer (run again), or not at Tidal and --found-only: not taken
                continue
            e = albums.setdefault(tid, {"favorite": None})
            e["taken"] = today
            e["fetched"] = today
            if "_error" in r:
                e["missing"] = r["_error"]
                failed += 1
            else:
                e["album"] = r
                fetched += 1
        print(f"update-tidal-albums: take {len(tids)} albums: {len(tids) - len(new)} in the store already, "
              f"{fetched} fetched, {failed} not (gone from Tidal or no answer)" + (" — dry run" if a.dry_run else ""))
        if not a.dry_run and (fetched or not a.found_only):
            write_store(store)
        sys.exit(1 if a.found_only and new and not fetched and not a.dry_run else 0)
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

    # Tidal's "Save for later": saved since/until, like the favourites; an album saved there
    # that clutter does not hold yet is fetched
    later = read_later()
    new_later = unlater = later_failed = 0
    if later is None:
        log.append("Save for later could not be read (stale Tidal token?): not updated")
    else:
        for tid, added in later.items():
            e = albums.setdefault(str(tid), {"favorite": None})
            lt = e.get("later") or {"since": added, "until": None}
            lt["since"] = min(lt.get("since") or added, added) or added
            lt["until"] = None
            if "later" not in e:
                new_later += 1
            e["later"] = lt
            if not e.get("album") and not a.dry_run:
                r = fetch_album(tid)
                if r is None:
                    later_failed += 1
                elif "_error" in r:
                    e["missing"] = r["_error"]
                    later_failed += 1
                else:
                    e["album"], e["fetched"] = r, today
        for tid, e in albums.items():
            lt = e.get("later")
            if lt and not lt.get("until") and int(tid) not in later:
                lt["until"] = today  # taken off the list
                unlater += 1

    def listed(e):
        return any((e.get(k) or {}).get("since") and not (e.get(k) or {}).get("until") for k in ("favorite", "later"))

    # removed from clutter, then saved or favourited again on Tidal: back
    undropped = 0
    for tid, e in albums.items():
        d = e.get("dropped")
        if d:
            again = [(current or {}).get(int(tid), ("",))[0], (later or {}).get(int(tid), "")]
            if any(c and c[:19] > d[:19] for c in again):
                del e["dropped"]
                undropped += 1

    # gone from Tidal. An album that left the lists today, or every unlisted one with
    # --check-gone, is asked for: 404 is gone, a record replaces the stored one. Then the
    # record's own flags: Tidal keeps a vanished favourite, marked not streamable.
    asked_gone = 0
    for tid, e in albums.items():
        if a.dry_run or not e.get("album") or listed(e):
            continue
        left_today = any((e.get(k) or {}).get("until") == today for k in ("favorite", "later"))
        if left_today or a.check_gone:
            r = fetch_album(tid)
            asked_gone += 1
            if r is not None and r.get("_error") == 404:
                e.setdefault("gone", today)
            elif r is not None and "_error" not in r:
                e["album"] = r
                e.pop("gone", None)
    for e in albums.values():
        al = e.get("album") or {}
        if al.get("streamReady") is False or al.get("allowStreaming") is False:
            e.setdefault("gone", today)
        elif al and listed(e):
            e.pop("gone", None)  # its record is from today's lists

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
    if later is not None:
        log.append(f"Save for later: {len(later)} albums, {new_later} new, {unlater} taken off"
                   + (f", {later_failed} not fetched" if later_failed else ""))
    log.append(f"{sum(1 for e in albums.values() if e.get('gone'))} gone from Tidal (asked about {asked_gone}); "
               f"{sum(1 for e in albums.values() if e.get('dropped'))} removed from clutter, {undropped} back")
    log.append(f"genre: Apple knows {napple} albums (asked {asked} new, found {found}); {ncls} classical; "
               f"classical agrees with Discogs on {agree} of {total} linked albums")
    if not a.dry_run:
        write_store(store)
    for line in log:
        print("update-tidal-albums: " + line, file=sys.stderr)


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
#
# Phase 3 of docs/listened-plan.md: backfill clutter's play log from
#   - the Discogs "YYYY Listened" lists 2020-2025 (cache/l<id>-raw.json; comment = dates, one per line)
#   - the 2026 Want-list transfer (cache/listened-import/2026-want-to-listened.json; stamps with time)
#   - the AlbumsPlayed notes (~/Sync/AlbumsPlayed/*.md), for time of day and Tidal/Apple-only plays
# and write listened/YYYY.jsonl into a STAGING directory plus a reconciliation report.
# It never touches the live log, Discogs, or tok.json; it only reads local files.
#
#   scripts/import-listened.py [--out cache/listened-import/out] [--notes ~/Sync/AlbumsPlayed]
#                              [--live ~/Sync/clutter/listened]
#   scripts/import-listened.py --merge     (server stopped!) staged + live -> live
#
# Rerunnable: the staging directory is rewritten on every run. --merge first copies the live
# log to cache/listened-import/live-backup-<time>/, then rewrites each live year file as its own
# non-imported events (via clutter/manual) plus the staged events, sorted by time, so running
# it twice does not duplicate anything.
#
import argparse
import collections
import datetime as dt
import difflib
import glob
import json
import os
import re
import sys
import unicodedata
from zoneinfo import ZoneInfo

LOCAL = ZoneInfo("America/Chicago")
UTC = dt.timezone.utc

LISTS = {  # Discogs "YYYY Listened" list ids
    2020: 540434, 2021: 701908, 2022: 915964, 2023: 1174210, 2024: 1442779, 2025: 1574563,
}
# counts from the plan ("Why"), items / dates
EXPECTED = {2020: (167, 232), 2021: (497, 546), 2022: (159, 174), 2023: (281, 309),
            2024: (152, 159), 2025: (404, 428), 2026: (86, 119)}

# AlbumsPlayed stamps are UTC up to and including this one, local after it (2026-09 report)
UTC_CUTOVER = "2021-07-01T03:02:59"
NOTE_MATCH_TIMED = dt.timedelta(minutes=30)

# AlbumsPlayed notes that link no album, identified by LATB 2026-11-01
NOTE_IDS = {
    "201229-Bob Dylan – Rough And Rowdy Ways.md": ("discogs", 15629634),
    "201229-Wagner - Götterdämmerung, Solti.md": ("discogs", 4888888),
    "201230-Mozart – Horn Concertos_ Rondeau K. 371 ·.md": ("discogs", 6285819),
    "201230-Massive Attack – Mezzanine.md": ("discogs", 5077187),
    "201230-PJ Harvey – To Bring You My Love.md": ("discogs", 15895363),
    "201230-Beethoven – Symphonie Nr. 5  - Wiener Ph.md": ("discogs", 1093216),
    "201231-Frank Zappa Featuring Captain Beefheart .md": ("discogs", 12061510),
    "201231-Hendrix - Band Of Gypsys.md": ("discogs", 7244343),
    "210101-Mozart – Horn Concertos_ Rondeau K. 371.md": ("discogs", 6285819),
    "210414-Bruch_ Violinkonzert G-Moll Op. 26 _ Men.md": ("discogs", 3966429),
    "210215-Genesis - Selling England by the Pound; .md": ("tidal", 35095630),
}

# AlbumsPlayed notes that link a pressing LATB does not own; the play was of his copy (2026-11-01)
RELEASE_ALIASES = {
    12213707: 24619814,  # Richter, Brahms – Liszt: the owned CD
    10991645: 4676923,  # Electric Ladyland: the owned LP (10991645 has no master)
}

FIELD_ORDER = ["ts", "src", "discogs", "inst", "tidal", "apple", "applelib",
               "artist", "title", "via", "note"]


def event(**kw):
    return {k: kw[k] for k in FIELD_ORDER if kw.get(k) is not None}


def iso(t: dt.datetime) -> str:
    return t.astimezone(LOCAL).isoformat(timespec="seconds")


def split_title(s: str, seps=(" – ", " - ")):
    for sep in seps:
        if sep in s:
            a, t = s.split(sep, 1)
            return a.strip(), t.strip()
    return "", s.strip()


# ---------------------------------------------------------------- collection

def read_collection(cache):
    """release id -> {inst, tidal, apple, artist, title, plays}; tidal id -> release id"""
    coll, t2d = {}, {}
    for fn in sorted(glob.glob(os.path.join(cache, "draw*.json"))):
        for r in json.load(open(fn))["releases"]:
            bi = r["basic_information"]
            notes = {n.get("field_id"): n.get("value", "") for n in r.get("notes") or []}
            loc = notes.get(4, "")
            tid = re.search(r"\bT(\d+)", loc)
            aid = re.search(r"\bA(\d+)", loc)
            c = {
                "inst": r.get("instance_id"),
                "tidal": int(tid.group(1)) if tid else None,
                "apple": int(aid.group(1)) if aid else None,
                "artist": ", ".join(re.sub(r" \(\d+\)$", "", a["name"]) for a in bi.get("artists", [])),
                "title": bi.get("title", ""),
                "plays": int(notes[7]) if str(notes.get(7, "")).isdigit() else 0,
            }
            coll.setdefault(r["id"], c)
            if c["tidal"]:
                t2d[c["tidal"]] = r["id"]
    return coll, t2d


# ---------------------------------------------------------------- list events

STAMP_RE = re.compile(r"(\d{4})-(\d{2}-\d{2})(?:[T ](\d{2}:\d{2}))?")
FIRST_YEAR, THIS_YEAR = 2015, dt.date.today().year


def parse_stamps(line, list_year, report, where):
    """all stamps in a comment line ('2025-05-17, 2025-05-14' happens):
    'YYYY-MM-DD' -> date, 'YYYY-MM-DDTHH:MM' -> aware local datetime; None if there are none.
    An impossible year (2923) is replaced by the list's year and reported."""
    out = []
    for y, md, hm in STAMP_RE.findall(line):
        if not FIRST_YEAR <= int(y) <= THIS_YEAR:
            report["typos"].append((where, line.strip(), f"{list_year}-{md}"))
            y = str(list_year)
        if hm:
            out.append(dt.datetime.fromisoformat(f"{y}-{md}T{hm}").replace(tzinfo=LOCAL))
        else:
            out.append(dt.date.fromisoformat(f"{y}-{md}"))
    if len(out) > 1:
        report["multi_lines"].append((where, line.strip()))
    return out or None


def list_events(cache, coll, report):
    evs = []
    for year, lid in LISTS.items():
        d = json.load(open(os.path.join(cache, f"l{lid}-raw.json")))
        ndates = 0
        for it in d["items"]:
            where = f"{year} Listened, {it.get('type')} {it['id']}"
            for line in (it.get("comment") or "").splitlines():
                if not line.strip():
                    continue
                sts = parse_stamps(line, year, report, where)
                if sts is None:
                    report["bad_lines"].append((where, line))
                    continue
                ndates += len(sts)
                if it.get("type") != "release":  # a master has no release id to log against
                    report["masters"].append((where, it.get("display_title", ""), line.strip()))
                    continue
                evs += [("import:discogs-list", it["id"], st, it.get("display_title", "")) for st in sts]
        report["list_counts"][year] = (len(d["items"]), ndates)
    # 2026: the Want-list stamps, as transferred to "2026 Listened"
    tr = json.load(open(os.path.join(cache, "listened-import", "2026-want-to-listened.json")))
    ndates = 0
    for it in tr:
        for line in (it.get("comment") or "").splitlines():
            if not line.strip():
                continue
            where = f"2026 transfer, release {it['id']}"
            sts = parse_stamps(line, 2026, report, where)
            if sts is None:
                report["bad_lines"].append((where, line))
                continue
            ndates += len(sts)
            evs += [("import:want", it["id"], st, f'{it.get("artist", "")} - {it.get("title", "")}') for st in sts]
    report["list_counts"][2026] = (len(tr), ndates)
    return evs


# ---------------------------------------------------------------- AlbumsPlayed notes

def read_notes(notes_dir, report):
    notes = []
    for fn in sorted(glob.glob(os.path.join(notes_dir, "*.md"))):
        t = open(fn, encoding="utf-8").read()
        m = re.search(r"^date:\s*(\d{4}-\d{2}-\d{2})T(\d{2})[:-](\d{2})(?:[:-](\d{2}))?", t, re.M)
        if not m:
            report["bad_notes"].append(os.path.basename(fn))
            continue
        raw = f"{m.group(1)}T{m.group(2)}:{m.group(3)}:{m.group(4) or '00'}"
        naive = dt.datetime.fromisoformat(raw)
        utc = raw <= UTC_CUTOVER
        when = (naive.replace(tzinfo=UTC) if utc else naive.replace(tzinfo=LOCAL)).astimezone(LOCAL)
        tm = re.search(r"^title:\s*(.+)$", t, re.M)
        did = re.search(r"https?://(?:www\.)?discogs\.com/+(?:[^\s/]+/)?(?:release/)?(\d+)", t)
        tid = re.search(r"(?:tidal\.com/(?:browse/)?album/|tidal://album/)(\d+)", t)
        aid = re.search(r"music\.apple\.com/[^\s]*?/album/(?:[^\s/]+/)?(\d+)", t)
        known = NOTE_IDS.get(unicodedata.normalize("NFC", os.path.basename(fn)))  # macOS names are NFD
        if known and not (did or tid or aid):
            did = tid = None
            if known[0] == "discogs":
                did = re.match(r"(\d+)", str(known[1]))
            else:
                tid = re.match(r"(\d+)", str(known[1]))
            report["identified"].append((os.path.basename(fn), *known))
        if did and int(did.group(1)) in RELEASE_ALIASES:
            report["aliased"].append((os.path.basename(fn), int(did.group(1)), RELEASE_ALIASES[int(did.group(1))]))
            did = re.match(r"(\d+)", str(RELEASE_ALIASES[int(did.group(1))]))
        notes.append({
            "file": os.path.basename(fn),
            "when": when,
            "utc": utc,
            "title": tm.group(1).strip() if tm else os.path.basename(fn)[7:-3],
            "discogs": int(did.group(1)) if did else None,
            "tidal": int(tid.group(1)) if tid else None,
            "apple": int(aid.group(1)) if aid else None,
            "used": False,
        })
    return notes


def read_live(live_dir):
    """the live log's own plays (clutter, manual): a previous import is replaced by --merge"""
    evs = []
    for fn in sorted(glob.glob(os.path.join(live_dir, "*.jsonl"))):
        for line in open(fn, encoding="utf-8"):
            try:
                e = json.loads(line)
            except json.JSONDecodeError:
                continue
            if not e.get("via", "").startswith("import:"):
                evs.append(e)
    return evs


# ---------------------------------------------------------------- matching

def note_releases(n, t2d):
    """the Discogs releases a note can stand for"""
    rs = set()
    if n["discogs"]:
        rs.add(n["discogs"])
    if n["tidal"] and n["tidal"] in t2d:
        rs.add(t2d[n["tidal"]])
    return rs


def best_note(cands, st):
    """closest unused note for a stamp: same local day, else same UTC day, else +-1 day; timed: +-30 min"""
    if isinstance(st, dt.datetime):
        ok = [(abs(n["when"] - st), n) for n in cands if abs(n["when"] - st) <= NOTE_MATCH_TIMED]
    else:
        ok = []
        for n in cands:
            ld, ud = n["when"].date(), n["when"].astimezone(UTC).date()
            rank = 0 if ld == st else 1 if ud == st else 2 if abs((ld - st).days) <= 1 else None
            if rank is not None:
                ok.append((rank, n))
    ok.sort(key=lambda x: (x[0], x[1]["when"]))
    return ok[0][1] if ok else None


def words(s):
    return {w for w in re.findall(r"\w+", s.lower()) if len(w) > 3}


def same_album(t1, t2):
    """two titles name the same album: at least two shared words, or half the words of the
    shorter one; words match loosely ("Ballettmusik" = "Balletmusik")"""
    a, b = words(t1), words(t2)
    if not a or not b:
        return False
    shared = sum(1 for x in a if any(difflib.SequenceMatcher(None, x, y).ratio() >= 0.85 for y in b))
    return shared >= 2 or shared >= (min(len(a), len(b)) + 1) // 2


def main():
    ap = argparse.ArgumentParser()
    here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
    home = os.path.expanduser("~")
    ap.add_argument("--cache", default=os.path.join(here, "cache"))
    ap.add_argument("--out", default=os.path.join(here, "cache", "listened-import", "out"))
    ap.add_argument("--notes", default=os.path.join(home, "Sync", "AlbumsPlayed"))
    ap.add_argument("--live", default=os.environ.get("CLUTTER_LISTENED") or os.path.join(home, "Sync", "clutter", "listened"))
    ap.add_argument("--merge", action="store_true", help="merge the staged files into the live log")
    a = ap.parse_args()
    if a.merge:
        return merge(a)

    report = collections.defaultdict(list)
    report["list_counts"] = {}
    coll, t2d = read_collection(a.cache)
    raw = list_events(a.cache, coll, report)
    notes = read_notes(a.notes, report)
    live = read_live(a.live)
    # one play noted twice: same minute, same album (possibly two pressings; keep an owned one)
    by_minute = collections.defaultdict(list)
    for n in sorted(notes, key=lambda n: n["discogs"] not in coll):
        by_minute[n["when"].replace(second=0)].append(n)
    for ns in by_minute.values():
        for i, n in enumerate(ns):
            first = next((m for m in ns[:i] if not m["used"] and (
                (m["discogs"], m["tidal"], m["apple"]) == (n["discogs"], n["tidal"], n["apple"])
                or same_album(split_title(m["title"])[1], split_title(n["title"])[1]))), None)
            if first:
                n["used"] = True
                report["dup_notes"].append((first["file"], n["file"]))

    by_release = collections.defaultdict(list)
    for n in notes:
        for r in note_releases(n, t2d):
            by_release[r].append(n)

    # notes already covered by the live log
    for e in live:
        when = dt.datetime.fromisoformat(e["ts"]) if "T" in e["ts"] else None
        cands = [n for n in notes if not n["used"] and when and (
            (e.get("discogs") and e["discogs"] in note_releases(n, t2d)) or
            (e.get("tidal") and e["tidal"] == n["tidal"]))]
        n = best_note(cands, when) if when else None
        if n:
            n["used"] = True

    out = []
    # list events, oldest first so that repeated same-day plays take notes in order
    raw.sort(key=lambda x: (x[2] if isinstance(x[2], dt.datetime) else
                            dt.datetime.combine(x[2], dt.time(), LOCAL)))
    matched = []
    for via, rid, st, disp in raw:
        n = best_note([n for n in by_release.get(rid, []) if not n["used"]], st)
        if n:
            n["used"] = True
        matched.append([via, rid, st, disp, n])

    # second pass: the same play, recorded in the list under one pressing and in the
    # AlbumsPlayed note under another. Only a list play without a note of its own
    # takes such a note, so two owned pressings played the same day stay two plays.
    for m in matched:
        via, rid, st, disp, n = m
        if n:
            continue
        ltitle = coll.get(rid, {}).get("title") or split_title(disp, (" - ",))[1]
        cands = [x for x in notes if not x["used"] and x["discogs"] and x["discogs"] != rid
                 and same_album(ltitle, x["title"])]
        x = best_note(cands, st)
        if x:
            x["used"] = True
            m[4] = x
            report["other_pressing"].append((str(st), rid, ltitle, x["discogs"], x["file"]))

    for via, rid, st, disp, n in matched:
        c = coll.get(rid, {})
        if isinstance(st, dt.datetime):
            ts = iso(st)
            report["timed_from_list"].append(rid)
        elif n:
            ts = iso(n["when"])
            report["time_from_note"].append((rid, str(st), ts))
        else:
            ts = st.isoformat()
            report["date_only"].append((st.year, rid, ts, disp))
        artist, title = (c.get("artist"), c.get("title")) if c.get("title") else split_title(disp, (" - ",))
        out.append(event(ts=ts, src="discogs", discogs=rid, inst=c.get("inst"), tidal=c.get("tidal"),
                         apple=c.get("apple"), artist=artist, title=title, via=via))

    # notes nobody claimed
    for n in notes:
        if n["used"]:
            continue
        artist, title = split_title(n["title"])
        if n["discogs"] is None and (n["tidal"] or n["apple"]):
            src = "tidal" if n["tidal"] else "apple"
            out.append(event(ts=iso(n["when"]), src=src, tidal=n["tidal"], apple=n["apple"],
                             artist=artist, title=title, via="import:albumsplayed"))
            report["recovered"].append(n)
        elif n["discogs"] is None:
            report["note_no_id"].append(n)
        else:  # LATB 2026-11-01: import these too
            c = coll.get(n["discogs"], {})
            if c.get("title"):
                artist, title = c["artist"], c["title"]
            out.append(event(ts=iso(n["when"]), src="discogs", discogs=n["discogs"], inst=c.get("inst"),
                             tidal=n["tidal"] or c.get("tidal"), apple=n["apple"] or c.get("apple"),
                             artist=artist, title=title, via="import:albumsplayed"))
            report["note_only"].append(n)

    # dedupe on (album key, minute) for timed events; date-only repeats are kept (two plays a day)
    seen, final = set(), []
    for e in sorted(out, key=lambda e: e["ts"]):
        if "T" in e["ts"]:
            key = (e["src"], e.get("discogs") or e.get("tidal") or e.get("apple"), e["ts"][:16])
            if key in seen:
                report["dupes"].append(e)
                continue
            seen.add(key)
        final.append(e)

    os.makedirs(a.out, exist_ok=True)
    for fn in glob.glob(os.path.join(a.out, "*.jsonl")):
        os.remove(fn)
    by_year = collections.defaultdict(list)
    for e in final:
        by_year[e["ts"][:4]].append(e)
    for y, es in by_year.items():
        with open(os.path.join(a.out, f"{y}.jsonl"), "w", encoding="utf-8") as f:
            for e in es:
                f.write(json.dumps(e, ensure_ascii=False, separators=(",", ":")) + "\n")

    write_report(a, report, by_year, notes, live, coll, final)


def merge(a):
    import shutil
    staged = collections.defaultdict(list)
    for fn in glob.glob(os.path.join(a.out, "*.jsonl")):
        staged[os.path.basename(fn)] += [json.loads(l) for l in open(fn, encoding="utf-8") if l.strip()]
    if not staged:
        sys.exit(f"nothing staged in {a.out}; run without --merge first")
    stamp = dt.datetime.now().strftime("%Y%m%d-%H%M%S-%f")
    backup = os.path.join(os.path.dirname(a.out), f"live-backup-{stamp}")
    if os.path.isdir(a.live):
        shutil.copytree(a.live, backup)
        print(f"live log backed up to {backup}", file=sys.stderr)
    os.makedirs(a.live, exist_ok=True)
    years = set(staged) | {os.path.basename(f) for f in glob.glob(os.path.join(a.live, "*.jsonl"))}
    for y in sorted(years):
        fn = os.path.join(a.live, y)
        own, bad = [], 0
        if os.path.exists(fn):
            for l in open(fn, encoding="utf-8"):
                if not l.strip():
                    continue
                try:
                    e = json.loads(l)
                except json.JSONDecodeError:
                    bad += 1
                    continue
                if not e.get("via", "").startswith("import:"):
                    own.append(e)
        seen, merged = set(), []
        for e in sorted(own + staged.get(y, []), key=lambda e: e["ts"]):
            if "T" in e["ts"]:
                k = (e["src"], e.get("discogs") or e.get("tidal") or e.get("apple"), e["ts"][:16])
                if k in seen:
                    continue
                seen.add(k)
            merged.append(e)
        tmp = fn + ".tmp"
        with open(tmp, "w", encoding="utf-8") as f:
            for e in merged:
                f.write(json.dumps(e, ensure_ascii=False, separators=(",", ":")) + "\n")
        os.replace(tmp, fn)
        print(f"{y}: {len(own)} live + {len(staged.get(y, []))} staged -> {len(merged)}"
              + (f" ({bad} torn lines dropped)" if bad else ""), file=sys.stderr)


def write_report(a, report, by_year, notes, live, coll, final):
    L = []
    w = L.append
    w(f"# Listened import — reconciliation report\n")
    w(f"Generated {dt.datetime.now(LOCAL).isoformat(timespec='minutes')} by `scripts/import-listened.py`. "
      f"Staged output: `{a.out}/YYYY.jsonl`. Nothing was written to the live log `{a.live}`.\n")

    w("## Sources\n")
    w("| list | items | dates | plan (items / dates) |\n|---|---|---|---|")
    for y, (i, d) in sorted(report["list_counts"].items()):
        ei, ed = EXPECTED.get(y, ("", ""))
        flag = "" if (i, d) == (ei, ed) else " ⚠️"
        w(f"| {y} Listened | {i} | {d} | {ei} / {ed}{flag} |")
    w(f"\nAlbumsPlayed notes read: {len(notes)} ({sum(n['utc'] for n in notes)} stamped in UTC, "
      f"before {UTC_CUTOVER}, converted to America/Chicago). Live log events: {len(live)}.\n")

    w("## Result per year (by the year of the play itself)\n")
    w("| year | events | with time | date only | discogs-list | want | albumsplayed |\n|---|---|---|---|---|---|---|")
    for y in sorted(by_year):
        es = by_year[y]
        c = collections.Counter(e["via"] for e in es)
        timed = sum("T" in e["ts"] for e in es)
        w(f"| {y} | {len(es)} | {timed} | {len(es) - timed} | {c['import:discogs-list']} | "
          f"{c['import:want']} | {c['import:albumsplayed']} |")
    w(f"\nTotal: {len(final)} events. A list's dates can fall into another year (the 2020 list has 2019 dates).\n")

    w("## Time of day\n")
    w(f"- {len(report['time_from_note'])} date-only list plays got their time from a matching AlbumsPlayed note "
      f"(same release, same local or UTC day, else ±1 day).")
    w(f"- {len(report['timed_from_list'])} plays had a time already (2026 Want stamps).")
    dy = collections.Counter(y for y, *_ in report["date_only"])
    w(f"- {len(report['date_only'])} plays stay date-only (no matching note): "
      + ", ".join(f"{y}: {n}" for y, n in sorted(dy.items())) + ". Full list in `date-only.tsv`.")
    shifted = [(r, d, ts) for r, d, ts in report["time_from_note"] if ts[:10] != d]
    w(f"- {len(shifted)} of the timed plays land on a different day than the list date "
      f"(the list date was the UTC day, or the note is a day off). Listed in `day-shifted.tsv`.\n")

    w("## Recovered from AlbumsPlayed (no Discogs link, added as `import:albumsplayed`)\n")
    w("| when | src | id | title |\n|---|---|---|---|")
    for n in sorted(report["recovered"], key=lambda n: n["when"]):
        w(f"| {iso(n['when'])[:16]} | {'tidal' if n['tidal'] else 'apple'} | {n['tidal'] or n['apple']} | {n['title']} |")

    w(f"\n## In AlbumsPlayed but not in any Discogs list — imported as `import:albumsplayed` ({len(report['note_only'])})\n")
    w("These notes link a Discogs release, but no list date matches (LATB 2026-11-01: import them all). "
      "Full list in `note-only.tsv`.\n")
    ny = collections.Counter(n["when"].year for n in report["note_only"])
    w("Per year: " + ", ".join(f"{y}: {c}" for y, c in sorted(ny.items())) + "\n")
    w("| when | discogs | file |\n|---|---|---|")
    for n in sorted(report["note_only"], key=lambda n: n["when"])[:40]:
        w(f"| {iso(n['when'])[:16]} | {n['discogs']} | {n['file']} |")
    if len(report["note_only"]) > 40:
        w(f"| … | | {len(report['note_only']) - 40} more in `note-only.tsv` |")

    if report["note_no_id"]:
        w(f"\n## Notes with no album link at all ({len(report['note_no_id'])}) — NOT imported\n")
        for n in report["note_no_id"]:
            w(f"- {iso(n['when'])[:16]} {n['file']}")
    if report["other_pressing"]:
        w(f"\n## Same play, other pressing ({len(report['other_pressing'])}) — merged into the list play\n")
        w("The list recorded the play under one release, the AlbumsPlayed note under another pressing of the same "
          "album. The list's release is kept, with the note's time.\n")
        w("| day | list release | title | note's release | note |\n|---|---|---|---|---|")
        for d, rid, t, nid, f in report["other_pressing"]:
            w(f"| {d} | {rid} | {t} | {nid} | {f} |")
    if report["aliased"]:
        w(f"\n## Notes re-pointed to the owned pressing ({len(report['aliased'])})\n")
        for f, old, new in report["aliased"]:
            w(f"- {f}: {old} → {new}")
    if report["identified"]:
        w(f"\n## Unlinked notes identified by LATB ({len(report['identified'])})\n")
        for f, src, i in report["identified"]:
            w(f"- {f} → {src} {i}")
    if report["bad_lines"]:
        w(f"\n## Unparseable comment lines ({len(report['bad_lines'])}) — skipped\n")
        for where, line in report["bad_lines"]:
            w(f"- {where}: `{line}`")
    if report["typos"]:
        w(f"\n## Impossible years corrected to the list's year ({len(report['typos'])})\n")
        for where, line, fixed in report["typos"]:
            w(f"- {where}: `{line}` → {fixed}")
    if report["multi_lines"]:
        w(f"\n## Comment lines with several dates, each imported ({len(report['multi_lines'])})\n")
        for where, line in report["multi_lines"]:
            w(f"- {where}: `{line}`")
    if report["masters"]:
        w(f"\n## List items that are Discogs masters, not releases — NOT imported ({len(report['masters'])})\n")
        for where, disp, line in report["masters"]:
            w(f"- {where} {disp}: `{line}`")
    if report["dup_notes"]:
        w(f"\n## Duplicate AlbumsPlayed notes (same album, same minute), second one ignored ({len(report['dup_notes'])})\n")
        for f1, f2 in report["dup_notes"]:
            w(f"- {f2} (same as {f1})")
    if report["bad_notes"]:
        w(f"\n## Notes without a date ({len(report['bad_notes'])}) — skipped\n")
        for f in report["bad_notes"]:
            w(f"- {f}")
    if report["dupes"]:
        w(f"\n## Duplicates dropped (same album, same minute): {len(report['dupes'])}\n")

    # Discogs field 7 vs logged count
    cnt = collections.Counter(e["discogs"] for e in final if e.get("discogs"))
    over = [(rid, c["plays"], cnt.get(rid, 0)) for rid, c in coll.items() if c["plays"] > cnt.get(rid, 0)]
    w(f"\n## Discogs field 7 \"Plays\" higher than the logged count ({len(over)})\n")
    w("Plays before 2020, or the wrap at 6, would show up here. Full list in `plays-over.tsv`.\n")
    for rid, p, n in sorted(over, key=lambda x: x[1] - x[2], reverse=True)[:20]:
        w(f"- {rid} {coll[rid]['artist']} – {coll[rid]['title']}: field 7 = {p}, logged {n}")

    open(os.path.join(a.out, "report.md"), "w", encoding="utf-8").write("\n".join(L) + "\n")

    def tsv(name, rows):
        with open(os.path.join(a.out, name), "w", encoding="utf-8") as f:
            for r in rows:
                f.write("\t".join(str(x) for x in r) + "\n")
    tsv("date-only.tsv", sorted(report["date_only"]))
    tsv("day-shifted.tsv", shifted)
    tsv("note-only.tsv", [(iso(n["when"])[:16], n["discogs"], n["file"]) for n in sorted(report["note_only"], key=lambda n: n["when"])])
    tsv("plays-over.tsv", [(rid, p, n, coll[rid]["artist"], coll[rid]["title"]) for rid, p, n in over])

    print(f"{len(final)} events staged in {a.out}; report: {os.path.join(a.out, 'report.md')}", file=sys.stderr)


if __name__ == "__main__":
    main()

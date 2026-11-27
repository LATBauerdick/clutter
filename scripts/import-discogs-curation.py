#!/usr/bin/env python3
"""Seed clutter's curated tags and ratings from Discogs, once (docs/album-layer-plan.md › slice 5).

    scripts/import-discogs-curation.py                 # dry run: report to stdout
    scripts/import-discogs-curation.py --lines FILE    # also the lines, as JSON, to FILE
    scripts/import-discogs-curation.py --write         # copy the log to
                                                       # collection.jsonl.before-curation,
                                                       # then append

Reads cache/draw*.json (folders, ratings), cache/folders-raw.json, cache/lists-raw.json and
the lists' l<id>-raw.json, and the album layer of the collection log. Writes, against the
album's C id if it has one, else its version key:

    {"ts", "op": "tag", "album": "C…", "tag": "my.piano", "value": true, "via": "discogs-import", …}
    {"ts", "op": "rate", "album": "C…", "stars": 4, "via": "discogs-import", …}

Curated tags: the genre folders and the lists in FOLDER_TAGS / LIST_TAGS; an album gets
the tags of all its records (and of a stand-in aliased to it). Rating: the highest of its
records' (and stand-ins') ratings; disagreements are listed. With --write it also writes
curated-tags.json beside the log ($CLUTTER_CURATED_TAGS), the curated tags clutter offers
with their names, if it does not exist. The folders CD, CDR, Streaming, Files
give no tag. A one-off seed: refused once the log has a discogs-import line, so a tag
taken off in clutter is not put back. Discogs is not written to.

--write: stop clutter first (on every machine); the log must be in the album format.
"""

import argparse
import collections
import datetime
import glob
import json
import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import collection_log  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CACHE = os.path.join(ROOT, "cache")

FOLDER_TAGS = {
    "Pop": "my.pop",
    "Piano": "my.piano",
    "Symphonic": "my.symphonic",
    "Opera&Vocal": "my.opera",
    "Chamber": "my.chamber",
    "Concertos": "my.concertos",
}
LIST_TAGS = {
    "Petra": "my.petra",
    "Darko/Lavorgna": "my.darkolavorgna",  # LATB's my.darkoLavorgna; tags are lower case, written so
}
# the curated tags clutter offers, in the sidebar's order, with the names it shows; my.vocal
# is there unused (LATB 2026-11-26). Written beside the log once, if it does not exist.
VOCABULARY = [
    {"tag": "my.pop", "name": "Pop"},
    {"tag": "my.piano", "name": "Piano"},
    {"tag": "my.symphonic", "name": "Symphonic"},
    {"tag": "my.concertos", "name": "Concertos"},
    {"tag": "my.chamber", "name": "Chamber"},
    {"tag": "my.opera", "name": "Opera"},
    {"tag": "my.vocal", "name": "Vocal"},
    {"tag": "my.petra", "name": "Petra"},
    {"tag": "my.darkolavorgna", "name": "Darko/Lavorgna"},
]


def read_discogs(cache: str) -> tuple[dict, dict]:
    """D key -> {folder, rating, added, artist, title}; and list name -> [D keys]"""
    folders = {x["id"]: x["name"] for x in json.load(open(os.path.join(cache, "folders-raw.json")))["folders"]}
    rel: dict = {}
    for fn in sorted(glob.glob(os.path.join(cache, "draw*.json"))):
        for r in json.load(open(fn)).get("releases", []):
            b = r["basic_information"]
            k = f"D{r['id']}"
            e = rel.setdefault(k, {"folders": [], "rating": 0, "added": r.get("date_added") or "",
                                   "artist": ", ".join(a["name"] for a in b.get("artists", [])),
                                   "title": b.get("title", "")})
            e["folders"].append(folders.get(r.get("folder_id"), str(r.get("folder_id"))))
            e["rating"] = max(e["rating"], r.get("rating") or 0)
            e["added"] = min(e["added"], r.get("date_added") or e["added"])
    lists: dict = {}
    for l in json.load(open(os.path.join(cache, "lists-raw.json"))).get("lists", []):
        if l["name"] in LIST_TAGS:
            items = json.load(open(os.path.join(cache, f"l{l['id']}-raw.json"))).get("items", [])
            lists[l["name"]] = [f"D{i['id']}" for i in items if i.get("type") == "release"]
    return rel, lists


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--log", default=collection_log.log_path(), help="the collection log (CLUTTER_COLLECTION)")
    ap.add_argument("--cache", default=CACHE, help="clutter's cache/ (Discogs collection pages and lists)")
    ap.add_argument("--lines", metavar="FILE", help="write the lines --write would append to FILE")
    ap.add_argument("--write", action="store_true", help="append the lines to the log (after a copy)")
    ap.add_argument("--now", help="timestamp for the lines (default: now)")
    args = ap.parse_args()

    stat0 = os.stat(args.log)  # before the first read: a line written after it is noticed
    lines_in = collection_log._lines(args.log)
    if not any(e.get("op") == "format" for e in lines_in):
        sys.exit(f"{args.log} is not in the album format: run album-layer-migrate.py --write first")
    if any(e.get("via") == "discogs-import" for e in lines_in):
        sys.exit(f"{args.log} has discogs-import lines already: the seed ran (a one-off)")
    # the curated tags file beside the log (the live log: where the server reads it), so a
    # test run on a copy does not create the live one
    curated = os.environ.get("CLUTTER_CURATED_TAGS") or os.path.join(os.path.dirname(os.path.abspath(args.log)), "curated-tags.json")
    al = collection_log.album_log(args.log)
    rel, lists = read_discogs(args.cache)
    now = args.now or datetime.datetime.now().astimezone().isoformat(timespec="seconds")

    def name(k: str) -> str:
        return al.album_of(k)

    def label(k: str) -> str:
        r = rel.get(k) or {}
        return f"{r.get('artist') or '?'} — {r.get('title') or '?'}"

    # the albums' records (and stand-ins aliased into them), each album once
    albums: dict = collections.defaultdict(list)
    for k in rel:
        albums[name(k)].append(k)

    # ---------------------------------------------------------------- tags
    tags: dict = collections.defaultdict(set)  # album -> my.* tags
    per_tag_records = collections.Counter()
    no_album = collections.Counter()
    for k, r in rel.items():
        for f in r["folders"]:
            if f in FOLDER_TAGS:
                tags[name(k)].add(FOLDER_TAGS[f])
                per_tag_records[FOLDER_TAGS[f]] += 1
    for lname, keys in lists.items():
        t = LIST_TAGS[lname]
        for k in keys:
            if k not in rel and al.album_of(k) == collection_log.parse_key(k):
                no_album[lname] += 1  # in the list, not in the collection nor in the log
                continue
            tags[name(k)].add(t)
            per_tag_records[t] += 1
    multi = {a: ts for a, ts in tags.items() if len(ts & set(FOLDER_TAGS.values())) > 1}

    # ---------------------------------------------------------------- ratings
    added = {k: r["added"] for k, r in rel.items()}
    rates: dict = {}
    disagree = []
    for a, ks in albums.items():
        rated = {k: rel[k]["rating"] for k in ks if rel[k]["rating"]}
        if not rated:
            continue
        # records that disagree: the higher rating (LATB 2026-11-26: the considered verdict)
        best = max(rated, key=lambda k: rated[k])
        rates[a] = rated[best]
        if len(set(rated.values())) > 1:
            disagree.append((a, best, rated))

    # ---------------------------------------------------------------- lines
    def head(a: str) -> dict:
        ks = albums.get(a) or [a]
        return {"artist": (rel.get(ks[0]) or {}).get("artist", ""), "title": (rel.get(ks[0]) or {}).get("title", "")}

    out_lines = []
    for a in sorted(tags):
        for t in sorted(tags[a]):
            out_lines.append({"ts": now, "op": "tag", "album": a, "tag": t, "value": True, "via": "discogs-import", **head(a)})
    for a in sorted(rates):
        out_lines.append({"ts": now, "op": "rate", "album": a, "stars": rates[a], "via": "discogs-import", **head(a)})

    # ---------------------------------------------------------------- report
    p = print
    p(f"# Curation import — {'write' if args.write else 'dry run'}, {now}")
    p("")
    p(f"Discogs: {len(rel)} releases in {len(albums)} albums of the log; lists " + ", ".join(f"{n} {len(v)}" for n, v in lists.items()))
    p("")
    p("## Curated tags")
    p("| tag | from | records | albums |")
    p("|---|---|---|---|")
    src = {**{v: f"folder {k}" for k, v in FOLDER_TAGS.items()}, **{v: f"list {k}" for k, v in LIST_TAGS.items()}}
    per_tag_albums = collections.Counter(t for ts in tags.values() for t in ts)
    for t in list(FOLDER_TAGS.values()) + list(LIST_TAGS.values()):
        p(f"| `{t}` | {src[t]} | {per_tag_records[t]} | {per_tag_albums[t]} |")
    p(f"\n{sum(per_tag_records.values())} record tags → {sum(per_tag_albums.values())} album tags on {len(tags)} albums "
      f"(plan: 1,313 + 48 less albums joined).")
    if no_album:
        p("List items neither in the collection nor in the log (left out): " + ", ".join(f"{n} {c}" for n, c in no_album.items()))
    p(f"\n### Albums with two genre tags ({len(multi)})")
    for a, ts in sorted(multi.items(), key=lambda x: label(albums[x[0]][0])):
        p(f"- {label(albums[a][0])} ({a}): " + ", ".join(sorted(ts)))
    p("")
    p("## Ratings")
    dist = collections.Counter(rates.values())
    p(f"{len(rates)} albums rated: " + ", ".join(f"{s}★ {dist[s]}" for s in sorted(dist, reverse=True))
      + f" (Discogs: {sum(1 for r in rel.values() if r['rating'])} releases rated; plan: 410)")
    p(f"\n### Albums whose records disagree ({len(disagree)}) — the higher is taken")
    for a, first, rated in disagree:
        p(f"- {label(first)} ({a}): " + ", ".join(f"{k} {s}★" + (" ←" if k == first else "") for k, s in rated.items()))
    p("")
    p("## Lines --write would append")
    for op, c in sorted(collections.Counter(l["op"] for l in out_lines).items()):
        p(f"- {op}: {c}")
    p(f"- total: {len(out_lines)}")

    if args.lines:
        with open(args.lines, "w") as f:
            for l in out_lines:
                f.write(json.dumps(l, ensure_ascii=False) + "\n")
    if args.write:
        backup = args.log + ".before-curation"
        if os.path.exists(backup):
            sys.exit(f"{backup} exists: not overwriting it")
        st = os.stat(args.log)
        if (st.st_size, st.st_mtime_ns) != (stat0.st_size, stat0.st_mtime_ns):
            sys.exit(f"{args.log} changed while this ran: stop clutter (on every machine) and run again")
        shutil.copy2(args.log, backup)
        try:
            collection_log.append_lines(args.log, out_lines)
        except RuntimeError as e:
            sys.exit(f"{e}: stop clutter, restore the log from {backup}, delete the copy, run again")
        print(f"\nappended {len(out_lines)} lines to {args.log} (copy: {backup})", file=sys.stderr)
        if not os.path.exists(curated):
            with open(curated, "w") as f:
                json.dump(VOCABULARY, f, ensure_ascii=False, indent=1)
                f.write("\n")
            print(f"wrote {curated} ({len(VOCABULARY)} curated tags)", file=sys.stderr)


if __name__ == "__main__":
    main()

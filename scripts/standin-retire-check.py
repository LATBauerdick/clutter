#!/usr/bin/env python3
"""What would be lost if the Discogs folders Streaming and Files were deleted (album layer,
slice 6: docs/album-layer-plan.md › Slices). Read-only.

    scripts/standin-retire-check.py [--log FILE] [--cache DIR]

A stand-in is a release in one of those folders (CLUTTER_STANDIN_FOLDERS); the migration
gave each that was a version of something else an alias to it. With the folders gone the
stand-ins are no longer held. For each stand-in this lists:

- its album with the stand-in no longer held: which versions clutter still holds; an
  album with none left would vanish from clutter (and its plays, marks, tags, rating with
  it), unless what it stands for (a Tidal album that is no favourite …) is taken in;
- a stand-in with no alias: it is its own album and would vanish;
- the plays whose keys resolve to no held version once the stand-ins are gone;
- keep marks, tags and ratings that name a stand-in's album that would vanish.

The answer for slice 6 is "nothing lost": then LATB can delete the folders at Discogs.
"""

import argparse
import collections
import glob
import json
import os
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import collection_log  # noqa: E402

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HOME = os.path.expanduser("~")
TIDAL_STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
JELLYFIN_STORE = os.environ.get("CLUTTER_JELLYFIN_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "jellyfin-albums.json")
LISTENED = os.environ.get("CLUTTER_LISTENED") or os.path.join(HOME, "Sync", "clutter", "listened")
STANDIN = [f.strip() for f in os.environ.get("CLUTTER_STANDIN_FOLDERS", "Streaming,Files").split(",") if f.strip()]


def discogs(cache: str) -> dict:
    folders = {x["id"]: x["name"] for x in json.load(open(os.path.join(cache, "folders-raw.json")))["folders"]}
    out = {}
    for p in sorted(glob.glob(os.path.join(cache, "draw*.json"))):
        for r in json.load(open(p))["releases"]:
            b = r["basic_information"]
            out["D" + str(r["id"])] = {"folder": folders.get(r.get("folder_id"), ""),
                                       "artist": ", ".join(a["name"] for a in b.get("artists", [])),
                                       "title": b.get("title", "")}
    return out


def tidal(cache: str) -> dict:
    store = json.load(open(TIDAL_STORE))["albums"]
    # as the server reads the store: not removed, and with the album's data (an entry Tidal
    # answered 404 for has none, and is no album)
    out = {"T" + t: e for t, e in store.items() if not e.get("dropped") and e.get("album")}
    try:
        for it in json.load(open(os.path.join(cache, "traw.json"))).get("items", []):
            k = "T" + str((it.get("item") or {}).get("id", ""))
            if k != "T" and k not in out and not (store.get(k[1:]) or {}).get("dropped"):
                out[k] = {"album": it.get("item")}
    except (OSError, json.JSONDecodeError):
        pass
    return out


def plays() -> list:
    out = []
    for fn in sorted(glob.glob(os.path.join(LISTENED, "*.jsonl"))):
        for line in open(fn, encoding="utf-8"):
            try:
                e = json.loads(line)
            except json.JSONDecodeError:
                continue
            if isinstance(e, dict):
                out.append(e)
    return out


def play_keys(e: dict) -> list:
    """the play's keys: the one its src names first, then discogs, tidal, apple, jellyfin"""
    ks = {"discogs": f"D{e['discogs']}" if e.get("discogs") else None,
          "tidal": f"T{e['tidal']}" if e.get("tidal") else None,
          "apple": f"A{e['apple']}" if e.get("apple") else None,
          "jellyfin": f"J{str(e['jellyfin']).lower()}" if e.get("jellyfin") else None}
    order = [e.get("src")] + ["discogs", "tidal", "apple", "jellyfin"]
    return [ks[s] for s in dict.fromkeys(order) if s in ks and ks[s]]


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--log", default=collection_log.log_path())
    ap.add_argument("--cache", default=os.path.join(ROOT, "cache"))
    ap.add_argument("--tidal-out", metavar="FILE",
                    help="write the un-held Tidal albums stand-ins stand for, one per line, for update-tidal-albums.py --take")
    args = ap.parse_args()

    al = collection_log.album_log(args.log)
    if not al.format:
        sys.exit(f"{args.log} is not in the album format")
    disc = discogs(args.cache)
    tid = tidal(args.cache)
    jel = {"J" + j.lower() for j in json.load(open(JELLYFIN_STORE))["albums"]}
    standins = {k for k, r in disc.items() if r["folder"] in STANDIN}
    held_now = set(disc) | set(tid) | jel
    held_after = held_now - standins

    def name(k: str) -> str:
        r = disc.get(k) or {}
        return f"{r.get('artist') or '?'} — {r.get('title') or '?'}"

    # each stand-in: its album, and what of it clutter still holds without the stand-ins
    vanish: dict = {}  # album -> (stand-in, what it stands for)
    unaliased = []
    kinds = collections.Counter()
    for s in sorted(standins):
        a = al.album_of(s)
        target = al.canonical(s)
        if target == s:
            unaliased.append(s)
            vanish[a] = (s, None)
            continue
        vs = al.versions(a)
        if not any(v in held_after for v in vs):
            vanish[a] = (s, target)
            kinds[target[0]] += 1

    # plays that resolve to a held version now but to none once the stand-ins are gone
    lost_plays = []
    ps = plays()
    for e in ps:
        ks = play_keys(e)
        now = next((al.canonical(k) for k in ks if al.canonical(k) in held_now), None)
        after = next((al.canonical(k) for k in ks if al.canonical(k) in held_after), None)
        if now and not after:
            lost_plays.append((e, now))

    # marks, tags, ratings on an album that would vanish
    marked = {a: v for a, v in al.keep.items() if a in vanish}
    tagged = {a: ts for a, ts in al.tags.items() if a in vanish}
    rated = {a: s for a, s in al.rate.items() if a in vanish}

    if args.tidal_out:
        with open(args.tidal_out, "w") as f:
            f.write("".join(t + "\n" for _, t in sorted(vanish.values(), key=lambda x: x[0]) if t and t[0] == "T"))
    p = print
    p(f"# Stand-ins: what deleting the Discogs folders {', '.join(STANDIN)} would lose")
    p("")
    p(f"{len(standins)} stand-in releases; {len(standins) - len(unaliased)} aliased to what they stand for, {len(unaliased)} not aliased.")
    p(f"Albums that would have no held version left: {len(vanish)} "
      f"({len(unaliased)} un-aliased stand-ins; aliased to an un-held " + ", ".join(f"{k} {c}" for k, c in sorted(kinds.items())) + ")")
    p(f"Plays that would no longer resolve: {len(lost_plays)} (of {len(ps)})")
    p(f"Keep marks on such albums: {len(marked)}; tagged: {len(tagged)}; rated: {len(rated)}")
    p("")
    p("## Albums that would vanish")
    for a, (s, t) in sorted(vanish.items(), key=lambda x: name(x[1][0])):
        why = "no alias" if t is None else f"stands for {t} (not held)"
        extra = []
        if a in marked:
            extra.append("keep " + ("yes" if marked[a] else "no"))
        if a in tagged:
            extra.append(",".join(sorted(tagged[a])))
        if a in rated:
            extra.append(f"{rated[a]}★")
        np = sum(1 for _, n in lost_plays if al.album_of(n) == a)
        if np:
            extra.append(f"{np} plays")
        p(f"- {name(s)} ({s}, {disc[s]['folder']}): {why}" + (f" — {'; '.join(extra)}" if extra else ""))
    if lost_plays:
        p("")
        p("## Plays that would be lost")
        for e, n in lost_plays[:40]:
            p(f"- {e.get('ts', '')[:10]} {e.get('artist', '')} — {e.get('title', '')} (now {n})")
        if len(lost_plays) > 40:
            p(f"- … and {len(lost_plays) - 40} more")


if __name__ == "__main__":
    main()

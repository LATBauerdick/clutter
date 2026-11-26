#!/usr/bin/env python3
"""Compare two clutter servers list by list: the check of the album layer's slice 2
(docs/album-layer-plan.md › Slices).

  python3 test/album-layer-compare.py --old http://localhost:8086 --new http://localhost:8085 \\
      --log /path/to/migrated/collection.jsonl [--versions] [--focus format.vinyl ...]

Every key of both servers is read as the album it belongs to in the migrated log (--log;
collection_log.album_log). Then, list by list:

- a shelf list (its rows are records): the same keys in the same order;
- a list of plays (Listened …): the same albums, as often;
- every other list: the same albums, and on the new server each album once. Where the old
  server had two or more rows for one album, that is a join of the migration (an album
  with an id); anything else is a difference and is printed.

Plays: for every album in "All", the new server's count equals the sum over the old
server's rows of that album. With --versions, every album with an id that the new server
shows: its page's Versions (the row, xversions) are the log's versions. With --focus, the
same comparison of "All" and "Collection" under each focus (#tag). Exit 1 on a difference.
"""

import argparse
import collections
import json
import os
import sys
import urllib.parse
import urllib.request

sys.path.insert(0, os.path.join(os.path.dirname(os.path.abspath(__file__)), "..", "scripts"))
import collection_log  # noqa: E402

SHELVES = ("Basement", "Box", "Cube", "Shelf", "Incoming", "Lost&Found")


def get(base: str, path: str):
    with urllib.request.urlopen(base + path, timeout=300) as r:
        return json.load(r)


def rows(base: str, name: str, focus=()) -> list:
    q = urllib.parse.quote(name, safe="")
    qs = "&".join("focus=" + urllib.parse.quote("#" + f, safe="") for f in focus)
    j = get(base, f"/api/albumsq/{q}" + (f"?{qs}" if qs else ""))
    return [(a[0]["albumID"], len(a[0].get("albumListenedDates") or [])) for a in j["lalbums"]]


def lists(base: str) -> list:
    p = get(base, "/api/paramsq/all")["params"]
    return list(dict.fromkeys(p["mlistNames"] + p["mlocNames"]))


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--old", required=True)
    ap.add_argument("--new", required=True)
    ap.add_argument("--log", required=True, help="the migrated collection log")
    ap.add_argument("--versions", action="store_true")
    ap.add_argument("--tags", action="store_true", help="album row vs shelf row (needs the old server for a record's own tags)")
    ap.add_argument("--focus", nargs="*", default=[])
    ap.add_argument("--show", type=int, default=8, help="examples per difference")
    a = ap.parse_args()

    al = collection_log.album_log(a.log)
    album = al.album_of
    bad = 0
    total_joined = 0

    def compare(label: str, old: list, new: list, shelf: bool, plays: bool) -> None:
        nonlocal bad, total_joined
        ok, joined = keyed_compare(label, old, new, shelf, plays)
        total_joined += joined
        if not ok:
            bad += 1

    def keyed_compare(label, old, new, shelf, plays):
        ok = True
        ko = [k for k, _ in old]
        kn = [k for k, _ in new]
        if shelf:
            if ko != kn:
                print(f"✗ {label}: shelf rows differ ({len(ko)} old, {len(kn)} new); first: "
                      f"{next(((x, y) for x, y in zip(ko, kn) if x != y), None)}")
                return False, 0
            return True, 0
        co = collections.Counter(album(k) for k in ko)
        cn = collections.Counter(album(k) for k in kn)
        if plays:
            if co != cn:
                print(f"✗ {label}: plays differ: {sum((co - cn).values())} rows only old, {sum((cn - co).values())} only new; "
                      f"e.g. {list((co - cn).items())[:a.show]} / {list((cn - co).items())[:a.show]}")
                return False, 0
            return True, 0
        only_old = set(co) - set(cn)
        only_new = set(cn) - set(co)
        twice_new = [n for n, c in cn.items() if c > 1]
        joined = {n: c for n, c in co.items() if c > 1}
        not_ids = [n for n in joined if not n.startswith("C")]
        for what, xs in (("only on the old server", only_old), ("only on the new server", only_new),
                         ("twice on the new server", twice_new), ("old rows joined without an album id", not_ids)):
            if xs:
                ok = False
                ex = sorted(xs)[: a.show]
                keys = {n: [k for k in (ko if what != "only on the new server" else kn) if album(k) == n] for n in ex}
                print(f"✗ {label}: {len(xs)} albums {what}: {keys}")
        n_joined = sum(c - 1 for c in joined.values())
        if ok:
            print(f"✓ {label}: {len(cn)} albums" + (f" ({n_joined} old rows joined into {len(joined)} albums)" if joined else ""))
        return ok, n_joined

    names = lists(a.new)
    old_names = set(lists(a.old))
    for n in names:
        if n not in old_names:
            print(f"✗ {n}: not a list on the old server")
            bad += 1
            continue
        shelf = n.split(" ")[0] in SHELVES
        plays = n.startswith("Listened")
        compare(n, rows(a.old, n), rows(a.new, n), shelf, plays)
    for n in sorted(old_names - set(names)):
        print(f"✗ {n}: not a list on the new server")
        bad += 1

    # plays per album: the new row's count = the old rows' sum
    old_all = rows(a.old, "All")
    new_all = rows(a.new, "All")
    summed = collections.Counter()
    for k, c in old_all:
        summed[album(k)] += c
    got = {album(k): c for k, c in new_all}
    diff = {n: (summed[n], got.get(n)) for n in set(summed) | set(got) if summed[n] != got.get(n, 0)}
    if diff:
        bad += 1
        print(f"✗ plays per album: {len(diff)} albums differ (old sum, new): {sorted(diff.items())[: a.show]}")
    else:
        print(f"✓ plays per album: {sum(got.values())} plays on {sum(1 for c in got.values() if c)} albums, the same on both")

    for f in a.focus:
        for n in ("All", "Collection"):
            compare(f"{n} #{f}", rows(a.old, n, [f]), rows(a.new, n, [f]), False, False)

    if a.versions:
        ids = al.albums_with_ids()
        row_of = {album(k): k for k, _ in new_all}
        wrong = []
        checked = 0
        for name in ids:
            if name not in row_of:
                continue
            r = row_of[name]
            x = get(a.new, "/api/albumxq/" + r)
            shown = {r} | {v["cvKey"] for v in x.get("xversions") or []}
            # an album none of whose versions clutter holds is shown by a stand-in (its row)
            want = set(al.versions(name)) | ({r} if al.canonical(r) != r else set())
            checked += 1
            if shown != want:
                wrong.append((name, r, sorted(want - shown), sorted(shown - want)))
        if wrong:
            bad += 1
            print(f"✗ Versions: {len(wrong)} of {checked} albums differ (album, row, missing, extra): {wrong[: a.show]}")
        else:
            print(f"✓ Versions: all {checked} albums with an id that the new server shows list every version")

    if a.tags:
        # docs/album-layer-plan.md › Tags: an album's row matches the union of its versions'
        # tags; a record's row in a shelf list its own (and the album-level ones)
        shelves = {}
        for n in lists(a.new):
            if n.split(" ")[0] in SHELVES:
                for k, _ in rows(a.new, n):
                    shelves.setdefault(k, n)
        own = lambda k: set(get(a.old, "/api/albumq/" + k)["album"]["albumTags"])  # noqa: E731
        row_of = {album(k): k for k, _ in new_all}
        checked, wrong = 0, []
        for name in al.albums_with_ids():
            ds = [k for k in al.versions(name) if k.startswith("D")]
            r = row_of.get(name)
            if len(ds) < 2 or r not in ds:
                continue
            for d in ds:
                if d == r:
                    continue
                only = sorted(t for t in own(d) - own(r) if t.startswith("format."))
                if not only:
                    continue
                f = only[0]
                checked += 1
                if r not in {k for k, _ in rows(a.new, "All", [f])}:
                    wrong.append((name, f"All #{f} lacks the album row {r}"))
                if r in shelves and r in {k for k, _ in rows(a.new, shelves[r], [f])}:
                    wrong.append((name, f"{shelves[r]} #{f} shows {r}, whose own format it is not"))
                if d in shelves and d not in {k for k, _ in rows(a.new, shelves[d], [f])}:
                    wrong.append((name, f"{shelves[d]} #{f} lacks {d}"))
                break
            if checked >= a.show * 3:
                break
        if wrong or not checked:
            bad += 1
            print(f"✗ Tags: {len(wrong)} of {checked} albums with two records wrong: {wrong[: a.show]}")
        else:
            print(f"✓ Tags: {checked} albums with two records of different formats: the album row matches either format, a shelf row only its own")

    print(f"\n{total_joined} old rows joined in all lists; {'no difference' if not bad else f'{bad} checks failed'}")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())

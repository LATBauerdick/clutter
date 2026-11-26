#!/usr/bin/env python3
"""Move clutter's collection log to the album layer (docs/album-layer-plan.md › Migration).

Dry run by default: reads the log as today's server does, forms the albums, and prints a
report plus the lines --write would append. Writes nothing.

    scripts/album-layer-migrate.py                 # report to stdout
    scripts/album-layer-migrate.py --lines FILE    # also the lines, as JSON, to FILE
    scripts/album-layer-migrate.py --write         # copy the log to
                                                   # collection.jsonl.before-album-layer,
                                                   # then append (not before slice 2)

Steps (plan › Migration):
1. the log as the server reads it: stream links (one of a kind per holder, last line
   counts) and file links, every file album under the id it has now (moved lines);
2. albums = the connected groups over those links, over the keys clutter holds (Discogs
   releases in the collection, Tidal albums in the store or the favourites, file albums
   in the Jellyfin store) and the Apple Music and Qobuz albums linked to them;
3. a stand-in release (Discogs folders Streaming, Files; CLUTTER_STANDIN_FOLDERS) is no
   version: it gets an alias to the lead of the rest of its group; a stand-in with nothing
   else clutter holds in its group stays as it is (listed);
4. releases of one Discogs master in different groups are joined (listed);
5. each album of two or more keys gets an id and its version lines; then one format line.

The id of an album is "C" + 6 characters of 0-9a-hjkmnp-tv-z. The plan says random; here
it is taken from a hash of the album's keys, so that a second run gives the same ids and
the same report (and a collision is resolved by hashing again).
"""

import argparse
import collections
import datetime as dt
import glob
import hashlib
import json
import os
import shutil
import sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
import collection_log  # noqa: E402

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
HOME = os.path.expanduser("~")
CACHE = os.path.join(HERE, "cache")
TIDAL_STORE = os.environ.get("CLUTTER_TIDAL_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "tidal-albums.json")
JELLYFIN_STORE = os.environ.get("CLUTTER_JELLYFIN_ALBUMS") or os.path.join(HOME, "Sync", "clutter", "jellyfin-albums.json")
LISTENED = os.environ.get("CLUTTER_LISTENED") or os.path.join(HOME, "Sync", "clutter", "listened")
STANDIN_FOLDERS = [f.strip().casefold() for f in os.environ.get("CLUTTER_STANDIN_FOLDERS", "Streaming,Files").split(",") if f.strip()]

ALPHABET = "0123456789abcdefghjkmnpqrstvwxyz"  # 0-9a-hjkmnp-tv-z: 32, no i l o u
KIND = {"D": "record", "T": "tidal", "J": "files", "A": "apple", "Q": "qobuz"}
KIND_ORDER = "DTJAQ"  # the lead rule: record, Tidal, file album, Apple Music, Qobuz
SRC_KEY = {"discogs": "D", "tidal": "T", "apple": "A", "jellyfin": "J"}


# ---------------------------------------------------------------- what clutter holds


def read_discogs(cache: str) -> dict:
    """D<id> -> {artist, title, added, master, folder, standin}"""
    folders = {}
    try:
        with open(os.path.join(cache, "folders-raw.json")) as f:
            folders = {x["id"]: x["name"] for x in json.load(f)["folders"]}
    except (OSError, KeyError, json.JSONDecodeError) as e:
        sys.exit(f"cannot read the Discogs folders in {cache}: {e}")
    out = {}
    pages = sorted(glob.glob(os.path.join(cache, "draw*.json")))
    if not pages:
        sys.exit(f"no Discogs collection pages (draw*.json) in {cache}")
    for p in pages:
        with open(p) as f:
            for r in json.load(f)["releases"]:
                b = r["basic_information"]
                folder = folders.get(r.get("folder_id"), str(r.get("folder_id")))
                out["D" + str(r["id"])] = {
                    "artist": ", ".join(a["name"] for a in b.get("artists", [])),
                    "title": b.get("title", ""),
                    "added": r.get("date_added", ""),
                    "master": b.get("master_id") or 0,
                    "folder": folder,
                    "standin": folder.casefold() in STANDIN_FOLDERS,
                }
    return out


def read_tidal() -> dict:
    """T<id> -> {artist, title}: the album store less the dropped ones, and the favourites"""
    out = {}
    try:
        with open(TIDAL_STORE) as f:
            store = json.load(f)["albums"]
    except (OSError, KeyError, json.JSONDecodeError) as e:
        sys.exit(f"cannot read the Tidal album store {TIDAL_STORE}: {e}")
    for tid, e in store.items():
        if e.get("dropped"):
            continue
        a = e.get("album") or {}
        out["T" + str(tid)] = {"artist": (a.get("artist") or {}).get("name", ""), "title": a.get("title", "")}
    try:
        with open(os.path.join(CACHE, "traw.json")) as f:
            for it in json.load(f).get("items", []):
                a = it.get("item") or {}
                k = "T" + str(a.get("id", ""))
                if k != "T" and k not in out and not (store.get(k[1:]) or {}).get("dropped"):
                    out[k] = {"artist": (a.get("artist") or {}).get("name", ""), "title": a.get("title", "")}
    except (OSError, json.JSONDecodeError):
        pass
    return out


def read_jellyfin() -> dict:
    """J<id> -> {artist, title, path}"""
    try:
        with open(JELLYFIN_STORE) as f:
            albums = json.load(f)["albums"]
    except (OSError, KeyError, json.JSONDecodeError) as e:
        sys.exit(f"cannot read the Jellyfin album store {JELLYFIN_STORE}: {e}")
    return {"J" + j.lower(): {"artist": a.get("artist", ""), "title": a.get("title", ""), "path": a.get("path", "")} for j, a in albums.items()}


# ---------------------------------------------------------------- the log


def read_log(path: str) -> tuple:
    """edges (key, key) as the log leaves them, the line number where each key was first
    linked, the names the log gives keys, the keep marks, and existing C ids"""
    log = collection_log.read(path)
    mv = collection_log.moves(path)
    edges = []
    for holder, st in log["streams"].items():
        for kind, vid in st.items():
            edges.append((holder, kind + vid))
    for jid, key in log["files"].items():
        edges.append(("J" + jid, key))
    first, names, marks, cids, has_format = {}, {}, {}, set(), False
    for n, e in enumerate(collection_log._lines(path)):
        op = e.get("op")
        if op == "format":
            has_format = True
        for f in ("key", "version", "album", "from", "into", "to"):
            v = e.get(f)
            if isinstance(v, str) and v[:1] == "C":
                cids.add(v)
        keys = [norm(e.get(f), mv) for f in ("key", "version")]
        if op == "link":
            for k in keys:
                if k:
                    first.setdefault(k, n)
                    if e.get("artist") or e.get("title"):
                        names.setdefault(k, {"artist": e.get("artist", ""), "title": e.get("title", "")})
        elif op == "keep" and keys[0]:
            marks[keys[0]] = e.get("value")
    marks = {k: v for k, v in marks.items() if v in ("yes", "no")}
    return edges, first, names, marks, cids, has_format


def norm(k, mv) -> str:
    if not isinstance(k, str) or not k:
        return ""
    if k[:1] == "J" and len(k) == 33:
        return "J" + collection_log.follow(mv, k[1:].lower())
    if k.isdigit():
        return "D" + k
    return k


# ---------------------------------------------------------------- groups


class DSU:
    def __init__(self):
        self.p = {}

    def find(self, x):
        self.p.setdefault(x, x)
        while self.p[x] != x:
            self.p[x] = self.p[self.p[x]]
            x = self.p[x]
        return x

    def union(self, a, b):
        ra, rb = self.find(a), self.find(b)
        if ra != rb:
            self.p[max(ra, rb)] = min(ra, rb)


def groups_of(keys, edges) -> list:
    d = DSU()
    for k in keys:
        d.find(k)
    for a, b in edges:
        if a in keys and b in keys:
            d.union(a, b)
    g = collections.defaultdict(set)
    for k in keys:
        g[d.find(k)].add(k)
    return sorted((frozenset(s) for s in g.values()), key=lambda s: sorted(s))


def lead_of(keys, disc, first) -> str:
    def rank(k):
        kind = KIND_ORDER.index(k[0]) if k[0] in KIND_ORDER else 9
        if k[0] == "D":
            when = disc.get(k, {}).get("added", "")
        else:
            when = "%09d" % first.get(k, 10**8)
        return (kind, when, k)

    return min(keys, key=rank)


def mint(keys, taken) -> str:
    seed = "|".join(sorted(keys))
    n = 0
    while True:
        h = int.from_bytes(hashlib.sha256(f"{seed}#{n}".encode()).digest()[:8], "big")
        cid = "C" + "".join(ALPHABET[(h >> (5 * i)) & 31] for i in range(6))
        if cid not in taken:
            taken.add(cid)
            return cid
        n += 1


def shape(keys) -> str:
    c = collections.Counter(k[0] for k in keys)
    return " + ".join(f"{KIND[k]}" + (f"×{c[k]}" if c[k] > 1 else "") for k in KIND_ORDER if c[k])


# ---------------------------------------------------------------- plays


def play_keys():
    """for each play: the key its src names, and the other ids on the line in the plan's order"""
    for p in sorted(glob.glob(os.path.join(LISTENED, "*.jsonl"))):
        with open(p) as f:
            for line in f:
                try:
                    e = json.loads(line)
                except json.JSONDecodeError:
                    continue
                ks = []
                s = SRC_KEY.get(e.get("src"))
                if s and e.get(e["src"]):
                    ks.append(s + str(e[e["src"]]))
                for src in ("discogs", "tidal", "apple", "jellyfin"):
                    if e.get(src):
                        k = SRC_KEY[src] + str(e[src])
                        if k not in ks:
                            ks.append(k)
                yield e, ks


# ---------------------------------------------------------------- main


def main() -> None:
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--log", default=collection_log.log_path(), help="the collection log (CLUTTER_COLLECTION)")
    ap.add_argument("--cache", default=CACHE, help="clutter's cache/ (Discogs collection pages)")
    ap.add_argument("--lines", metavar="FILE", help="write the lines --write would append to FILE")
    ap.add_argument("--write", action="store_true", help="append the lines to the log (after a copy)")
    ap.add_argument("--dry-run", action="store_true", help="the default: write nothing")
    ap.add_argument("--now", help="timestamp for the lines (default: now); fixed for a reproducible run")
    args = ap.parse_args()
    if args.write and args.dry_run:
        sys.exit("--write and --dry-run")

    disc = read_discogs(args.cache)
    tidal = read_tidal()
    jelly = read_jellyfin()
    held = set(disc) | set(tidal) | set(jelly)
    if any(e.get("op") == "format" for e in collection_log._lines(args.log)):
        sys.exit(f"{args.log} already has a format line: migrated before")
    edges, first, lognames, marks, cids, _ = read_log(args.log)

    def name(k):
        n = disc.get(k) or tidal.get(k) or jelly.get(k) or lognames.get(k) or {}
        return f"{n.get('artist') or '?'} — {n.get('title') or '?'}"

    out = []
    p = out.append

    # step 1–2, as the log stands: every key named by a link, held or not
    linked = {k for e in edges for k in e}
    before = [g for g in groups_of(linked, edges) if len(g) > 1]

    # keys named by links that clutter does not hold
    unheld = sorted(k for k in linked if k[0] in "DJ" and k not in held)
    # the albums: held keys, and the streams the links name (a Tidal album named by a
    # release need not be in the store); a release or file album clutter does not hold is
    # no album and is not followed
    nodes = held | {k for k in linked if k[0] in "TAQ"}
    g_all = groups_of(nodes, [e for e in edges if e[0] in nodes and e[1] in nodes])
    orphans = sorted(k for g in g_all if not (g & held) for k in g)
    g_all = [g for g in g_all if g & held]

    # step 3: stand-ins
    aliases = []  # (stand-in, target)
    standin_alone = []
    groups = []
    for g in g_all:
        si = sorted(k for k in g if k in disc and disc[k]["standin"])
        rest = g - set(si)
        if si and not rest:
            standin_alone.append(g)
            groups.append(g)
            continue
        if si:
            target = lead_of(rest, disc, first)
            aliases += [(s, target) for s in si]
            groups.append(frozenset(rest))
        else:
            groups.append(g)

    # step 4: same master
    d = DSU()
    for i, g in enumerate(groups):
        d.find(i)
    by_master = collections.defaultdict(list)
    for i, g in enumerate(groups):
        for k in g:
            if k in disc and not disc[k]["standin"] and disc[k]["master"]:
                by_master[disc[k]["master"]].append((i, k))
    master_joins = []  # (master, [keys of releases in different groups])
    for m, ik in sorted(by_master.items()):
        if len({i for i, _ in ik}) > 1:
            master_joins.append((m, sorted(k for _, k in ik), sorted({i for i, _ in ik})))
            for i, _ in ik:
                d.union(ik[0][0], i)
    merged = collections.defaultdict(set)
    parts = collections.defaultdict(list)
    for i, g in enumerate(groups):
        merged[d.find(i)] |= g
        parts[d.find(i)].append(g)
    albums = sorted((frozenset(s) for s in merged.values()), key=lambda s: sorted(s))
    joined_in = {}  # key -> True if it came into its album by a master join
    for r, ps in parts.items():
        if len(ps) > 1:
            whole = merged[r]
            lead = lead_of(whole, disc, first)
            for g in ps:
                for k in g:
                    joined_in[k] = lead not in g

    # step 5: ids and lines
    now = args.now or dt.datetime.now().astimezone().isoformat(timespec="seconds")
    taken = set(cids)
    lines = []
    album_of = {}
    for a in albums:
        lead = lead_of(a, disc, first)
        if len(a) == 1:
            album_of[lead] = lead
            continue
        cid = mint(a, taken)
        n = disc.get(lead) or tidal.get(lead) or jelly.get(lead) or lognames.get(lead) or {}
        for k in sorted(a, key=lambda k: (k != lead, KIND_ORDER.index(k[0]), k)):
            album_of[k] = cid
            kn = disc.get(k) or tidal.get(k) or jelly.get(k) or lognames.get(k) or n
            lines.append({"ts": now, "op": "version", "album": cid, "key": k,
                          "via": "discogs-master" if joined_in.get(k) else "migration",
                          "artist": kn.get("artist", ""), "title": kn.get("title", "")})
    for s, t in aliases:
        n = disc[s]
        lines.append({"ts": now, "op": "alias", "from": s, "to": t, "via": "migration",
                      "artist": n["artist"], "title": n["title"]})
    # not "version": every reader of today's log takes that field for a key
    lines.append({"ts": now, "op": "format", "format": 2, "via": "migration"})

    # ------------------------------------------------------------ checks
    seen = collections.Counter(k for a in albums for k in a)
    dup = sorted(k for k, c in seen.items() if c > 1)
    missing = sorted(k for k in held if k not in seen and k not in {s for s, _ in aliases})
    doggy = sorted(k for k in held | set(lognames) if "doggystyle" in name(k).casefold())

    p(f"# Album layer migration — {'write' if args.write else 'dry run'}, {now}")
    p("")
    p(f"Log {args.log}: {len(edges)} links. Holds {len(disc)} Discogs releases "
      f"({sum(v['standin'] for v in disc.values())} stand-ins), {len(tidal)} Tidal albums, {len(jelly)} file albums.")
    p("")
    p("## Checks")
    p(f"- groups of two or more keys over the links, as the log stands: {len(before)} (plan: 1,307)")
    p(f"- every key in exactly one album: {'yes' if not dup and not missing else 'NO'}"
      f" ({len(seen)} keys in {len(albums)} albums; {len(aliases)} stand-ins aliased)")
    for k in dup:
        p(f"  - twice: {k} {name(k)}")
    for k in missing[:20]:
        p(f"  - in none: {k} {name(k)}")
    p("- Doggystyle: " + "; ".join(f"{k} → {album_of.get(k, '(none)')}" for k in doggy))
    dog_albums = {album_of.get(k) for k in doggy if k[0] == "J"}
    p(f"  - its file copies are in {len(dog_albums)} albums (expect 2)")
    p("")

    p("## Albums by shape")
    sh = collections.Counter(shape(a) for a in albums)
    p("| shape | albums |")
    p("|---|---:|")
    for s, c in sorted(sh.items(), key=lambda x: (-x[1], x[0])):
        p(f"| {s} | {c} |")
    p(f"| **all** | **{len(albums)}** |")
    p("")

    def album_line(a):
        lead = lead_of(a, disc, first)
        return f"- {album_of.get(lead, lead)} · {name(lead)} · {shape(a)}: " + ", ".join(sorted(a, key=lambda k: (KIND_ORDER.index(k[0]), k)))

    multi = [a for a in albums if len(a) > 1]

    def section(title, sel, note=""):
        p(f"## {title} ({len(sel)})")
        if note:
            p(note)
        for a in sel:
            p(album_line(a))
        p("")

    diffm = [a for a in multi if len({disc[k]["master"] or k for k in a if k in disc}) > 1]
    p(f"## Albums with releases of different masters ({len(diffm)})")
    for a in diffm:
        p(album_line(a))
        for k in sorted(k for k in a if k in disc):
            p(f"  - {k} master {disc[k]['master'] or '—'} · {disc[k]['folder']} · {name(k)}")
    p("")
    section("Albums of more than six keys", [a for a in multi if len(a) > 6])
    section("Albums with two or more streams of one service",
            [a for a in multi if any(c > 1 for kd, c in collections.Counter(k[0] for k in a).items() if kd in "TAQ")])
    section("Albums with two or more file albums",
            [a for a in multi if sum(k[0] == "J" for k in a) > 1])

    p(f"## Same-master joins, step 4 ({len(master_joins)})")
    for m, ks, gi in master_joins:
        p(f"- master {m}: {len(gi)} groups joined")
        for i in gi:
            p(f"  - {shape(groups[i])}: " + ", ".join(f"{k} {name(k)}" if k in disc else k for k in sorted(groups[i], key=lambda k: (KIND_ORDER.index(k[0]), k))))
    p("")

    p(f"## Stand-ins aliased ({len(aliases)})")
    by_target = collections.Counter(t[0] for _, t in aliases)
    p("To: " + ", ".join(f"{KIND[k]} {c}" for k, c in sorted(by_target.items())))
    p("")
    p(f"## Stand-ins left as they are ({len(standin_alone)})")
    p("Only stand-ins in their group: each stays an album of its own until slice 6.")
    for a in standin_alone:
        p(album_line(a))
    p("")

    km = collections.defaultdict(list)
    alias_to = dict(aliases)
    for k, v in marks.items():
        km[album_of.get(alias_to.get(k, k), k)].append((k, v))
    twice = {a: ms for a, ms in km.items() if len(ms) > 1}
    p(f"## Keep marks on one album twice ({len(twice)}; {len(marks)} keep marks in all)")
    for a, ms in sorted(twice.items()):
        p(f"- {a}: " + ", ".join(f"{k}={v} ({name(k)})" for k, v in ms))
    p("")

    p(f"## Keys the links name that clutter does not hold ({len(unheld)}) — left out")
    for k in unheld:
        p(f"- {k} {name(k)}")
    p(f"- streams with no held album left (orphans, left out): {len(orphans)}" + (": " + ", ".join(orphans) if orphans else ""))
    p("")

    # plays, read as the plan says
    resolve = {}
    pc = collections.Counter()
    for e, ks in play_keys():
        k = next((alias_to.get(x, x) for x in ks if x in held or x in alias_to or x in seen), None)
        if k is None:
            pc["no key clutter holds"] += 1
        elif any(x in alias_to for x in ks[:1]):
            pc["via a stand-in's alias"] += 1
        else:
            pc["to a version"] += 1
    p("## Plays as the new reader would place them")
    for k, c in sorted(pc.items()):
        p(f"- {k}: {c}")
    p("")

    p("## Lines --write would append")
    for op, c in sorted(collections.Counter((l["op"], l["via"]) for l in lines).items()):
        p(f"- {op[0]} ({op[1]}): {c}")
    p(f"- total: {len(lines)}")

    print("\n".join(out))
    if args.lines:
        with open(args.lines, "w") as f:
            for l in lines:
                f.write(json.dumps(l, ensure_ascii=False) + "\n")
    if args.write:
        backup = args.log + ".before-album-layer"
        if os.path.exists(backup):
            sys.exit(f"{backup} exists: not overwriting it")
        shutil.copy2(args.log, backup)
        with open(args.log, "rb") as f:
            f.seek(0, 2)
            torn = f.tell() > 0 and (f.seek(-1, 2) or f.read(1) != b"\n")
        with open(args.log, "a") as f:
            if torn:
                f.write("\n")
            for l in lines:
                f.write(json.dumps(l, ensure_ascii=False) + "\n")
        print(f"\nappended {len(lines)} lines to {args.log} (copy: {backup})", file=sys.stderr)


if __name__ == "__main__":
    main()

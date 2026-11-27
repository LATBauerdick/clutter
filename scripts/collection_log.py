"""Read clutter's collection log for the scripts (the server's reader is src/Collection.hs).

~/Sync/clutter/collection.jsonl ($CLUTTER_COLLECTION): one line per change, the last one
counts. Since 2026-11-25 this log, not the Discogs Location field, says which Tidal, Apple
Music and Qobuz album a release has and which file albums are versions of which album.

A "moved" line (from, to: J keys) says that a file album's folder moved and Jellyfin gave
it a new id; every file album id in the log is read through these lines (follow()).
"""

import json
import os


def log_path() -> str:
    return os.path.expanduser(os.environ.get("CLUTTER_COLLECTION", "~/Sync/clutter/collection.jsonl"))


def _lines(path: str | None) -> list:
    try:
        with open(path or log_path()) as f:
            lines = f.readlines()
    except OSError:
        return []
    out = []
    for line in lines:
        try:
            e = json.loads(line)
        except json.JSONDecodeError:
            continue
        if isinstance(e, dict):
            out.append(e)
    return out


def _jid(key) -> str:
    return key[1:].lower() if isinstance(key, str) and key[:1] == "J" and len(key) == 33 else ""


def moves(path: str | None = None) -> dict:
    """old Jellyfin id -> new one, in the order of the log; a move to an id takes off a
    move from it (a folder moved back)"""
    mv: dict = {}
    for e in _lines(path):
        a, b = _jid(e.get("from")), _jid(e.get("to"))
        if e.get("op") == "moved" and a and b:
            mv.pop(b, None)
            mv[a] = b
    return mv


def follow(mv: dict, jid: str) -> str:
    """the id a file album has now"""
    seen = set()
    while jid in mv and jid not in seen:
        seen.add(jid)
        jid = mv[jid]
    return jid


def file_refs(path: str | None = None) -> set:
    """every file album (Jellyfin id, as it is now) that the log names: links, unlinks,
    skips, keep marks, streams of a file album; in the album format versions, splits,
    aliases, leads, tags and ratings by key"""
    mv = moves(path)
    refs = set()
    for e in _lines(path):
        for f in ("key", "version", "album", "from", "to"):
            j = _jid(e.get(f))
            if j:
                refs.add(follow(mv, j))
    return refs


def read(path: str | None = None) -> dict:
    """{"streams": {"D123": {"T": "5201052", "A": "…", "Q": "…"}},
        "files": {"<jellyfin id>": "D123" | "T456"},
        "touched": {<jellyfin ids ever linked or unlinked>}}
    In a log with a format line (the album format) no link or unlink line is read: all
    three are empty, and the album layer is read with album_log()."""
    streams: dict = {}
    files: dict = {}
    touched: set = set()
    mv = moves(path)
    if any(e.get("op") == "format" for e in _lines(path)):
        return {"streams": {}, "files": {}, "touched": set()}
    for e in _lines(path):
        op, version, key = e.get("op"), e.get("version") or "", e.get("key")
        kind, vid = version[:1], version[1:]
        if op not in ("link", "unlink") or not vid:
            continue
        if isinstance(key, str) and _jid(key):
            key = "J" + follow(mv, _jid(key))
        if kind == "J":
            jid = follow(mv, vid.lower())
            touched.add(jid)
            if op == "link" and key:
                files[jid] = key
            else:
                files.pop(jid, None)
        elif kind in "TAQ":
            if op == "link" and key:
                streams.setdefault(key, {})[kind] = vid
            else:
                for k in [key] if key else list(streams):
                    if streams.get(k, {}).get(kind) == vid:
                        del streams[k][kind]
    return {"streams": {k: v for k, v in streams.items() if v}, "files": files, "touched": touched}


def tidal_of_release(log: dict) -> dict:
    """Discogs release id (int) -> Tidal album id (int)"""
    return {int(k[1:]): int(v["T"]) for k, v in log["streams"].items() if k[:1] == "D" and v.get("T", "").isdigit()}


# ---------------------------------------------------------------- the album layer
#
# The same reading as src/AlbumLayer.hs (docs/album-layer-plan.md › Spec): an album with
# one version is named by its key, one with two or more has a C id; every name a line
# gives is resolved through the membership as the whole log leaves it. Last line wins for
# version/split (per key), keep, rate and lead (per album); tags fold in order. Moves,
# aliases and merges are chains, followed to the end; a loop stops where it closes. For
# moves a line to an id takes off a line from it; for merges and aliases only the exact
# reverse line does. Where a version key is meant a C id is not read.

CID_CHARS = set("0123456789abcdefghjkmnpqrstvwxyz")
KIND_RANK = {"D": 0, "T": 1, "J": 2, "A": 3, "Q": 4, "C": 5}


def parse_key(k) -> str | None:
    """the key as clutter writes it, or None (as Types.parseKey)"""
    if not isinstance(k, str) or not k:
        return None
    h, n = k[0], k[1:]
    if h in "DTA":
        return h + str(int(n)) if n.isdigit() else None
    if h == "J":
        return "J" + n.lower() if len(n) == 32 and all(c in "0123456789abcdefABCDEF" for c in n) else None
    if h == "Q":
        return k if n and all(c.isalnum() or c in "-_" for c in n) else None
    if h == "C":
        return k if len(n) == 6 and all(c in CID_CHARS for c in n) else None
    return "D" + str(int(k)) if k.isdigit() else None


def _chain(pairs) -> dict:
    """moves: a line to an id takes off a line from it"""
    m: dict = {}
    for a, b in pairs:
        m.pop(b, None)
        m[a] = b
    return m


def _links(pairs) -> dict:
    """merges and aliases: a later line from an id replaces the earlier one; only the exact
    reverse (b → a after a → b) takes the earlier one off, so a chain a → b → c stays"""
    m: dict = {}
    for a, b in pairs:
        if m.get(b) == a:
            del m[b]
        m[a] = b
    return m


def _vkey(k) -> str | None:
    """a version key: not a C id"""
    v = parse_key(k)
    return v if v and v[0] != "C" else None


def _event(e: dict):
    """(op, …) for a line of the album layer; None for a line it leaves to others or
    cannot read"""
    op = e.get("op")
    k = parse_key
    if op == "format":
        return ("format",)
    if op == "version":
        a, v = k(e.get("album")), _vkey(e.get("key"))
        return ("version", a, v) if a and a[0] == "C" and v else None
    if op == "split":
        v = _vkey(e.get("key"))
        return ("split", v) if v else None
    if op == "merge":
        a, b = k(e.get("from")), k(e.get("into"))
        return ("merge", a, b) if a and b and a[0] == b[0] == "C" else None
    if op == "lead":
        a = k(e.get("album"))
        if not a:
            return None
        if e.get("key") is None:
            return ("lead", a, None)
        v = _vkey(e.get("key"))
        return ("lead", a, v) if v else None
    if op == "alias":
        a, b = _vkey(e.get("from")), _vkey(e.get("to"))
        return ("alias", a, b) if a and b else None
    if op == "keep":
        a, v = k(e.get("key")), e.get("value")
        return ("keep", a, {"yes": True, "no": False, "unset": None}[v]) if a and v in ("yes", "no", "unset") else None
    if op == "tag":
        a, t, v = k(e.get("album")), e.get("tag"), e.get("value")
        return ("tag", a, t.lower(), v) if a and isinstance(t, str) and isinstance(v, bool) else None
    if op == "rate":
        a, s = k(e.get("album")), e.get("stars")
        return ("rate", a, s) if a and isinstance(s, int) and not isinstance(s, bool) and 0 <= s <= 5 else None
    if op == "moved":
        a, b = k(e.get("from")), k(e.get("to"))
        return ("moved", a[1:], b[1:]) if a and b and a[0] == b[0] == "J" else None
    return None


class AlbumLog:
    def __init__(self, events: list):
        self.format = any(e[0] == "format" for e in events)
        self.rated = any(e[0] == "rate" for e in events)  # ratings are clutter's, not Discogs's
        self.moves = _chain((e[1], e[2]) for e in events if e[0] == "moved")
        self.aliases = _links((self._mv(e[1]), self._mv(e[2])) for e in events if e[0] == "alias")
        self.merges = _links((e[1], e[2]) for e in events if e[0] == "merge")
        self.member: dict = {}  # canonical key -> (C id as written, line index)
        for n, e in enumerate(events):
            if e[0] == "version":
                self.member[self.canonical(e[2])] = (e[1], n)
            elif e[0] == "split":
                self.member.pop(self.canonical(e[1]), None)
        # C id -> [(version key, line index)] in the order they joined, computed once
        table: dict = {}
        for k, (c, n) in self.member.items():
            table.setdefault(follow(self.merges, c), []).append((k, n))
        self.albums = {a: sorted(vs, key=lambda x: x[1]) for a, vs in table.items()}
        self.keep: dict = {}
        self.tags: dict = {}
        self.rate: dict = {}
        self.lead: dict = {}
        for e in events:
            if e[0] == "keep":
                a = self.resolve(e[1])
                if e[2] is None:
                    self.keep.pop(a, None)
                else:
                    self.keep[a] = e[2]
            elif e[0] == "tag":
                a = self.resolve(e[1])
                ts = self.tags.setdefault(a, set())
                (ts.add if e[3] else ts.discard)(e[2])
                if not ts:
                    del self.tags[a]
            elif e[0] == "rate":
                a = self.resolve(e[1])
                if e[2] == 0:
                    self.rate.pop(a, None)
                else:
                    self.rate[a] = e[2]
            elif e[0] == "lead":
                a = self.resolve(e[1])
                if e[2] is None:
                    self.lead.pop(a, None)
                else:
                    self.lead[a] = self.canonical(e[2])

    def _mv(self, k: str) -> str:
        return "J" + follow(self.moves, k[1:]) if k[0] == "J" else k

    def canonical(self, k: str) -> str:
        """the key a version is known by now: moves and aliases followed"""
        seen = set()
        while True:
            k1 = self._mv(k)
            if k1 in seen:
                return k1
            k2 = self.aliases.get(k1)
            if k2 is None or k2 == k1:
                return k1
            seen.add(k1)
            k = k2

    def album_of(self, k: str) -> str:
        k = self.canonical(k)
        m = self.member.get(k)
        return follow(self.merges, m[0]) if m else k

    def resolve(self, name: str) -> str:
        """the album a line names, by C id or by version key"""
        return follow(self.merges, name) if name[0] == "C" else self.album_of(name)

    def albums_with_ids(self) -> dict:
        """C id -> [(version key, line index)] in the order they joined"""
        return self.albums

    def versions(self, name: str) -> list:
        a = self.resolve(name)
        return [k for k, _ in self.albums.get(a, [])] if a[0] == "C" else [a]

    def keep_of(self, name: str):
        return self.keep.get(self.resolve(name))

    def tags_of(self, name: str) -> set:
        return self.tags.get(self.resolve(name), set())

    def rating_of(self, name: str):
        return self.rate.get(self.resolve(name))

    def lead_of(self, name: str, added=lambda k: None):
        """the version that leads: a lead line's, if still a version; else records (the
        earliest added; no date last), Tidal, files, Apple Music, Qobuz, each by when it
        became a version"""
        a = self.resolve(name)
        vs = self.albums.get(a, []) if a[0] == "C" else [(a, 0)]
        named = self.lead.get(a)
        if named and any(k == named for k, _ in vs):
            return named

        def order(kn):
            k, n = kn
            r = KIND_RANK[k[0]]
            d = added(k) if r == 0 else None
            return (r, (0, d) if d is not None else (1, ""), n)

        return min(vs, key=order)[0] if vs else None

    def play_version(self, holds, keys: list):
        """a play's version: the first of its keys clutter holds, through moves and aliases"""
        for k in keys:
            c = self.canonical(k)
            if holds(c):
                return c
        return None


def album_log(path: str | None = None) -> AlbumLog:
    """the album layer as the whole log leaves it"""
    return AlbumLog([ev for e in _lines(path) if isinstance(e, dict) and (ev := _event(e))])


# ---------------------------------------------------------------- for the scripts


def has_format(path: str | None = None) -> bool:
    """the log is in the album format (album-layer-migrate.py --write ran)"""
    return any(e.get("op") == "format" for e in _lines(path))


def release_links(path: str | None = None) -> dict:
    """what each key has through its album, in the shape the scripts use:
        {"streams": {"D123": {"T": "5201052", "A": "…", "Q": "…"}},
         "files": {"D123": ["<jellyfin id>", …]}}
    In the album format (album_log()): the other versions of the key's album — the first
    of each stream service by when it joined, every file album; a release aliased away (a
    Streaming or Files stand-in) gets those of the album it now belongs to, including the
    key it was aliased to. Before it: read(), the links of the old log. Keys other than D
    (a Tidal album with file albums) are there too; scripts that want releases take D."""
    if not has_format(path):
        old = read(path)
        by_holder: dict = {}
        for jid, holder in old["files"].items():
            by_holder.setdefault(holder, []).append(jid)
        return {"streams": old["streams"], "files": by_holder}
    al = album_log(path)
    holders = {k for vs in al.albums.values() for k, _ in vs} | set(al.aliases)
    streams: dict = {}
    files: dict = {}
    for h in sorted(holders):
        st: dict = {}
        js: list = []
        for v in al.versions(al.album_of(h)):
            if v == h:
                continue
            if v[0] in "TAQ":
                st.setdefault(v[0], v[1:])
            elif v[0] == "J":
                js.append(v[1:])
        if st:
            streams[h] = st
        if js:
            files[h] = js
    return {"streams": streams, "files": files}


def release_of_tidal(path: str | None = None, keep=lambda rid: True) -> dict:
    """Tidal album id (int) -> Discogs release id (int): every Tidal album that is a version
    of an album with a release (the first release by when it joined that keep() accepts —
    e.g. still in the collection; a stand-in's alias counts), so an album with two Tidal
    editions keeps both. Before the album format: the old links, reversed."""
    if not has_format(path):
        out0: dict = {}
        for r, t in tidal_of_release(read(path)).items():
            if keep(r):
                out0.setdefault(t, r)
        return out0
    al = album_log(path)
    out: dict = {}
    pairs = [(a, [k for k, _ in vs]) for a, vs in al.albums.items()]
    pairs += [(al.album_of(d), [d]) for d in al.aliases if d[0] == "D"]
    for a, ds in pairs:
        rs = [k for k in ds if k[0] == "D" and keep(int(k[1:]))]
        if not rs:
            continue
        for v in al.versions(a):
            if v[0] == "T" and v[1:].isdigit():
                out.setdefault(int(v[1:]), int(rs[0][1:]))
    return out


def append_lines(path: str, lines: list) -> None:
    """append the lines (dicts) to the log as one O_APPEND write, after a newline if the
    log ends without one, and read them back; raises RuntimeError if the log does not end
    with them afterwards (another writer: stop clutter first)"""
    torn = False
    with open(path, "rb") as f:
        f.seek(0, 2)
        if f.tell() > 0:
            f.seek(-1, 2)
            torn = f.read(1) != b"\n"
    data = (("\n" if torn else "") + "".join(json.dumps(l, ensure_ascii=False) + "\n" for l in lines)).encode()
    fd = os.open(path, os.O_WRONLY | os.O_APPEND)
    try:
        view = memoryview(data)
        while view:
            view = view[os.write(fd, view):]
        os.fsync(fd)
    finally:
        os.close(fd)
    with open(path, "rb") as f:
        f.seek(0, 2)
        f.seek(max(0, f.tell() - len(data)))
        if f.read() != data:
            raise RuntimeError(f"{path} does not end with the lines just written (another writer?)")

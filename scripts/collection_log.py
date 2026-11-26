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
            out.append(json.loads(line))
        except json.JSONDecodeError:
            continue
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
    skips, keep marks, streams of a file album"""
    mv = moves(path)
    refs = set()
    for e in _lines(path):
        for f in ("key", "version"):
            j = _jid(e.get(f))
            if j:
                refs.add(follow(mv, j))
    return refs


def read(path: str | None = None) -> dict:
    """{"streams": {"D123": {"T": "5201052", "A": "…", "Q": "…"}},
        "files": {"<jellyfin id>": "D123" | "T456"},
        "touched": {<jellyfin ids ever linked or unlinked>}}"""
    streams: dict = {}
    files: dict = {}
    touched: set = set()
    mv = moves(path)
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

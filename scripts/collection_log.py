"""Read clutter's collection log for the scripts (the server's reader is src/Collection.hs).

~/Sync/clutter/collection.jsonl ($CLUTTER_COLLECTION): one line per change, the last one
counts. Since 2026-11-25 this log, not the Discogs Location field, says which Tidal, Apple
Music and Qobuz album a release has and which file albums are versions of which album.
"""

import json
import os


def log_path() -> str:
    return os.path.expanduser(os.environ.get("CLUTTER_COLLECTION", "~/Sync/clutter/collection.jsonl"))


def read(path: str | None = None) -> dict:
    """{"streams": {"D123": {"T": "5201052", "A": "…", "Q": "…"}},
        "files": {"<jellyfin id>": "D123" | "T456"},
        "touched": {<jellyfin ids ever linked or unlinked>}}"""
    streams: dict = {}
    files: dict = {}
    touched: set = set()
    try:
        with open(path or log_path()) as f:
            lines = f.readlines()
    except OSError:
        lines = []
    for line in lines:
        try:
            e = json.loads(line)
        except json.JSONDecodeError:
            continue
        op, version, key = e.get("op"), e.get("version") or "", e.get("key")
        kind, vid = version[:1], version[1:]
        if op not in ("link", "unlink") or not vid:
            continue
        if kind == "J":
            jid = vid.lower()
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

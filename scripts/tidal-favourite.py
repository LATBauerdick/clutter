#!/usr/bin/env python3
#
# Add one album to the Tidal collection (the favourites), with the token in tok.json
# (scope w_usr). The album page's "+ Tidal" button: the server runs this, then
# update-json --tidal, so the album comes back as a favourite with its date added.
#
#   scripts/tidal-favourite.py TID
#
# On a 401 the token is refreshed once with ./tidalauth (no terminal: no device login)
# and the call repeated. Exit 0 on success; otherwise one line on stderr, never the token.
#
import json
import os
import subprocess
import sys
import urllib.error
import urllib.parse
import urllib.request

HERE = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))


def add(tid: int) -> int:
    """HTTP status of the POST (0: no answer)"""
    tok = json.load(open(os.path.join(HERE, "tok.json"), encoding="utf-8"))
    url = (f"https://api.tidalhifi.com/v1/users/{tok['tid']}/favorites/albums?"
           + urllib.parse.urlencode({"countryCode": tok.get("tcc") or "US"}))
    req = urllib.request.Request(url, data=urllib.parse.urlencode({"albumIds": str(tid)}).encode(),
                                 headers={"Authorization": "Bearer " + tok["tdt"],
                                          "Content-Type": "application/x-www-form-urlencoded",
                                          "User-Agent": "clutter"},
                                 method="POST")
    try:
        with urllib.request.urlopen(req, timeout=20) as r:
            return r.status
    except urllib.error.HTTPError as e:
        return e.code
    except (urllib.error.URLError, TimeoutError):
        return 0


def main() -> None:
    if len(sys.argv) != 2 or not sys.argv[1].isdigit():
        print("usage: scripts/tidal-favourite.py TID", file=sys.stderr)
        sys.exit(2)
    tid = int(sys.argv[1])
    try:
        st = add(tid)
        if st == 401:
            subprocess.run(["./tidalauth"], cwd=HERE, stdin=subprocess.DEVNULL,
                           stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, timeout=60)
            st = add(tid)
    except (OSError, ValueError, KeyError, subprocess.TimeoutExpired) as e:
        print(f"no Tidal token ({type(e).__name__})", file=sys.stderr)
        sys.exit(1)
    if 200 <= st < 300:
        print(f"T{tid} added to the Tidal collection")
        return
    print(f"Tidal did not take T{tid}: " + (f"HTTP {st}" if st else "no answer"), file=sys.stderr)
    sys.exit(1)


if __name__ == "__main__":
    main()

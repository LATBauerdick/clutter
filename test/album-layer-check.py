#!/usr/bin/env python3
"""Slice 1 check of the album layer (docs/album-layer-plan.md › Slices): the Haskell reader
(src/AlbumLayer.hs via Collection.readAlbumLog, in cabal repl) and the Python one
(scripts/collection_log.py) read the same scratch log and must both give the answers
worked out by hand below.

Covers: last line wins (membership, keep, rate, lead), merge chain, merge back, split
(what was recorded against the key follows it), alias chain (a version line naming a
stand-in), a move, a move + alias loop that must end, names by C id and by version key,
tags folded in order (case folded), lines that cannot be read are skipped, the lead rule
with and without dates, plays resolved through moves and aliases, and the format line
switching the old link/unlink lines off in the old Python reader (collection_log.read;
the server's old reader went with slice 2, so the Haskell half has two answers fewer).

Run from the repository root:  python3 test/album-layer-check.py [--no-haskell]
cabal must be on PATH (with CABAL_DIR as the build needs); --no-haskell checks Python only.
"""

import json
import os
import subprocess
import sys
import tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
sys.path.insert(0, os.path.join(ROOT, "scripts"))
import collection_log as cl  # noqa: E402

J1, J2, J3, JX, JY = ("J" + c * 32 for c in "123ab")

LOG = [
    {"op": "link", "key": "D100", "version": J1, "via": "inventory:sure"},
    {"op": "link", "key": "D100", "version": "T500", "via": "clutter"},
    {"op": "keep", "key": "D100", "value": "yes"},
    {"op": "version", "album": "Caaaaaa", "key": "D100"},
    {"op": "version", "album": "Caaaaaa", "key": "T500"},
    {"op": "version", "album": "Cbbbbbb", "key": "D200"},
    {"op": "version", "album": "Cbbbbbb", "key": J1},
    {"op": "keep", "key": "Cbbbbbb", "value": "no"},
    {"op": "tag", "album": "D200", "tag": "My.Petra", "value": True},
    {"op": "merge", "from": "Cbbbbbb", "into": "Caaaaaa"},
    {"op": "tag", "album": "Caaaaaa", "tag": "my.jazz", "value": True},
    {"op": "tag", "album": "D100", "tag": "my.piano", "value": True},
    {"op": "tag", "album": "T500", "tag": "my.piano", "value": False},
    {"op": "rate", "album": "D100", "stars": 4},
    {"op": "rate", "album": "Cbbbbbb", "stars": 3},
    {"op": "moved", "from": J1, "to": J3},
    # merge chain C -> D -> E; a lead line naming the first id
    {"op": "version", "album": "Ccccccc", "key": "T600"},
    {"op": "version", "album": "Ccccccc", "key": "A700"},
    {"op": "merge", "from": "Ccccccc", "into": "Cdddddd"},
    {"op": "merge", "from": "Cdddddd", "into": "Ceeeeee"},
    {"op": "lead", "album": "Ccccccc", "key": "A700"},
    {"op": "keep", "key": "A700", "value": "yes"},
    # merge back: G -> H, then H -> G
    {"op": "version", "album": "Cgggggg", "key": "D300"},
    {"op": "version", "album": "Chhhhhh", "key": "D301"},
    {"op": "rate", "album": "D301", "stars": 2},
    {"op": "merge", "from": "Cgggggg", "into": "Chhhhhh"},
    {"op": "merge", "from": "Chhhhhh", "into": "Cgggggg"},
    # split; a lead line naming a version no longer in the album
    {"op": "version", "album": "Cjjjjjj", "key": "T800"},
    {"op": "version", "album": "Cjjjjjj", "key": "T801"},
    {"op": "version", "album": "Cjjjjjj", "key": "T802"},
    {"op": "keep", "key": "T801", "value": "no"},
    {"op": "lead", "album": "Cjjjjjj", "key": "T801"},
    {"op": "split", "key": "T801"},
    {"op": "rate", "album": "T801", "stars": 5},
    {"op": "rate", "album": "T801", "stars": 0},
    # last line wins for membership: T802 goes over to album K
    {"op": "version", "album": "Ckkkkkk", "key": "T802"},
    # alias chain D900 -> D901 -> T901; a version line names the stand-in
    {"op": "alias", "from": "D900", "to": "D901"},
    {"op": "alias", "from": "D901", "to": "T901"},
    {"op": "version", "album": "Ckkkkkk", "key": "D900"},
    {"op": "version", "album": "Ckkkkkk", "key": J2},
    {"op": "keep", "key": "D901", "value": "yes"},
    {"op": "keep", "key": "Ckkkkkk", "value": "unset"},
    {"op": "keep", "key": J2, "value": "yes"},
    {"op": "lead", "album": J2, "key": "T802"},
    {"op": "lead", "album": "Ckkkkkk"},
    # a loop: moved JX -> JY, alias JY -> JX
    {"op": "moved", "from": JX, "to": JY},
    {"op": "alias", "from": JY, "to": JX},
    {"op": "keep", "key": JX, "value": "yes"},
    # merges in both orders (review of 11-26): b -> c then a -> b, and a -> b then b -> c
    {"op": "version", "album": "Cmmmmmm", "key": "D11"},
    {"op": "version", "album": "Cmmmmmm", "key": "T11"},
    {"op": "version", "album": "Cnnnnnn", "key": "D12"},
    {"op": "version", "album": "Cnnnnnn", "key": "T12"},
    {"op": "version", "album": "Cpppppp", "key": "D13"},
    {"op": "version", "album": "Cpppppp", "key": "T13"},
    {"op": "merge", "from": "Cnnnnnn", "into": "Cpppppp"},
    {"op": "merge", "from": "Cmmmmmm", "into": "Cnnnnnn"},
    {"op": "version", "album": "Cqqqqqq", "key": "D21"},
    {"op": "version", "album": "Crrrrrr", "key": "D22"},
    {"op": "version", "album": "Cssssss", "key": "D23"},
    {"op": "merge", "from": "Cqqqqqq", "into": "Crrrrrr"},
    {"op": "merge", "from": "Crrrrrr", "into": "Cssssss"},
    # aliases in both orders
    {"op": "alias", "from": "T31", "to": "T32"},
    {"op": "alias", "from": "D39", "to": "T31"},
    {"op": "alias", "from": "D49", "to": "T41"},
    {"op": "alias", "from": "T41", "to": "T42"},
    # an album id where a version key is meant: not read (were it read, Ctttttt would hold
    # Caaaaaa, T600 would be read as Cvvvvvv, and a play of Cwwwwww would be T5's)
    {"op": "version", "album": "Ctttttt", "key": "Caaaaaa"},
    {"op": "split", "key": "Caaaaaa"},
    {"op": "lead", "album": "D100", "key": "Caaaaaa"},
    {"op": "alias", "from": "T600", "to": "Cvvvvvv"},
    {"op": "alias", "from": "Cwwwwww", "to": "T5"},
    # lines left to other readers, and lines that cannot be read
    {"op": "skip", "version": J2},
    {"op": "notsame", "version": J2, "key": "D100"},
    {"op": "version", "album": "D5", "key": "T5"},
    {"op": "version", "album": "Cilouxx", "key": "T5"},
    {"op": "rate", "album": "T5", "stars": 9},
    {"op": "tag", "album": "T5", "tag": "x", "value": "yes"},
    {"op": "format", "format": 2, "via": "migration"},
    {"op": "link", "key": "D100", "version": "A999", "via": "clutter"},
]

NAMES = ["D100", "T500", "D200", J1, J3, "Caaaaaa", "Cbbbbbb", "T600", "A700", "Ccccccc", "Cdddddd",
         "Ceeeeee", "D300", "D301", "Cgggggg", "Chhhhhh", "T800", "T801", "T802", "Cjjjjjj", "D900", "D901",
         "T901", J2, "Ckkkkkk", JX, JY, "T5",
         "D11", "T12", "Cmmmmmm", "Cnnnnnn", "Cpppppp", "D21", "Cqqqqqq", "Crrrrrr", "Cssssss",
         "D39", "T31", "D49", "T41", "Ctttttt"]
ADDED = {"D200": "2020-01-01", "D100": "2021-05-05"}
HOLDS = ["D100", "T500", "D200", J3, "T600", "A700", "D300", "D301", "T800", "T801", "T802", "T901", J2, JY, "T5"]
PLAYS = [["D900", "T999"], [J1], ["D555", "T500"], ["D555"], [JX], ["Cwwwwww"]]
# single lines through each reader's line parser: read or not
LINES = [
    {"op": "version", "album": "Ctttttt", "key": "Caaaaaa"},
    {"op": "split", "key": "Caaaaaa"},
    {"op": "lead", "album": "D100", "key": "Caaaaaa"},
    {"op": "alias", "from": "Cwwwwww", "to": "T5"},
    {"op": "alias", "from": "T5", "to": "Cwwwwww"},
    {"op": "version", "album": "Ctttttt", "key": "T5"},
    {"op": "lead", "album": "Ctttttt", "key": "T5"},
    {"op": "alias", "from": "D5", "to": "T5"},
]


def expected() -> list:
    # name | album | versions | keep | tags | rating | lead (with dates) | lead (no dates)
    a = f"Caaaaaa|D100,T500,D200,{J3}|no|my.jazz,my.petra|3|D200|D100"
    e = "Ceeeeee|T600,A700|yes|-|-|A700|A700"
    g = "Cgggggg|D300,D301|-|-|2|D300|D300"
    k = f"Ckkkkkk|T802,T901,{J2}|yes|-|-|T802|T802"
    j = "Cjjjjjj|T800|-|-|-|T800|T800"
    by = {n: a for n in ["D100", "T500", "D200", J1, J3, "Caaaaaa", "Cbbbbbb"]}
    by |= {n: e for n in ["T600", "A700", "Ccccccc", "Cdddddd", "Ceeeeee"]}
    by |= {n: g for n in ["D300", "D301", "Cgggggg", "Chhhhhh"]}
    by |= {n: k for n in ["T802", "D900", "D901", "T901", J2, "Ckkkkkk"]}
    by |= {"T800": j, "Cjjjjjj": j, "T801": "T801|T801|no|-|-|T801|T801",
           JX: f"{JY}|{JY}|yes|-|-|{JY}|{JY}", JY: f"{JY}|{JY}|yes|-|-|{JY}|{JY}", "T5": "T5|T5|-|-|-|T5|T5"}
    m = "Cpppppp|D11,T11,D12,T12,D13,T13|-|-|-|D11|D11"
    s = "Cssssss|D21,D22,D23|-|-|-|D21|D21"
    by |= {n: m for n in ["D11", "T12", "Cmmmmmm", "Cnnnnnn", "Cpppppp"]}
    by |= {n: s for n in ["D21", "Cqqqqqq", "Crrrrrr", "Cssssss"]}
    by |= {"D39": "T32|T32|-|-|-|T32|T32", "T31": "T32|T32|-|-|-|T32|T32",
           "D49": "T42|T42|-|-|-|T42|T42", "T41": "T42|T42|-|-|-|T42|T42", "Ctttttt": "Ctttttt|-|-|-|-|-|-"}
    out = [f"{n}|{by[n]}" for n in NAMES] + ["format|True", "rated|True"]
    out += [f"play {','.join(p)}|{r}" for p, r in zip(PLAYS, ["T901", J3, "T500", "-", JY, "-"])]
    out += [f"line {i}|{r}" for i, r in enumerate(["not read"] * 5 + ["read"] * 3)]
    return out + ["old log.jsonl|files 0|streams 0", "old log-noformat.jsonl|files 1|streams 2"]


def python_answers(d: str) -> list:
    al = cl.album_log(os.path.join(d, "log.jsonl"))
    f = lambda x: "-" if x is None else ("yes" if x is True else "no" if x is False else str(x))  # noqa: E731
    out = []
    for n in NAMES:
        out.append("|".join([n, al.resolve(n), ",".join(al.versions(n)) or "-", f(al.keep_of(n)),
                             ",".join(sorted(al.tags_of(n))) or "-", f(al.rating_of(n)),
                             f(al.lead_of(n, ADDED.get)), f(al.lead_of(n))]))
    out.append(f"format|{al.format}")
    out.append(f"rated|{al.rated}")
    holds = set(HOLDS)
    out += [f"play {','.join(p)}|{f(al.play_version(holds.__contains__, p))}" for p in PLAYS]
    out += [f"line {i}|{'read' if cl._event(e) else 'not read'}" for i, e in enumerate(LINES)]
    for p in ("log.jsonl", "log-noformat.jsonl"):
        r = cl.read(os.path.join(d, p))
        out.append(f"old {p}|files {len(r['files'])}|streams {sum(len(v) for v in r['streams'].values())}")
    return out


GHCI = r'''
:set -XOverloadedStrings
import qualified AlbumLayer as AL
import qualified Collection as Co
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as LB
import Types (AlbumKey (..), parseKey, keyText)
import Data.Maybe (mapMaybe)
import Data.List (intercalate)
:{
albumLayerCheck :: IO ()
albumLayerCheck = do
  al <- Co.readAlbumLog "DIR/log.jsonl"
  let names = mapMaybe parseKey (T.words "NAMES")
      added k = M.lookup k (M.fromList (mapMaybe (\(a, b) -> (\x -> (x, b)) <$> parseKey a) ADDED))
      f = maybe "-" id
      b = fmap (\x -> if x then "yes" else "no")
      ks = T.unpack . keyText
      csv xs = if null xs then "-" else intercalate "," xs
      out = putStrLn . ("OUT " ++)
  mapM_ (\n -> out $ intercalate "|"
      [ ks n, ks (AL.resolveName al n), csv (map ks (AL.versionsOf al n)), f (b (AL.keepOf al n))
      , csv (map T.unpack (S.toList (AL.tagsOf al n))), f (show <$> AL.ratingOf al n)
      , f (ks <$> AL.leadOf added al n), f (ks <$> AL.leadOf (const (Nothing :: Maybe String)) al n) ]) names
  out ("format|" ++ show (AL.alFormat al))
  out ("rated|" ++ show (AL.alRated al))
  let holds = (`elem` mapMaybe parseKey (T.words "HOLDS"))
  mapM_ (\w -> out ("play " ++ T.unpack w ++ "|" ++ f (ks <$> AL.playVersion holds al (mapMaybe parseKey (T.splitOn "," w))))) (T.words "PLAYS")
  mapM_ (\(i, l) -> out ("line " ++ show i ++ "|" ++ either (const "not read") (maybe "not read" (const "read")) (A.eitherDecode l >>= AT.parseEither AL.logEvent)))
    (zip [0 :: Int ..] LINES)
:}
albumLayerCheck
'''


def haskell_answers(d: str) -> list:
    script = (GHCI.replace("DIR", d).replace("NAMES", " ".join(NAMES)).replace("HOLDS", " ".join(HOLDS))
              .replace("PLAYS", " ".join(",".join(p) for p in PLAYS))
              .replace("LINES", "[" + ", ".join("LB.pack " + json.dumps(json.dumps(e)) for e in LINES) + "]")
              .replace("ADDED", "[" + ", ".join(f'("{k}", "{v}" :: String)' for k, v in ADDED.items()) + "]"))
    r = subprocess.run(["cabal", "repl", "--offline", "-v0", "lib:clutter"], input=script, cwd=ROOT,
                       capture_output=True, text=True)
    out = [line.split("OUT ", 1)[1] for line in r.stdout.splitlines() if "OUT " in line]
    if not out:
        sys.stderr.write(r.stdout[-3000:] + r.stderr[-3000:])
    return out


def compare(name: str, got: list, want: list) -> bool:
    bad = [(w, g) for w, g in zip(want, got) if w != g]
    if len(got) != len(want):
        print(f"{name}: {len(got)} answers, want {len(want)}")
    for w, g in bad:
        print(f"{name}:\n  want {w}\n  got  {g}")
    ok = not bad and len(got) == len(want)
    print(f"{name}: {'OK' if ok else 'FAILED'} ({len(want)} answers)")
    return ok


def main() -> None:
    with tempfile.TemporaryDirectory() as d:
        ts = "2026-11-26T20:00:00-06:00"
        with open(os.path.join(d, "log.jsonl"), "w") as f:
            for e in LOG:
                f.write(json.dumps({"ts": ts, **e}) + "\n")
            f.write("not json\n")
        with open(os.path.join(d, "log-noformat.jsonl"), "w") as f:
            for e in LOG:
                if e["op"] != "format":
                    f.write(json.dumps({"ts": ts, **e}) + "\n")
        want = expected()
        ok = compare("python", python_answers(d), want)
        if "--no-haskell" not in sys.argv:
            # the server's old link reader is gone (slice 2): no "old …" answers
            ok = compare("haskell", haskell_answers(d), [w for w in want if not w.startswith("old ")]) and ok
    sys.exit(0 if ok else 1)


if __name__ == "__main__":
    main()

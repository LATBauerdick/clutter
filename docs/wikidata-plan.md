# Wikipedia and AllMusic links via Wikidata — plan

Status: **phases 1–3 done**. Phases 1–2 (2026-11-21): `scripts/update-wikidata.py`, run by
`update-json` (both modes). Fills: 3,574 Discogs keys in 18 queries, then 7,257 Tidal/Apple keys
in 37, no rate limiting. Phase 3 (2026-11-22): `src/Wikidata.hs`; `/api/albumq/…` carries
`albumWikiKeys`, `albumWiki`, `albumAllMusic` and `albumArtists` — checked on a Discogs album
(U.F.O.F.), a Tidal-only album (Offset, *Father of 4*) and two classical Tidal albums whose
artists were matched by name (Pletnev, Wiener Philharmoniker). Next: phase 4, the frontend.

| key | keys | Wikidata | enwiki | dewiki | AllMusic |
|---|---|---|---|---|---|
| Discogs master (`dm`) | 1,786 | 552 | 536 | 290 | 401 |
| Discogs artist (`da`) | 1,788 | 1,571 | 1,459 | 1,267 | 1,347 |
| Tidal album (`t`) | 2,806 | 150 | 148 | 74 | 145 |
| Apple album (`a`) | 2,272 | 148 | 147 | 70 | 140 |
| Tidal artist (`ta`) | 2,179 | 1,247 | 1,212 | 1,019 | 1,111 |

- Per Discogs release (2,037): 580 get an album link, 1,975 at least one artist link, 1,980 any
  link (97 %).
- Per Tidal-only album (2,714, not linked from a Discogs release): 174 an album link, 1,874 an
  artist link, 1,887 any link (70 %); classical 318 of 581 (55 %).
- Tidal artist ids are thinner in Wikidata (57 %) than Discogs artist ids (88 %). Of the 1,269
  Tidal artist slots without a link, 213 match a linked Discogs artist by name (Discogs `(n)`
  suffix stripped, case-folded): phase 3 does that match when it attaches links. The rest get
  search links (decision 2).
- Apple album ids add almost nothing over Tidal album ids (the same albums), but cost nothing.
- A rerun asks only new keys; rechecks by hand (`--recheck`).

Decisions are at the end.

## Goal

On an album's page in clutter, show links to the album's and each artist's Wikipedia page and
AllMusic page, where they exist, and search links where they don't.

## Why Wikidata

Wikidata is the hub that ties together the ids clutter already has (Discogs, Tidal, Apple Music)
and the pages we want (Wikipedia in every language, AllMusic). One batched SPARQL query answers
hundreds of ids at once. AllMusic itself has no API; Wikidata gives its exact page URL.

Properties used:

| property | id |
|---|---|
| Discogs master ID | P1954 |
| Discogs artist ID | P1953 |
| Tidal album ID | P4577 |
| Tidal artist ID | P4576 |
| Apple Music album ID | P2281 |
| AllMusic album ID | P1729 → `https://www.allmusic.com/album/<id>` |
| AllMusic artist ID | P1728 → `https://www.allmusic.com/artist/<id>` |
| MusicBrainz release group ID | P436 (only if the MusicBrainz route is added later) |

Wikipedia pages are the item's sitelinks (`schema:about` / `schema:isPartOf`).

## Measured coverage (2026-11-21, random samples of 200)

| key | in Wikidata | English Wikipedia | en or de | AllMusic |
|---|---|---|---|---|
| Discogs master → album | 61 (30 %) | 58 (29 %) | 58 | 48 (24 %) |
| Discogs artist → artist | 180 (90 %) | 168 (84 %) | 171 | 160 (80 %) |
| Tidal album id → album | 7 (3.5 %) | 7 | 7 | 7 |

- Artists are the strong part: most albums get useful links through their artists.
- Albums via Discogs master: about a third. 1,876 of the 2,037 collection releases have a master;
  the rest can only get artist links.
- Tidal album ids are almost absent from Wikidata, and none of the 42 classical albums in the
  sample was there. Individual classical recordings seldom have a Wikipedia page at all.
- Not yet measured: Tidal **artist** ids (P4576) and Apple Music album ids (P2281). Measure
  before building phase 2.

## Design

### Lookup script: `scripts/update-wikidata.py`

Stdlib only, like `update-tidal-albums.py`. Run by `update-json` after the Tidal step (also by
`update-json --tidal`, for the Tidal side only). Sends only public ids to Wikidata, with a
generic User-Agent.

- Collects the keys to look up:
  - Discogs collection (`cache/draw*.json`): `master_id`, and each `artists[].id`
    (`basic_information.artists`)
  - Tidal album store (`~/Sync/clutter/tidal-albums.json`): album id, the album's artist ids,
    and the Apple id where there is one
- Asks only for keys that are new. Rechecks run only by hand (`--recheck`: misses only;
  `--recheck=all`: every key), never automatically; every run prints a reminder once misses are
  180 days or older (decision 4).
- Batches 200 ids per SPARQL POST to `https://query.wikidata.org/sparql`. A full first run is
  ~6,000 keys, about 30 queries. Handles **429** by waiting for `Retry-After` (on 2026-11-21 WDQS
  was limited to 1 request a minute because of an outage), so the first run may take ~30 min,
  later runs seconds.
- `--dry-run`, and a summary line on stderr like the Tidal script.

### Store: `~/Sync/clutter/wikidata.json`

Next to the Tidal store (syncthing + snapshots, survives a cache wipe). `CLUTTER_WIKIDATA`
overrides. Written atomically with a `.bak`, like `write_store()` in the Tidal script.

```json
{ "updated": "2026-11-21T23:10:00-06:00",
  "albums":  { "dm:12345": { "qid": "Q123", "enwiki": "…", "dewiki": "…", "allmusic": "mw0000123", "checked": "2026-11-21" },
               "t:71675494": { "checked": "2026-11-21" } },
  "artists": { "da:45467": { "qid": "Q1299", "enwiki": "…", "dewiki": "…", "allmusic": "mn0000754032", "checked": "2026-11-21" },
               "ta:3346": { … } } }
```

Key prefixes: `dm` Discogs master, `t` Tidal album, `a` Apple album, `da` Discogs artist,
`ta` Tidal artist. A miss is stored as `{checked}` only.

### Server

- `Album` gets the album's links (`albumWiki`, `albumAllMusic` as `Maybe Text`) and
  `albumArtists :: [ArtistRef]` with name, Wikipedia and AllMusic per artist. Today `Album`
  carries only the joined artist name, so the Discogs and Tidal parsers must also keep the artist
  ids.
- Read `wikidata.json` at `envInit` and attach, like the play log and the Tidal store.
  Missing file = no links, no error.
- For an album with both a Discogs master and a Tidal/Apple id, the Discogs master wins, then
  Tidal, then Apple.

### Frontend (album view)

One line under the existing Discogs / Tidal / Apple Music links:

`Wikipedia · AllMusic   —   Artist: Wikipedia · AllMusic   (one group per artist)`

- Where nothing was found: search links instead (Wikipedia search for "artist title";
  AllMusic search), shown dimmed (decision 2).
- Classical albums often list composer and performers as separate artists, so per-artist links
  matter more there than the album link.

### Optional: journal and note

`renderAlbumJournal` (Day One) and the Obsidian note could carry the album's Wikipedia link in
their links line, and each artist's (decision 3).

## Phases

| phase | content | done when |
|---|---|---|
| 1 ✅ | script + store for Discogs masters and artists | store filled; coverage report matches the sample |
| 2 ✅ | Tidal side: Tidal artists, Tidal and Apple albums | report per key type |
| 3 ✅ | server: parse artist ids, load store, extend `Album` JSON | `/api/albumq/…` carries the links |
| 4 | frontend links line, with fallback search links if wanted | visible on a Discogs, a Tidal-only and a classical album |
| 5 | Wikipedia links in the Day One and Obsidian entries | a new play's entry carries them |

Phase 1 alone is useful to judge the data before any Haskell work.

### As built (phase 3)

- `Release` carries `dartistids` (store keys parallel to `dartists`: `da:`, `ta:`, `""` for Apple
  and the Discogs placeholder artists) and `dmaster` (`master_id`, 0 → `Nothing`).
- `Album` gets `albumWikiKeys` (best first: `dm`, `t`, `a`; a Discogs release with a `T…`/`A…`
  location note gets those too), `albumWiki`, `albumAllMusic` (full URLs) and
  `albumArtists :: [ArtistRef]` (`arName` without the Discogs `(n)` suffix, `arKey`, `arWiki`,
  `arAllMusic`). Each link field takes the first key that has it.
- An artist with no link of its own takes the entry of the linked Discogs artist with the same
  case-folded display name, if exactly one has that name.
- `Wikidata.attachWiki` runs at `envInit` and again in `withPlays` (via the `Env` field
  `wikiLinks`), so albums added later (Tidal reconnect, made-up play albums) get links too. The
  store is read once at start; after `update-wikidata.py` restart the server.

## Decisions (LATB, 2026-11-21)

1. Wikipedia language: **English, German as fallback** (one link; the store keeps both).
2. **Search links** when nothing matched: there is usually something on Wikipedia somewhere.
3. **Yes**, Wikipedia links in the Day One and Obsidian entries.
4. **Rechecks by hand**, with a reminder at 180 days, not an automatic refresh.

## Later, not in scope

- MusicBrainz route (UPC → release → release group → Wikidata) for Tidal-only albums, if phase 2
  shows the gap matters. MusicBrainz allows 1 request/s, so it is a slow backfill.
- Composer and work pages for classical albums (from Discogs or MusicBrainz credits).
- Improving Wikidata itself: adding a Discogs master id to a Wikidata item is a public edit that
  the next refresh picks up.

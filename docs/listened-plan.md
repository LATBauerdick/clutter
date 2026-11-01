# Plan: move "Listened" out of Discogs into clutter's own store

Written 2026-11-01 (Claude, from the aiws workspace) for a Claude Code session run in `~/clutter`. Status: **Phases 1–2 implemented 2026-11-01, not yet deployed** (see "Implementation notes" at the end). The decisions marked ❓ belong to LATB.

## Why

Album plays are currently tracked through Discogs, in two ways:

| period | where the plays live | format |
|---|---|---|
| 2020–2025 | Discogs **lists** `2020 Listened` … `2025 Listened` (ids 540434, 701908, 915964, 1174210, 1442779, 1574563) | one list item per release; the item **comment** holds the dates, one per line, `YYYY-MM-DD` (newest first) |
| 2026 | the Discogs **Want list** (pseudo-list `"Want"`, id 7, in `FromDiscogs.readDiscogsLists`), plus a stub list `2026 Listened` (1656131, 1 item) | the want's **notes** hold one `YYYY-MM-DDTHH:MM` per line, newest first |

Counts in `cache/` on 2026-11-01: 2020 167 items / 232 dates, 2021 497/546, 2022 159/174, 2023 281/309, 2024 152/159, 2025 404/428, Want 85/118 (2026-01-01 → 2026-10-31).

The kludge has these problems:
- A play can only be recorded against a **Discogs release**. A Tidal-only (or Apple-only) album has no Want entry to write to. `updateAlbumsPlayed` then sends `PUT /users/LATB/wants/<albumID>` with the **Tidal id** as if it were a Discogs id. This usually 404s, so the play is silently lost. If a Discogs release happens to have that number, the play lands on an unrelated record. The id ranges overlap: Tidal 12571482 is a valid Discogs-size number.
- It pollutes the real Want list, and the year rollover is manual: wants have to be moved to a `YYYY Listened` list by hand.
- `readWantListAids` reads only **page 1 of 500**, so it breaks silently once a year passes 500 distinct albums. 2021 had 497.
- The Plays count is a Discogs custom field (field **7** on the collection instance). It is incremented blindly, and the code wraps it at 6 (`6 -> ""`, "should be a configuration parameter").
- Notes are written with hand-built JSON (`"{\"notes\": \"" <> tn <> "\"}"`). Only newlines are escaped, so a `"` or `\` in existing notes breaks the write.
- Every play needs 3 Discogs API calls, and Discogs rate-limits them. Two 50-byte rate-limit bodies once ended up in Day One as fake JPEGs.

## Goal

clutter keeps its own **append-only play log**, one JSON-lines file per year. Any album clutter knows can be logged, whether it comes from Discogs, Tidal, Apple Music, Qobuz or Jellyfin. Discogs goes back to being only the catalogue of physical releases.

## Proposed design

### Storage

- **Format: JSON Lines, one file per year: `listened/2026.jsonl`.** LATB asked for "a json file per year", and JSONL is one: one event per line. It is append-only, so a write never rewrites history. It also diffs cleanly in git, survives a crash mid-write (at worst a torn last line, which the reader skips with a warning), and merges trivially if syncthing ever produces a conflict copy (concatenate, then dedupe). A SQLite database would be overkill for ~500 events a year.
- ❓ **Location.** Recommended: `~/Sync/clutter/listened/`, next to `~/Sync/AlbumsPlayed`. That puts it under syncthing to umini, with ZFS snapshots and fmini syncoid. Today `tok.json` and `cache/` have **no backup at all**, and the play log must not share that fate. Make the location configurable (`CLUTTER_LISTENED`, defaulting to `./listened`, which can be a symlink like `AlbumsPlayed`).
- One event per line:

```json
{"ts":"2026-11-01T10:58:00-06:00","src":"discogs","discogs":6715388,"inst":2200131655,"tidal":283130328,"apple":1556465220,"artist":"Maria Yudina, Franz Schubert, Robert Schumann","title":"The Legacy Of Maria Yudina - Volume 6","via":"clutter"}
{"ts":"2026-10-31T17:21:00-05:00","src":"tidal","tidal":187293380,"artist":"Various Artists","title":"Dark Nights: Death Metal Soundtrack","via":"clutter"}
```

  - `ts`: local time **with UTC offset**. AlbumsPlayed notes were stamped in UTC until 2021-07-01 and in local time after that, with no offset in either case. Don't repeat that ambiguity.
  - `src`: which catalogue the album came from (`discogs|tidal|apple|qobuz|jellyfin`), plus **every id known at play time**. Album identity is a typed key, never a bare `Int`. See "ID namespaces" below.
  - `artist` and `title` are denormalised, so the log stays readable and survives a release being removed from the collection or a Tidal album going dark.
  - `via`: `clutter` | `import:discogs-list` | `import:want` | `import:albumsplayed` | `manual`. This is provenance, so an import can be redone or undone.
  - Optional `note` field for free text.

### ID namespaces (the root bug)

`Album.albumID :: Int` holds a Discogs release id for Discogs albums and a **Tidal album id** for Tidal-only albums (`Provider.readTidalReleases` → `toAlbum`). Apple ids are handled the same way. `updateTidalFolderAids` maps Tidal ids to Discogs ids when a Discogs release carries `T<id>` in its Location, which shows the overlap is already known. Don't widen `albumID` itself, because that touches every view. Instead:
- Add `data AlbumKey = KDiscogs Int | KTidal Int | KApple Int | …`, derived from `albumFolder` (`TTidal`, `TAMusic`, …) plus `albumID`.
- The log matches an event to a displayed album by trying, in order:
  1. Discogs id
  2. Tidal id, then the Tidal→Discogs map
  3. Apple id, likewise

  A Tidal play therefore shows up on the Discogs release once that release gets `T<id>` in its Location. This is exactly the workflow used for Nikolayeva, Richter, Levit and Yudina in Oct–Nov 2026.

### Write path (`serveAlbump` → `updateAlbumsPlayed`, `src/App.hs`)

1. **Append the event to the log first** (new module `src/Listened.hs`: `appendPlay :: FilePath -> PlayEvent -> IO ()`, open in append mode, one `hPutStrLn` of the aeson-encoded event, then flush). This step must succeed for every album type.
2. Keep the Obsidian note (`AlbumsPlayed/`) and the Day One entry unchanged. They are LATB's second and third bookkeeping. Day One is the authoritative clock: the 2026-09 AlbumsPlayed analysis used it to correct 359 UTC stamps.
3. The Discogs Want write and the field-7 increment go behind a flag (`CLUTTER_DISCOGS_PLAYS=1`) during the transition, then get deleted. ❓ See open questions.
4. **Skip all Discogs calls when the album is not a Discogs release** (`KTidal`/`KApple`). This fixes the lost-play bug even before the rest lands.
5. Update the in-memory `listenedDatesR` so the UI reflects the play without a restart.

### Read path

- Load every `listened/*.jsonl` at `envInit`, building a `Map AlbumKey [ZonedTime]` (resolved to display album ids via the matching above). The `listenedDatesR` IORef already exists in `Env.hs` (currently `M.empty`).
- **Plays = number of events**, computed from the log. Discogs field 7 is no longer read for the count (❓ unless LATB wants to keep it as the source of truth for pre-2020 plays; check whether any release has field 7 > its logged count after import).
- Replace the `"Want"` pseudo-list (id 7) with a `"Listened"` list: every album with ≥1 event, newest play first. An album appears once at its latest play, and the "Played N times" hover gives the rest. Add `Listened YYYY` lists per year.
- Re-enable `albumListenedDates` on `Album` (commented out in `Types.hs`, `Provider.hs` has `-- []` placeholders) and in `frontend/src/Types.purs`.
- Menus: `RenderTopMenu.hs:126` still links to `2024 Listened`; `frontend/src/RenderTopMenu.purs:30` links "Listened" to `"Want"`. Point both at the new list.

### Backfill (one-off import, rerunnable)

Use a standalone script (`scripts/import-listened.py`, stdlib only, reading `tok.json` for the Discogs token and **never printing it**) or a hidden clutter subcommand. It writes `listened/YYYY.jsonl` with `via` set:
1. **Discogs `YYYY Listened` lists 2020–2025** (+ the 2026 stub): one event per date line in each item comment. Dates are date-only, so emit `"ts":"2021-03-04"` (no time) rather than inventing 12:00.
2. **Want list notes** (2026): one event per `YYYY-MM-DDTHH:MM` line, America/Chicago offset.
3. **Cross-check against `~/Sync/AlbumsPlayed/*.md`** (1,777 files, 2020-12 → today). Each has `date:` frontmatter and `[1]:` = Discogs/Tidal/Apple URL (1,478 / 11 / 1 on 2026-11-01). Use them to:
   - **add time of day** to date-only list events where a note matches the same release and day
   - **recover Tidal-only plays** that never reached Discogs. There are 11, e.g. `210706-Dark Nights_ Death Metal Soundtrack` (Tidal 187293380) and `250820-Automat – Ost West` (88150238).
   - report events present in one source but not the other. **Don't auto-merge these; show LATB the diff.**
   - ⚠️ Stamps before **2021-07-01 03:02** are **UTC**; from 2021-07-01 22:42 they are local (America/Chicago). 359 entries were affected; see `~/Notes/Claude/2026-09-albums-played-report.md`. The note file name prefix `YYMMDD-` uses the same clock.
4. Optional, and the most precise: the **Day One "Albums Played" journal** in `~/Library/Group Containers/5U8NS4GX82.dayoneapp2/Data/Documents/DayOne.sqlite` (read-only: copy the file first, never open the live DB for writing). It has real timestamps with timezones (`ZTIMEZONE`). Use it only if steps 1–3 leave too many gaps.
5. Dedupe on (album key, minute). Print per-year totals before and after, and compare with the counts in "Why" above.

### Retirement (only after LATB has checked the imported log)

- Stop writing the Want list and field 7 (remove the flag and the code).
- ❓ Clean up the Want list: remove the 85 play-tracking wants, or keep them as an archive. Don't touch it without LATB's OK.
- Freeze the `YYYY Listened` Discogs lists as an archive. No more writes.
- Drop `wants1.json` from `update-json` and remove `readWantListAids`/`sortByMultipleDates` once nothing reads them.
- Remove the dead `extractListenedDates`/`readDiscogsListAidsWithComments` code in `FromDiscogs.hs`, or reuse it in the importer.

## Phases

| # | what | done when |
|---|---|---|
| 0 | Snapshot all sources into `cache/listened-import/` (lists, wants, AlbumsPlayed listing) | raw copies exist and counts match the table above |
| 1 | `Listened.hs`: types, `AlbumKey`, read/append JSONL, tests in `test/` | round-trip test passes; a torn last line is skipped with a warning |
| 2 | Write path: append on play; skip Discogs for non-Discogs albums; Discogs writes behind a flag | a Tidal-only "Album Played" writes one log line and **no** Discogs call (watch the stderr log) |
| 3 | Backfill importer + reconciliation report | LATB has reviewed the report |
| 4 | Read path: Listened list(s), plays from the log, `albumListenedDates` in the API | `/api/albumsq/Listened` matches the log; Plays hover matches |
| 5 | Frontend Listened view (the old TODO: newest first, all dates per album, per-year) | visible in `/app` |
| 6 | Retirement steps above | after LATB's OK |

Phase 2 alone fixes the immediate pain (Tidal plays get recorded) and can ship first.

## Decisions (LATB, 2026-11-01) — these override the ❓ marks above

1. **Location:** `~/Sync/clutter/listened/YYYY.jsonl`, configurable through `CLUTTER_LISTENED`.
2. **Keep Discogs field 7 "Plays"** for Discogs releases, permanently, not only during the transition. Keep the increment in the write path (no flag), but only for `KDiscogs` albums. Field 7 then stays a Discogs-side count; clutter's own count comes from the log. Check why the code wraps at 6 (`6 -> ""`): field 7 is probably a Discogs dropdown with fixed options. Ask LATB before changing that behaviour.
3. **Want list → Discogs `2026 Listened`**, in the 2020–2025 format: one list item per release, with the item comment holding all of that release's play stamps, one per line, newest first. ⚠️ The Discogs API treats lists as **read-only** (`OPTIONS /lists/{id}` allows only GET/HEAD; wants allow POST/PUT/DELETE), so items can only be added on discogs.com. The transfer data is prepared in `cache/listened-import/2026-want-to-listened.json` (85 releases, 118 plays as of the 10:17 cache, sorted by first play). **Done 2026-11-01:** all 86 releases / 119 plays transferred via discogs.com (browser), verified against the API; snapshots in `cache/listened-import/`. The same release can already be in the list: 6708911 was, so merge its comment. After the transfer, and only on LATB's OK, delete the play-wants through the API (`DELETE /users/LATB/wants/{id}`). Going forward, clutter stops writing the Want list once the JSONL log is live (Phase 2). Whether clutter should also keep writing the Discogs `YYYY Listened` lists is moot, because the API cannot.
4. **Show every play:** the Listened views list one row per play event, so the same album appears several times, whatever its source (Discogs, Tidal, Apple Music, Jellyfin, later Qobuz). The `getList` vectors are album ids, so a list with repeats needs either a vector that allows duplicates (check that the renderers tolerate repeated ids, since they likely key on id) or a dedicated event-based view that carries the play timestamp per row. The second is preferable: it can show the date in each row.
5. **No manual CLI for now**, but design for it: expose the append as an API call (`POST /api/listened` with a JSON `PlayEvent`, or `/api/albump/:key` accepting a typed key such as `tidal:283130328`) so another client can log plays later. Not needed in the first phases.

6. **Phase 4 shape (LATB, 2026-11-01):** the per-play view is a **separate endpoint** (e.g. `/api/listenedq/:year`), one row per play with its timestamp. Reuse the existing album display infrastructure (`Album` JSON, renderers, frontend album components) as far as possible. The Want-list repeat machinery (`readWantListAids`, `sortByMultipleDates` in `FromDiscogs.hs`, the `"Want"` pseudo-list id 7) is to be removed.

## Implementation notes (Phases 1–2, 2026-11-01)

- `src/Listened.hs`: `AlbumKey` (`KDiscogs | KTidal | KApple`, from `albumFolder`), `PlayEvent` (JSON fields as above, `Nothing` fields omitted, plus `applelib` for Apple albums without a catalog id), `playEvent`, `appendPlay` (starts a new line if the file ends in a torn line), `readPlays`/`readPlayFile` (skip undecodable lines with a stderr warning), `listenedDir` (`CLUTTER_LISTENED`, default `~/Sync/clutter/listened`). `ts` is `%Y-%m-%dT%H:%M:%S%Ez`.
- Test: `test/listened/ListenedTest.hs` (suite `listened-test`, no hspec; `cabal test --enable-tests`, also run by `nix build`). Round trip, torn line, append after a torn line.
- `App.updateAlbumsPlayed`: appends to the log first (a failure is logged to stderr and the rest still runs), adds the day to `listenedDatesR`, then writes the AlbumsPlayed note and the Day One entry as before. Discogs calls go to `updateDiscogsPlayed` and only for `KDiscogs` albums: field 7 is incremented (still wraps at 6; skipped when `albumInst == 0`). The **Want-list write is off** and runs only with `CLUTTER_DISCOGS_WANTS=1` (decision 3). Request bodies are aeson-encoded now.
- `serveAlbump` no longer passes Tidal/Apple albums through `envUpdateAlbum`, which re-read Discogs folders and lists on every call.
- Stderr shows `listened: <key> -> <file>`, and for non-Discogs albums `... is not a Discogs release, no Discogs calls`. Startup logs `logging plays to <dir>`.
- Verified on a test server (port 8081, fake `curl`/`dayone`, scratch log dir): Tidal 2209151 wrote one line and only the cover download; Discogs 28759924 wrote one line and only the field-7 POST.

## Implementation notes (Phase 3, 2026-11-01)

- `scripts/import-listened.py` (stdlib, local files only, no network, no `tok.json`): reads `cache/l<id>-raw.json` for 2020–2025, `cache/listened-import/2026-want-to-listened.json` for 2026, the collection `cache/draw*.json` (inst, Tidal/Apple ids from field 4, field 7), `~/Sync/AlbumsPlayed/*.md` and the live log. Writes `cache/listened-import/out/YYYY.jsonl` + `report.md` + TSVs. Rerunnable; never writes the live log.
- Matching: a list date takes the time of an unused AlbumsPlayed note for the same release (or its Tidal id via field 4) on the same local day, else the same UTC day, else ±1 day; 2026 stamps ±30 min. Notes are UTC up to 2021-07-01T03:02 and local after. Unclaimed notes without a Discogs link are recovered as `import:albumsplayed`; unclaimed notes with one are only reported.
- Edge cases handled and reported: `2025-05-17, 2025-05-14` on one line (both imported, so 2025 has 429 dates against the plan's 428), `2923-03-08` → list year, 2 master items in `2022 Listened` (skipped), duplicate notes in the same minute.
- The `tidal`/`apple`/`inst` of imported Discogs events are today's values from the collection, not necessarily those at play time.
- LATB 2026-11-01: the AlbumsPlayed-only plays are imported (`import:albumsplayed`); the 11 notes without a link are identified in `NOTE_IDS` (10 Discogs releases, Genesis = Tidal 35095630). Note file names are NFD on macOS and are normalised before lookup.
- Result: 2,064 events 2019–2026, every list date, Want stamp and note accounted for; 272 stay date-only (196 in 2020, 12 in 2019, before AlbumsPlayed began).
- `--merge` (server stopped) backs up the live log to `cache/listened-import/live-backup-<time>/`, then rewrites each live year file as its non-`import:` events plus the staged ones, sorted, deduped on (album, minute). Idempotent: a second run leaves the files byte-identical.

## Implementation notes (Phase 4, 2026-11-01)

- `PlayEvent` moved to `Types.hs` (Env holds the log: `playsR :: IORef [PlayEvent]`, replacing `listenedDatesR`); `Listened` re-exports it. `envInit` reads the log (`read N plays from …` on stderr).
- `Album` has `albumListenedDates :: [Text]` (play times, newest first), set by `Listened.attachPlays` whenever albums are (re)stored: `envInit`, `envUpdate`, `envUpdateAlbum`, `envTidalConnect`, and after each play (`envRefreshPlays`). `albumPlays` stays the Discogs field 7 counter (decision 2), and so do the `played.*` tags; the "Played N times" hover (HTML and frontend) now counts the log.
- `Listened.playAlbumId` resolves a play to a displayed album: Discogs release in the collection, else the release carrying its Tidal/Apple id in field 4, else the Tidal/Apple album itself.
- Lists: `getList` serves `Listened` and `Listened YYYY` from the log, one entry per play, newest first, so the existing list views, sorts and focus filters work unchanged (an album played twice is listed twice). They head the List menu. The `"Want"` pseudo-list, `readWantListAids`, `sortByMultipleDates` and `readWWItems*` are gone (the Discogs wantlist client binding and `wants1.json` in `update-json` remain, for Phase 6).
- `GET /api/listenedq/:year` (`all` or `YYYY`, `sortOrder=Asc` for oldest first, `focus=` as for albumsq): same JSON as `albumsq` (`listName`, `lalbums`) plus `lplayed`, the play time per row. Plays of albums clutter does not hold (released sold or never owned, Tidal albums no longer favourites: 81 of 2,066 on 2026-11-01) appear as an album made from the play (`albumID` 0, no cover); albumsq's lists leave them out.
- Frontend: `albumListenedDates` in `Types.purs`, hover from the log, the Listened button shows `Listened`. Phase 5 still has to use `listenedq` to show the date per row.

## Corrections after review (2026-11-01, evening)

- **Collection page 5:** the collection passed 2,000 releases (2,044), but `update-json` and `FromDiscogs` (cache reader and API reader) took only 4 pages of 500. The last 44 releases by label were missing from clutter altogether. Both now take a 5th page. A loop over all pages is still to do.
- **Same play, other pressing:** the importer's second pass gives a list play without a note of its own (date-only, or timed ±30 min) an unclaimed note for another pressing of the same album (loose title match). The list's release is kept. LATB owns some albums twice, so a list play that has its own note never takes a second one. 14 plays were counted twice before.
- **Same minute, same album:** two notes in one minute for the same album title (artist excluded) are one play.
- `RELEASE_ALIASES`: notes linking a pressing LATB does not own → his copy (Brahms–Liszt → CD 24619814, Electric Ladyland → LP 4676923).
- The importer ignores `import:*` events in the live log (a re-import replaces them).
- Result: 2,050 events.

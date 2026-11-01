# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Clutter is a music library management web application with a Haskell backend and PureScript frontend. It integrates with multiple music services (Tidal, Apple Music, Discogs) to provide a unified interface for managing and browsing music collections.

## Build and Development Commands

### Backend (Haskell)

The backend uses Cabal with hpack for package management and is configured for Nix:

```bash
# Build the project
cabal build

# Run the executable (default port 8080, with caching)
cabal run clutter -- -c

# Run with custom port
cabal run clutter -- -c 8080
```

Static assets (`index.js`, `style.css`, `bootstrap.min.css`, `icons/`) are served from
`static/`, resolved **relative to the working directory** and absent from the installed
Nix package — so the server must be started from the source directory, or
`CLUTTER_STATIC` must point at a `static/` directory. It logs the resolved path to
stderr at startup and warns there if `index.js` is missing. Note that `tok.json` and
`cache/` are working-directory-relative too, so the source directory is still the only
place the server starts cleanly.

```bash

# Development with auto-reload
ghcid --command="cabal repl app/Main.hs"

# Enter Nix development shell
nix develop
```

### Frontend (PureScript)

Located in the `frontend/` directory:

```bash
cd frontend

# Install dependencies (runs spago install)
npm install

# Build frontend
npm run build

# Build and serve with hot reload
npm run serve

# Bundle for production (outputs to frontend/index.js, which static/index.js symlinks to)
npm run bundle

# Run tests
npm run test

# Clean build artifacts
npm run clean
```

## Architecture

### Backend Structure

The backend is built using the Servant framework with a ReaderT-based application monad:

- **AppM**: `type AppM = ReaderT Env Handler` - The main application monad providing access to shared state
- **Env**: Mutable references (IORef) for albums, lists, locations, tags, and provider configurations
- **EnvR**: Immutable snapshot of Env for safe reading

Key modules:

- `App.hs`: Defines the Servant API and server implementation. The ClutterAPI combines multiple API types (HTML rendering, JSON APIs for frontend, provider integrations)
- `Types.hs`: Core data types including Album, Release, Env, AppM, SortOrder, MenuParams
- `Provider.hs`: Abstraction layer for reading from different music services
- `Env.hs`: Environment initialization and update operations (envInit, envUpdate, envUpdateAlbum, envTidalConnect)
- `FromDiscogs.hs`, `FromTidal.hs`, `FromAMusic.hs`: Service-specific API integrations
- `Render*.hs`: Lucid-based HTML rendering for different views

### Provider Integration Pattern

Providers (Discogs, Tidal, Apple Music) can operate in two modes:
1. **Session mode**: Direct API calls to the service
2. **File/Cache mode**: Read from cached JSON files in `cache/` directory

All provider data is normalized to the `Release` type, then converted to `Album` for display.

Authentication tokens are read from `tok.json` (git-ignored; `tok.dat` is obsolete). Keys: `amt aky tok usr jky tid tsi tcc tdt` — Discogs token `tok`, user `usr` (= `LATB`), Tidal fields last. **Never print, echo or `cat` it** (`update-json` echoes tokens — redirect its output to a file); read values inside a script.

### Frontend Structure

PureScript application using Halogen or similar framework:

- `Main.purs`: Application entry point
- `Types.purs`: Frontend type definitions mirroring backend JSON responses
- `Render.purs`: UI rendering logic
- `AlbumComponent.purs`: Component for album display
- `GetStuff.purs`: HTTP client for backend API calls

Frontend communicates with backend through JSON APIs under `/api/` routes.

### API Endpoints

**HTML Endpoints** (for direct browser access):
- `/album/:id` - Single album view
- `/albums/:list?sortBy=&sortOrder=&focus=` - Album list view with filtering
- `/app` - Main application view

**JSON Endpoints** (for frontend):
- `/api/albumq/:id` - Get album data
- `/api/albumsq/:list?sortBy=&sortOrder=&focus=` - Get filtered album list
- `/api/paramsq/all` - Get menu parameters (sorts, lists, locations)
- `/api/req?event=` - Trigger backend actions (e.g., "update")

**Provider Update Endpoints**:
- `/provider/discogs?token=&username=&nreleases=&release=` - Update from Discogs
- `/provider/tidal?nalbums=` - Update from Tidal

### Album Organization

Albums are organized by:
1. **Folders**: TagFolder enum (TDiscogs, TTidal, TAMusic, TAll)
2. **Lists**: Named collections from Discogs (e.g., "Box 1", "Cube 3", "Shelf 2")
3. **Tags**: Arbitrary text tags, referenced with `#tag` syntax
4. **Location**: Physical location info for albums with `pLocList` predicate (Box, Cube, Shelf, Incoming, Lost&Found)

Special list handling:
- Tidal and Apple Music albums that also exist in Discogs show the Discogs ID
- The "All" list includes everything
- The "Discogs" list excludes Tidal/Apple Music-only albums

### Sorting and Filtering

Sorting is implemented through `getSort` function in Env, which returns different sort functions based on sort name.
Filtering uses focus items (tags prefixed with `#`), with negation support using `-#tag` syntax.

### Integration with External Tools

The app integrates with external tools when albums are played:
1. **Obsidian**: Writes notes to `AlbumsPlayed/` directory as markdown files
2. **Day One**: Uses `dayone` CLI to create journal entries with album cover images

These integrations are triggered via the `/api/albump/:id` endpoint.

## Project Conventions

- **Prelude**: Uses `relude` instead of standard Prelude (NoImplicitPrelude extension)
- **String types**: Primarily uses `Text` from `Data.Text`
- **Extensions**: OverloadedStrings and NoImplicitPrelude are default extensions
- **Error handling**: Uses Maybe/Either patterns, some areas use `error` for impossible states
- **Servant**: API routes use type-level DSL with `:<|>` combinators
- **HTML rendering**: Uses Lucid library for type-safe HTML generation
- **JSON**: Aeson for JSON serialization with Generic deriving

## Listened / play tracking — current state and planned change

**Current mechanism (a kludge, to be replaced):** pressing *Album Played* (`/api/albump/:id` → `App.updateAlbumsPlayed`) does five things:
1. writes an Obsidian note `AlbumsPlayed/<YYMMDD>-<artist> – <title>.md`
2. `PUT`s the release onto the Discogs **Want list**
3. prepends `YYYY-MM-DDTHH:MM` to the want's notes
4. increments the Discogs collection custom field **7** ("Plays", wraps at 6)
5. creates a Day One entry in the journal "Albums Played", with the cover staged in the Day One group container

The "Listened" menu shows the pseudo-list `"Want"` (id 7), sorted by those note dates. Earlier years live in the Discogs lists `2020 Listened` … `2025 Listened`, with dates in the item comments. The `albumListenedDates` field and `extractListenedDates` exist but are **disabled** (`listenedDatesMap = M.empty` in `Env.hs`; field commented out in `Types.hs`).

Known defects: Tidal-only albums have a Tidal id in `albumID`, so their plays go to `/wants/<tidal id>` and are lost, or land on an unrelated release. The Want list is read only up to 500 entries. The notes JSON is hand-built and escapes only newlines.

**State on 2026-11-01:** LATB's decisions are recorded in the plan. All 2026 plays so far (86 releases, 119 stamps) were moved from the Want list into the Discogs list `2026 Listened` (1656131), and the Want list was emptied. Phase 2 is implemented, but not yet deployed: once rebuilt, plays go to `~/Sync/clutter/listened/YYYY.jsonl` (`src/Listened.hs`), and the Want list is no longer written (`CLUTTER_DISCOGS_WANTS=1` turns it back on). The running server still writes the Want list until it is restarted. Snapshots are in `cache/listened-import/`.

**State on 2026-11-01, evening (Phases 1–4 built):** plays go to clutter's own log `~/Sync/clutter/listened/YYYY.jsonl` (2,066 events 2019–2026 after the backfill). The menu lists `Listened` and `Listened YYYY` and `GET /api/listenedq/:year` read it, and the plays hover counts it. The Want list is no longer written, and its pseudo-list is gone. The description of the old mechanism above is history.

**Plan:** `docs/listened-plan.md` replaces this with a per-year JSON Lines play log owned by clutter. It covers storage, typed album keys, the write and read paths, backfill from the Discogs lists + Want notes + `AlbumsPlayed` + Day One, the retirement of the Discogs writes, and the open questions for LATB. Read it before touching any play-tracking code.

## Running it on lmini (operations)

- **Start:** since 2026-11-01 LATB runs it as `cd ~/clutter && nix run` in a **herdr pane**; herdr keeps the session, so abduco is no longer used. `nix run` rebuilds first if the sources changed. Stop it with `Ctrl-C` in that pane. After a reboot, start it again the same way. The Day One cover write needs the "access data from other apps" permission of a GUI-started process. lmini's herdr server runs as the launchd agent `local.herdr.server`, so its panes have it. Started over SSH, or inside a herdr server that a remote client started, curl exits 56 and entries get no art. The older way, `abduco -n clutter ./result/bin/clutter` from a local Ghostty window, still works.
- **Tidal token:** no separate step any more. `./update-json` runs `./tidalauth` first, and the server starts in cache mode by default, reading Tidal favourites from `cache/traw.json`. `tidalauth` is plain Python since 2026-10-31: it refreshes silently from `tidal-session.json` (git-ignored, mode 600), and `--login` forces a new device link. Run it by hand only before live Tidal calls from the server (`/provider/tidal`). Tidal access tokens live **4 h** (`expires_in: 14400`), and the server reads `tok.json` only at startup, so those calls die 4 h after launch. Making it re-read or refresh the token is an open thread (`Env.hs`).
- **Refresh data:** `./update-json` fills `cache/` (Discogs collection pages `draw1..4.json`, folders, lists `l<id>-raw.json`, `wants1.json`, Tidal favourites `traw.json`). A stale `traw.json` fails with *key "limit" not found*. For **new Discogs additions** while the server runs, `curl 'localhost:8080/api/req?event=update'` (or the <Update> button on a Tidal album view) pulls the last 20 releases, with no restart.
- **Build:** backend `nix build` → `result/bin/clutter`. `package.nix` passes only `src/ app/ test/ clutter.cabal package.yaml README.md LICENSE` to the build (since 2026-11-01), so edits to docs, scripts or the frontend no longer force a rebuild. A new top-level file the build needs must be added to that list. The frontend builds in its **own** flake: `cd frontend && nix develop --command bash -c 'npm install && npm run bundle'`. The root dev shell has no `spago`. `frontend/spago.yaml` needs the `now` dependency.
- **Paths:** `tok.json`, `cache/` and `static/` resolve relative to the working directory, so start the server from `~/clutter`. `CLUTTER_STATIC` overrides `static/`. Startup diagnostics go to **stderr**, because stdout is block-buffered once redirected.
- `AlbumsPlayed` → `/Users/latb/Sync/AlbumsPlayed` since 2026-11-01. It moved out of the Obsidian vault and was `~/Notes/AlbumsPlayed` before. On lmini, home is `/Users/latb`; `/home/latb` is only a compatibility symlink, so write `/Users/latb` paths.
- **Backups:** the code is on GitHub (`LATBauerdick/clutter`). `tok.json` and `cache/` have **no backup**, so keep a copy of `tok.json` elsewhere. `cache/` can be regenerated.
- Full install/recovery recipe: `~/Notes/HowTo/HowTo install and run clutter.md`. Its older section still contains a Discogs token in plain text.

## Streaming IDs on Discogs releases

A Discogs release links to its streaming versions through the collection **Location** field (field **4**), value `T<tidal album id> A<apple album id>`. clutter parses it (`albumTidal`/`albumAMusic`), and `updateTidalFolderAids` then shows the Discogs release instead of the Tidal favourite.
- Write: `POST /users/LATB/collection/folders/{folder}/releases/{release}/instances/{instance}/fields/4` with body `{"value": "T… A…"}`.
- Releases bought only to anchor a stream go in the folder **Streaming** (3292597). Add one with `POST /users/LATB/collection/folders/3292597/releases/{id}`, which returns the `instance_id`.
- Matching: Tidal `GET https://api.tidal.com/v1/albums/{id}?countryCode=US` (Bearer `tdt`) gives title, UPC and tracks. Apple: `https://itunes.apple.com/lookup?upc=<UPC>&entity=album`, falling back to a text search. **Tidal's digital UPC usually differs from the physical barcode**, so match the Discogs release by cover + tracklist and use the barcode only as a cross-check. If the Tidal token has expired, Tidal search still works with the public web-client `x-tidal-token`. Now-playing can be read from the TIDAL window title via System Events. Album links for LATB: `tidal://album/<id>`.
- Done so far: Nikolayeva MK 418013 (9791348 → `T12571482 A206349046`), Nikolayeva Relief CR 861006 (17105709 → `T366233911 A1822507817`), Richter Olympia OCD 336 (18907429 → `T60901746 A1116300345`), Levit Sonatas box (15079008, by LATB), Levit *Fantasia* (28759924 → `T315583200 A1694557348`), Yudina Legacy Vol. 6 (6715388 → `T283130328 A1556465220`, an approximate match: the Tidal album is the RCD Schubert/Honegger/Bartók disc, which Discogs does not have).
- A UI for this lookup is a parked idea.

## History and lessons (from the aiws workspace logs, 2026-09 → 2026-11)

- **2026-09-14 AlbumsPlayed analysis**: 1,740 plays from 2020-12 on; report at `~/Notes/Claude/2026-09-albums-played-report.md`. ⚠️ Note timestamps (frontmatter `date:` and the file name prefix) are **UTC up to 2021-07-01 03:02** and local America/Chicago from 2021-07-01 22:42; 359 entries are affected. The **Day One "Albums Played" journal is the authoritative clock** (`DayOne.sqlite` in the group container, with `ZTIMEZONE`). Copy it before reading it.
- **2026-09-20 Day One covers**: `install_cli.sh` replaced `/usr/local/bin/dayone` with a symlink to the App-Sandboxed in-bundle CLI, which cannot read `/tmp`. Covers are now staged at `~/Library/Group Containers/5U8NS4GX82.dayoneapp2/__ac.jpg`. `curl -fsSL` keeps Discogs rate-limit bodies from being attached as JPEGs, and `-a` is passed only after a successful download (`e276cd5`).
- **2026-09-21 `/app` blank page**: the cause was the trailing slash plus relative asset URLs. They are now root-absolute, with `Cache-Control: no-cache` on js/css, a vendored bootstrap 4.4.1 and `CLUTTER_STATIC` (`51fe506`). `directory`/`filepath` had to be added to **both** `package.yaml` and `clutter.cabal` by hand, because the shell's hpack 0.38.3 differs from the 0.36.0 that generated the cabal file. This relude does not re-export `hPutStrLn`, so import `System.IO`.
- **2026-10-07**: rebuilt on lmini after the 10-04 disk wipe; the repo had been in neither Time Machine nor Arq. **2026-10-08**: icon 404s fixed (`static/icons/`), streaming badge changed `far fa-wifi` → `fas`.
- **2026-10-11/13**: cover failures came from a herdr server started over Tailscale SSH (TCC context). Fixed by the abduco-from-Ghostty start and later by the `local.herdr.server` launchd agent.
- **2026-10-31**: `tidalauth` rewritten in plain Python and now keeps the refresh token (`9ce41d7`). The old version discarded it, which is why the token was always stale.
- Stray checkouts: `~/Dev/clutter` copies are a stale 2022 checkout. lair's untracked `h.mcp/`, `neu.json` and `refac.md` were saved to `~/_from-lair-2026-10-11/`.

## Working rules for Claude in this repo

- **Do not `git commit` or push unless LATB says so.** LATB rebuilds, restarts the live server and commits. Say what needs a rebuild or restart.
- Never print secrets (`tok.json`, `tidal-session.json`). Don't send messages or post anything on LATB's behalf.
- Ask before writing to Discogs beyond what LATB requested: Want list clean-ups and list edits are his call. Reads are fine.
- Validate by building (`cabal build` / `nix build`) and by hitting the endpoints of a test server on another port (`cabal run clutter -- -c 8081`). Don't restart the live one on 8080.
- Facts that the other assistant (Hermes) or later aiws sessions need go into the aiws daily log (`~/aiws/memory/YYYY-MM-DD.md`), not only here.

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

# Bundle for production (outputs to ../backend/static/index.js)
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

Authentication tokens are read from `tok.dat` file (not in version control).

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

## Recent Changes and Next Steps

### Listened Dates Feature (Completed)
The backend now extracts listened dates from Discogs "Listened" lists (lists with names ending in "Listened"). The implementation:
- Parses list item comments to extract dates (format: "YYYY-MM-DD")
- Stores dates in the `Album` type as `albumListenedDates :: [Text]`
- Automatically loads this data during app initialization from cache or API
- Available via all JSON API endpoints that return album data

Key files modified:
- `Types.hs`: Added `albumListenedDates` field to Album, added `listenedDatesR` to Env
- `FromDiscogs.hs`: Updated `WAid` to include comment field, added functions to read list items with comments
- `Env.hs`: Added `extractListenedDates` function to parse "Listened" lists
- `Provider.hs`: Updated all Album constructors to include empty listened dates initially

### TODO: Frontend View for Listened Dates
**Next task**: Create a frontend view that displays albums sorted by their listened dates.

Implementation considerations:
- Backend already provides `albumListenedDates: [Text]` in Album JSON
- Available through `/api/albumsq/:list` endpoint
- Consider creating a new route (e.g., `/listened` or `/history`)
- Sort albums chronologically by most recent listen date
- May want to show all listen dates for each album (albums can have multiple dates)
- Could create a timeline view or a simple sorted list
- Frontend files to modify: `Main.purs`, `Types.purs`, `Render.purs`, possibly new component file

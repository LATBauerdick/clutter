# AGENTS.md

Agent guidance for the Clutter music library management app (Haskell backend + PureScript frontend).

## Build Commands

### Backend (Haskell)

```bash
cabal build                          # Build the project
cabal run clutter -- -c              # Run on port 8080 with caching
cabal run clutter -- -c 8080         # Run on custom port
ghcid                                # Dev with auto-reload (uses .ghcid config)
cabal repl app/Main.hs               # Start GHCi REPL
nix develop                          # Enter Nix dev shell
```

### Frontend (PureScript, run from `frontend/`)

```bash
npm install          # Install deps (also runs spago install)
npm run build        # Build (spago build)
npm run serve        # Build + hot reload dev server
npm run bundle       # Production bundle → ../backend/static/index.js
npm run test         # Run tests (spago test)
npm run clean        # Remove all build artifacts
```

### Linting and Formatting

```bash
hlint src/           # Lint Haskell (default .hlint.yaml config)
ormolu               # Format Haskell (available in Nix shell)
```

### Tests

There are currently **no Haskell tests**. The test stanza in `package.yaml` is commented out.
If adding tests: the project plans to use `hspec`, `hspec-wai`, `hspec-wai-json`, and `aeson`.
PureScript tests: `npm run test` (from `frontend/`), which runs `spago test`.

To run a single PureScript test (if using `spec` style): `spago test --main Test.MyModule`.

---

## Project Architecture

- **Backend**: Haskell with Servant (REST API + HTML rendering via Lucid), `ReaderT Env Handler` monad
- **Frontend**: PureScript with Halogen, communicates via JSON APIs under `/api/`
- **Providers**: Discogs, Tidal, Apple Music — normalized to `Release` → `Album`
- **Env**: mutable `IORef` fields; `EnvR` is an immutable snapshot for reading
- **Package management**: `hpack` (`package.yaml` → `clutter.cabal`) + `cabal`

---

## Haskell Code Style

### Language Extensions

Declare per-file at the top. Project-wide defaults (from `package.yaml`): `NoImplicitPrelude`, `OverloadedStrings`.
Common per-file extensions:

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
```

Do not repeat `NoImplicitPrelude` or `OverloadedStrings` in file headers unless needed for clarity (they are project defaults).

### Prelude

Use `relude` instead of standard `Prelude`. Import as:

```haskell
import Relude
-- or to exclude specific names:
import Relude hiding (ord)
```

`relude` provides: `Text`, `one`, `viaNonEmpty`, `putTextLn`, `readMaybe`, `toText`, `toString`, etc.

### Imports

Group in order: standard library, third-party, internal. Use qualified imports with explicit item lists:

```haskell
import qualified Data.Map.Strict as M
import qualified Data.Vector as V (empty, fromList, toList)
import qualified Data.Text as T (find, stripPrefix, intercalate)
import Data.Text as T (replace, stripPrefix, unlines, unpack)

import Types (Album (..), AppM, Env (..), EnvR (..), envGetEnvr)
import Env (envInit, envUpdate, envGetTag)
```

Internal import lists use trailing ` )` on its own line with a leading space:

```haskell
import Types (
  Album (..),
  AppM,
  Env (..),
 )
```

### Module Exports

Explicit export lists, 2-space indented, trailing comma style:

```haskell
module Types (
  Album (..),
  Env (..),
  AppM,
  pLocList,
) where
```

### Naming Conventions

| Category | Convention | Examples |
|----------|-----------|---------|
| Types / data constructors | PascalCase | `Album`, `TagFolder`, `SortOrder` |
| Type aliases | PascalCase | `AppM`, `Token`, `UserName` |
| Record fields | camelCase with type prefix | `albumID`, `albumTitle`, `albumArtist` |
| `IORef` fields in `Env` | suffix `R` | `albumsR`, `listNamesR`, `sortNameR` |
| JSON response types | suffix `J` | `AlbumJ`, `AlbumsJ`, `ParamsJ` |
| Discogs wire types | prefix `W` | `WRelease`, `WList`, `WFolders` |
| Env operations | prefix `env` | `envInit`, `envUpdate`, `envGetEnvr` |
| Read operations | prefix `read` | `readAlbum`, `readAlbums` |
| Render operations | prefix `render` | `renderApp`, `renderAlbumView` |
| Predicate helpers | prefix `p` | `pLocList` |

### Record Construction

Use named fields for clarity:

```haskell
pure Env
  { albumsR = ar
  , listNamesR = lnr
  , sortNameR = sr
  , url = "/"
  }
```

### Error Handling

- Use `Maybe` for optional values; pattern-match or use `fromMaybe`
- Use `Either String X` for fallible API calls
- Use `viaNonEmpty head` (not partial `head`)
- Use `readMaybe` (not `read`)
- Log IO errors then continue with defaults:

```haskell
case res of
  Left err -> putTextLn $ "Error: " <> show err
  Right _  -> pure ()
```

### `do` Blocks and Let Bindings

Annotate `let` bindings with types for clarity:

```haskell
let tagsMap :: Map Text [Int]
    tagsMap = foldr updateTags M.empty (M.elems albums')
```

Use `where` clauses for local helpers. End `do` blocks that return unit with `pure ()`.

### Servant API Definition

```haskell
type API0 =
  "album"
    :> Capture "aid" Int
    :> Get '[HTML] RawHtml

type ClutterAPI = API0 :<|> API1 :<|> API2 :<|> Raw
```

### Lucid HTML

```haskell
renderNav env = L.div_ [L.id_ "navbar"] $ do
  L.button_ [L.class_ "dropbtn"] $ L.toHtml "Menu"
  L.div_ [L.class_ "dropdown-content"] $
    F.traverse_ (\x -> L.a_ [L.href_ x] $ L.toHtml x) items
```

### Comments

Section separators: `------` or `--------------------`. Leave commented-out code in place rather than deleting.

---

## PureScript Code Style

### Module Exports

```purescript
module Types ( Album
             , AlbumJ
             , SortOrder (..)
             , State
             ) where
```

### Imports

Group: prelude, data types, effects, framework, local:

```purescript
import Prelude
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Effect.Aff (Aff)
import Halogen as H
import Types (Album, State, Action(..))
```

### Type Definitions

Use record type aliases:

```purescript
type Album = { albumID :: Int
             , albumTitle :: String
             , albumArtist :: String
             }
```

ADTs with `derive instance` for common type classes:

```purescript
data SortOrder = Asc | Desc
derive instance eqSortOrder :: Eq SortOrder
derive instance genericSortOrder :: Generic SortOrder _
instance showSortOrder :: Show SortOrder where show = genericShow
```

### Naming

- Types: PascalCase — `Album`, `AlbumList`, `SortOrder`
- Values/functions: camelCase — `albumID`, `renderTopMenu`
- Action constructors: PascalCase verbs — `ToggleSortOrder`, `ShowAlbum`, `MakeRequest`

### Halogen Components

```purescript
aComponent :: forall query input output m. MonadAff m => State -> H.Component query input output m
aComponent is = H.mkComponent
  { initialState: const is
  , render
  , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
  }
```

### Error Handling

Decode JSON with `Either` and use defaults on failure:

```purescript
let sje = (decodeJson =<< parseJson raw) :: Either JsonDecodeError ParamsJ
let result = case sje of
               Right { params: ps } -> ps
               Left _ -> defaultMenuParams
```

### HTML Rendering (Halogen)

```purescript
renderAlbumInfo a =
  HH.div [ HP.class_ $ HH.ClassName "album-info" ]
         [ HH.p [ HP.class_ $ HH.ClassName "album-title" ] [ HH.text a.albumTitle ]
         ]
```

Use 2-space indentation. Properties and children are always separate lists.

### `where` Clauses

Use heavily for local helpers in render functions and component logic.

---

## Key Dependencies

| Haskell | PureScript |
|---------|-----------|
| `relude` (Prelude replacement) | `halogen` (UI framework) |
| `servant`, `servant-server` | `affjax-node` (HTTP) |
| `lucid` (HTML) | `argonaut-codecs` (JSON) |
| `aeson` (JSON) | `halogen-css` |
| `warp` (server) | `formatters` |
| `time`, `process` | `datetime` |

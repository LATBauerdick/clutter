# Jellyfin project — plan

Status 2026-11-25, evening: inventory run; **slice 1 built** (below), sources
in the working tree, uncommitted, **not live** — needs a restart of the server
and then the new frontend bundle. Hub: `~/Notes/Claude/ws/clutter.md`.

## Slice 14 — Update from the server (built 2026-11-25, night)

Uncommitted (with slice 13); bundle installed. **Live after a restart.**

- Three indexes of the same files: Jellyfin (scans by itself), clutter's store
  (`jellyfin-albums.json`, only when the script runs), the Node's library (only
  when the player re-indexes). Nothing carried a change past Jellyfin.
- ⋯ menu › **Update from the server** (`/api/req?event=jellyfin`): runs
  `scripts/update-jellyfin-albums.py`, then `Env.envJellyfinReload` takes the
  store in without a restart — new file albums come, vanished ones go (also off
  the albums they were versions of), playing times are read again — and shows
  the server list. The answer counts new, gone, and **links that point at files
  that are gone** (a moved or renamed folder gets a new Jellyfin id; the link,
  keep mark and plays stay on the old one). The script's last error line is
  shown if it fails.
- Tested on 8084 with a scratch store and a stand-in script: 1 new, 3 gone, 1
  dangling link; the new album is listed with its tags, the linked release lost
  its files and storage tag; failure of the script is reported. **Not tested:**
  the real script from the server (it could not reach Jellyfin from the test
  server), the menu entry in a browser.
- Not built: re-attaching dangling links by artist and title (needed before
  folders are reorganised); asking the Node to re-index; a nightly run.
- Also: queue next / last on the album page are icons now; the Tidal / Files
  chooser and the "Files" word sit beside the button group, not in it.

## Slice 13 — files on the Node by folder (built 2026-11-25, night)

Uncommitted; bundle installed (works with the slice-12 server). **Live after a
restart.** Slice 12 is committed (`7f36fa9`).

- LATB hit the text match's weak side at once (*Istanbul*, not in the share →
  "not in the player's library", but the button was offered). Now by folder,
  which is exact:
  - The player lists its shares as the top of its folder browse
    (`/library/v1/Folders?service=LocalMusic` → `//umini/media/music/Music`) and
    queues a folder with everything below it: `/Add?service=LocalMusic&path=
    <share>/<folders>` ("Play now" of the Folder context in `/Services`).
  - clutter knows each file album's folder from the Jellyfin store (`path`,
    `Jellyfin.jellyfinPaths`, re-read when the store file changes). The share
    is laid over the Jellyfin path where the end of the share's name occurs in
    it (at least two folder names, or all but the host): `App.folderOn`. No
    configuration, no host names in code.
  - `/api/nodeplayfiles` plays by folder where a share holds the album, else by
    album and artist tag as before. `/api/albumxq` says `xnode`; the page offers
    Files only then. The shares are asked for at most every two minutes
    (`nodeShares`); an unreachable player has none.
- Tested on 8084 against a stand-in for the player (`fake-node.py` in the
  scratch copy): the paths sent for a file album, one with `&` and umlauts, a
  release with files; an album outside the share falls back to tags and is not
  offered. **Not tested on the Node**: `/Add?path=` itself — it was re-indexing
  LATB's new share (all of Music) and answered "Nothing here can be played" to
  everything. To check when indexing is done: that a box set's disc folders
  come along and in order, and that the Tidal mode parameters (`clear=1`,
  `where=nextAlbum`) do for a folder what they do for an album (the folder
  menu itself uses `where=next`).
- While the player indexes, its library is empty: Files is offered (the share
  is listed) but nothing plays — the message says so.

## Slice 12 — playing files on the Node (built 2026-11-25, night)

Uncommitted; bundle installed. **Live after a restart.**

- LATB: Roon is out (subscription to be cancelled). Route chosen: the **Node's
  own library** — a network share of umini indexed by the player (BluOS app ›
  Settings › Music Library). UPnP: the Node is not a renderer (from memory, not
  verified); AirPlay only from a Jellyfin client, CD quality.
- Trial share: `\\umini\media\music\Music\_Classical\_Piano` → the Node indexed
  8,853 songs (ALAC, MP3, FLAC, AAC; all 16/44.1 or 16/48); all 348 Jellyfin
  albums under `_Piano` have their files there and the Jellyfin title equals
  the Node's album tag (9 folders hold several album tags).
- The player knows a local album by **album and artist tag**, not by an id:
  `/Add?service=LocalMusic&album=…&artist=…` + the same mode parameters as
  Tidal. Test play by hand worked (Barenboim, Liszt–Wagner; queue of 6, ALAC
  16/44.1). An album it does not have: `<error>Nothing here can be played`.
- `GET /api/nodeplayfiles/:id?mode=replace|now|next|last[&version=J…]`: a file
  album, or an album with file versions (the first, or the one named); sends
  the file album's title and artist (`bluosAddLocal`). Album page: **▶ <node>
  · files** with now / next / last, next to the Tidal ▶ where both exist.
- Tested on 8084: refusals (no files, unknown, wrong version) and the
  unreachable-player answer. **The call from clutter to the Node is not
  tested** — a server started from Claude's shell has no LAN access; first
  real click is LATB's, after the restart.
- Known limits of matching by text: two albums with the same title and artist
  (367 such pairs in Jellyfin: MP3 next to FLAC, `_1001` copies) are one album
  to the Node and would be queued together; album artist vs the Node's artist
  tag may differ outside this folder. Exact alternative if it bites: play by
  file (`/Add?service=LocalMusic&file=<fn>` per song, `fn` from
  `/library/v1/Songs?…&start=&end=`, paged — unpaged is empty), matching the
  Node's folders to Jellyfin's paths.
- Open: DSD and hi-res on the Node (none in `_Piano`); the whole library's
  size; a read-only Samba user for the Node; whether a ▶ on the Node should
  log a play (today it does not, as for Tidal).

## Slice 11 — bulk Keep in a list (built 2026-11-25, late)

Uncommitted (with slice 10); bundle installed. **Live after a restart.**

- List toolbar: **Select** (becomes "Done"). In select mode a click on a cover
  selects the album instead of opening it (outlined, dimmed); a bar shows
  "n selected", **All n shown** (everything the focus and the search box
  leave), None, and **Keep · Not for me · Take mark off** for the selection.
  After a mark the list is loaded anew (in the Inbox the marked albums are
  gone) and the selection is empty; select mode stays on until Done or until
  another view opens. The selection is emptied whenever the list is loaded
  (focus, sort).
- `POST /api/albumskeep?value=yes|no|unset`, body: JSON array of album keys
  (thousands do not fit in a URL; `GetStuff.postStrings`). One `keep` line in
  the log per album whose mark changes, one recomputation of the tags
  (`Env.envSetMarks`); unknown keys and unchanged marks are counted in the
  answer, not written.
- Tested on 8084 with a scratch log: 3 albums yes / again (nothing written) /
  no / unset, tags and list sizes follow; bad value, empty list, a malformed
  key (HTTP 400); 3,000 Inbox albums kept and unmarked again in 0.2 s each.
  **Not tested:** select mode in a browser.

## Slice 10 — review of the unsure matches (built 2026-11-25, late)

Uncommitted; bundle installed (`frontend/index.js`). **Live after a restart**
(until then the sidebar entry answers "no answer from the server").

- Sidebar, On the server › **Unsure matches**: one block per file album the
  inventory matched but did not link for sure (grade not `sure`, or several
  clutter albums fit) — title, artist, year, minutes, storage location, folder,
  a Jellyfin link, why it is unsure — and under it up to five albums it may be:
  the inventory's choice first (a Tidal album that a release has → that
  release), then the title rule of "Same album as…". **Link** per album,
  **None of these** per block; either way the block goes. Names open the
  albums; Back returns to the review (`VReview`), which is asked for anew.
- `GET /api/reviewq` reads `cache/jellyfin-inventory/link-candidates.tsv`
  (`CLUTTER_INVENTORY` overrides) each time; leaves out file albums the log has
  a `link`, `unlink` or `skip` line for (`Collection.reviewedFiles`) and those
  not in the Jellyfin store. Order: artist differs → several fit → title only.
- `GET /api/albumskip/J…` appends `{"op":"skip","version":"J…"}` to the
  collection log (new op; readers that do not know it leave it alone — the
  scripts too). A skipped album can still be linked by hand on its page.
  Link is the existing `/api/albumlink`.
- Numbers: 234 unsure rows in the TSV → **193 to review** (39 are ghost
  entries not in the store, 2 were linked by hand already): 58 artist differs,
  48 two fit, 87 title only (mostly wrong, as sampled before — "None of
  these" is the usual answer there).
- Tested on 8084 with a scratch copy of the log: list, link (row gone, album
  has the files and the storage tag), skip (row gone, log line), refusals
  (a release, an unknown id), bundle and style served. **Not tested:** the
  page in a browser.
- The sandbox cannot reach localhost any more and cannot link the binary:
  build and test ran outside it (`cabal build --offline exe:clutter` in the
  scratch copy; frontend with `purs compile … -g js,corefn` + `purs-backend-es
  bundle-app`, spago itself fails on its cache database).

## Slice 9 — a file album and its stream (built 2026-11-25)

Uncommitted (with slices 7 and 8), bundle installed; **live after a restart.**

- LATB's case: Moby *18*, files in Music, no Discogs release; Tidal has
  *18 & 18 B-Sides* (T246075966, in the store). Nothing on the file album's
  page let him add the Tidal id.
- Two ways now, either one, not both (each refuses while the other holds):
  - **Same album as…** on either page — the title rule now also takes a short
    title inside a longer one when an artist word is shared ("18" in "18 & 18
    B-Sides"): the files become a version of the Tidal album, which stands.
  - **Link a stream** on the file album's page (as on a release): the file
    album stands and has the Tidal album — for a stream that is not in
    clutter, or when the files are what LATB thinks of as the album.
- For that a file album can hold streams like a release (`Collection.holds`;
  stand-ins, the Tidal list and the plays follow: `Provider.standIns`,
  `updateTidalFolderAids`, `Listened.playAlbumId`).
- Tested on 8083 with exactly this pair, both ways and back.

## Slice 8 — the log is the only source of links (built 2026-11-25)

Uncommitted (with slice 7), bundle installed; **live after a restart — and the
import below has already run, which the restart needs.**

- LATB: become independent of the `T<id>` / `A<id>` info in Discogs — parse it
  once, from then on only what is on disk; no more Location info in Discogs
  (inconsistent, missing on most releases). Scrap the 18 Apple library ids
  (never worked) and the old file paths; keep Qobuz for a later extension;
  leave Discogs untouched, just unused.
- **Import done**: `scripts/import-location-links.py` appended 1,811 links
  (`via: discogs-location`) to `~/Sync/clutter/collection.jsonl`: 895 Tidal,
  860 Apple Music, 28 Qobuz, 28 file albums. Backup of the log from before:
  `collection.jsonl.before-location-import`. Repeatable; a release added to
  Discogs later with ids in Location is not picked up unless it is run again.
- `FromDiscogs` no longer reads T/A/Q/J from field 4 (the old parsers stay as
  `…Loc`, unused); the rest of the field (box numbers `C…`, free text) is read
  as before, an old file path at its end is cut off.
- Log model: streams are kept **per release** (`Streams = release → [stream]`,
  one Tidal, one Apple, one Qobuz), so the vinyl and the CD of an album can
  have the same stream (38 Tidal ids were on more than one release). An
  `unlink` line names the release. New key `Q<id>` (`AlbumKey` `Q Text`).
- `attachLinks` sets `albumTidal` / `albumAMusic` / `albumQobuz`, the provider
  tag and the Wikidata key (`t:`, `a:`) from the log.
- Every link has its × now; taking a replaced edition off leaves the release
  without that stream (there is no Discogs value to fall back to any more).
- Scripts read the log (`scripts/collection_log.py`): `update-lengths.py`,
  `update-tidal-albums.py`, `jellyfin-inventory.py`,
  `import-inventory-links.py`. **Not run** after the change — compiled only.
- Tested on 8083 against the live server (which still reads Discogs), release
  by release, 2,033 releases: same lists (Collection, Inbox, All, Discogs,
  Server); Tidal list +1 (D4157426, a tidal.com link the old parser missed);
  differences only where intended — 18 Apple library ids gone, 43 path texts
  gone, 2 bogus Qobuz values gone ("Q1234", "Quartet…"), 6 releases show a
  different first file album. Link / replace / unlink of Tidal and Qobuz on a
  release; test suite.

## Slice 7 — from the stream album's page; the log goes before Location (built 2026-11-25)

*(The "log goes before Location" rule and the undo that restored the Location
id lasted an hour: slice 8 makes the log the only source.)*

Uncommitted, bundle installed; **live after a restart.** Slice 6 is committed
(`902a400`).

- LATB's case: Tidal album *Dead Meat* (T533112642, a favourite since 11-15)
  in the Inbox, the vinyl D29080564 on the shelf — whose Location names an
  older Tidal edition, T235035298, not in the album store.
- **Same album as…** now also on a Tidal / Apple album that stands alone: the
  releases with a fitting title, also those that have another edition of the
  stream ("has T235035298, to be replaced"). Link → the release's page.
- **The log goes before Location** (changes slice 6): a stream linked in
  clutter takes the place of the one the Location field names. The box "Link
  a stream" is on every release. Taking the link off brings the Location id
  back (`envSetStream` clears it; the next read of the release restores it —
  at once with a live Discogs session, as the album page re-reads the
  release).
- Tested on 8083 with exactly this pair: offered, linked ("in place of the
  one it had"), the Tidal album leaves the Inbox, unlink restores
  T235035298. Sample: 21 of 200 inbox Tidal albums get a release offered;
  classical ones with generic titles are noisy, as for files.
- Open: several editions of a stream per album (old and new Tidal id both
  kept, as versions) — part of the album layer.

## Slice 6 — a release's stream, linked by hand (built 2026-11-25)

Uncommitted, bundle installed; **live after a restart.** Slice 5 is committed
(`a744773`) and live.

- LATB: a way to add a Tidal and/or Apple Music link by hand — paste a share
  link or `T<id>` / `A<id>`.
- Kept in clutter's log, **not written to Discogs**: the same `link` /
  `unlink` lines, with a `T…` or `A…` key as `version` and the release as
  `key`. One Tidal and one Apple album per release; a new one replaces the
  old. The Location field wins where it names one (then the page offers no
  box for that kind, and the endpoint refuses).
- `Collection.parseStreamRef` reads `T…`, `A…`, `tidal://album/…`,
  `tidal.com/album/…`, `listen.tidal.com/album/…[/track/…]`,
  `music.apple.com/<cc>/album/<name>/<id>[?i=…]`.
- `attachLinks` sets `albumTidal` / `albumAMusic` and the provider tag from
  the log, so everything that follows from those ids follows: ▶ on the Node,
  the Tidal list, stand-in for the Tidal album, plays, length.
- `/api/albumstream/:id?ref=…&op=link|unlink`; album page of a release, block
  **Link a stream**: the box where a Tidal or Apple album is missing, and the
  log-linked ones with ×.
- Tested on 8083: all six link forms, replacement, refusals (junk, a release
  with Location streams, a Tidal album as target), unlink, restart reads the
  links back. **Not tested:** the box in a browser. The id is not checked
  against Tidal or Apple — a wrong number links a wrong album (the page then
  shows it; × takes it off).

## Slice 5 — files shared across releases of one master (built 2026-11-25)

Uncommitted, bundle installed; **live after a restart.** Also uncommitted:
the Keep button is hidden on an album that is in by the rule (frontend only).

- LATB's case: *Before The Flood* as vinyl (D375065) and CD (D2691592), same
  Discogs master, files in Music and in HiRez. A file album still links to
  **one** release (stand-in, plays). But releases of one master
  (`Collection.masterKey`, the `dm:<id>` key) now share the **storage tags**
  (`attachLinks`), and the page of each shows the files of the others under
  "On the server" as "… on the Vinyl" (click opens that release; no × there —
  the link is taken off on the release that holds it) (`App.filesOf`,
  `fvOwner`).
- Tested on 8083: both releases before and after linking the hi-res files to
  the CD; test suite. **Not tested** in a browser.
- This is the stop-gap for the album layer of the model. The full step — one
  row per album in the lists, with its versions (records, streams, files)
  underneath, plays and locations per version — is still to design.

## Slice 4 — linking by hand (built 2026-11-25, evening)

Sources in the working tree, uncommitted; bundle installed. **Live after a
restart.** Slices 1–3 are committed (`d8e89c0`) and live.

- `/api/albumxq` of a file album carries `xcandidates`: up to 8 releases and
  stream albums it may be a version of, by title — the same; one title inside
  the other (a one-word title only with a shared artist word); or most words
  shared plus a shared artist word (`App.candidatesFor`).
- A file album's page shows them under **Same album as…**: "Link" makes the
  files a version of that album (the existing `/api/albumlink`, `via:
  clutter`) and shows it; the name opens the album first.
- Sample on the test server: 42 of 150 unlinked file albums get candidates;
  pop/rock ones are mostly right (Zappa *One Size Fits All*, Hendrix *Live At
  Berkeley*, Black Keys *Brothers* ↔ its anniversary edition), classical ones
  with generic titles ("Piano Sonatas", "Symphonie Nr. …") are noisy.
- First link by hand, through the API on the live server: the *Mind Maps 6*
  files → CD D34897583 (the names differ too much for the import).
- **Not tested:** the block in a browser. No free search yet for an album the
  title rule does not find — open the release and use its id is not possible
  from the page either; to add if it is missed.

## Slice 3 — links between versions (built 2026-11-25)

Sources in the working tree, uncommitted; bundle installed (it works with an
older server). **Live after a restart**, which also brings slice 2.

- Log lines: `{"op":"link","key":"D…|T…","version":"J…","via":…}` and
  `{"op":"unlink","version":"J…"}`. A file album is a version of one album at
  most; the last line counts.
- **907 links imported** into `~/Sync/clutter/collection.jsonl` by
  `scripts/import-inventory-links.py` (`via: inventory:sure`): 550 to Discogs
  releases, 357 to Tidal albums. Left in
  `cache/jellyfin-inventory/link-candidates.tsv` for review: 186 not sure, 48
  where several clutter albums fit. The script is repeatable and never links a
  file album that was unlinked by hand.
- A linked file album (`Collection.attachLinks`): is in `albumFiles` of its
  album (new `Album` field; Location-named ids first), is its stand-in (so not
  listed itself), gives it `provider.jellyfin` and its `storage.*` tags, and
  its plays count for the album.
- Lists `Server`, `Server <name>` and `All` are now read off the album map when
  asked for (`Provider.serverList`, `allList`): a linked file album shows as
  its album, so "On the server › hirez" has records and Tidal albums in it.
- Album page, block **On the server**: each file version with its storage
  location (click → that list), a link into Jellyfin, and × for a wrong match
  (`/api/albumlink/:id?version=J…&op=unlink`). Files named in the Discogs
  Location field have no × — that link is changed in Discogs.
- Numbers on the test server after the import: Collection 2,018 (unchanged),
  Inbox 6,470, All 8,488, Server 4,443 (3,738 file albums, 400 releases, 304
  Tidal albums standing for the rest).
- Tested on 8083 with scratch logs: import twice (second run adds nothing),
  lists, tags on a linked release, three file versions on one album, unlink
  (logged: yes; Location-named: refused; twice: refused), link back, test
  suite. **Not tested:** the block in a browser, a play of a linked file album.
- Not in this slice: linking by hand from a file album's page ("this is the
  same as …"), the review of the 234 unsure candidates, one Versions block for
  record + streams + files, re-resolving ids after files move.

## Slice 2 — collection log and keep mark (built 2026-11-25)

Sources in the working tree, uncommitted; the new bundle and two style rules
are installed (they work with the slice-1 server too: the Collection block
only shows once the backend sends the tags). **Live after a restart.**

- `src/Collection.hs`: `~/Sync/clutter/collection.jsonl`
  (`CLUTTER_COLLECTION`), append-only, one line per change:
  `{"ts","op":"keep","key":"T…","value":"yes|no|unset","artist","title"}`.
  The last line for a key counts; other `op`s are skipped (links to come).
- The rule as agreed: in = marked yes, or not marked and (a record on a shelf
  or played). A record on a shelf = a Discogs release outside the stand-in
  folders (`CLUTTER_STANDIN_FOLDERS`, default `Streaming,Files`).
- Tags on every album, recomputed with the plays: `collection.in`,
  `collection.inbox`, `collection.out`; `keep.yes`, `keep.no`.
- Lists **Collection** and **Inbox** (sidebar, Library), read off the tags
  when asked for, so a play or a mark shows at once. **All is everything
  again** (minus stand-ins), file albums included; slice 1's interim rule
  (`outsideCollection`) is gone. The album page's strips show only the
  collection.
- `GET /api/albumkeep/:id?value=yes|no|unset`; album page: block
  "Collection" with the status and why, buttons Keep / Not for me (a click on
  the set mark takes it off).
- Numbers on the test server: Collection 2,018 (1,984 releases, 34 Tidal
  albums); Inbox 7,376 (4,645 file albums, 2,682 Tidal, 49 unplayed stand-in
  releases); All 9,394.
- Tested on 8083 with a scratch log: yes / no / unset on a file album, a Tidal
  album and a release; tags and list sizes follow; marks read back after a
  restart; bad value and unknown album refused; test suite passes; sidebar
  seen in a screenshot. **Not tested:** the buttons in a browser. A mark takes
  about 3 s (all tags are recomputed) — to speed up if it annoys.
- Not in this slice: links between versions and the inventory's matches, the
  Versions block, a bulk Keep on a list, Collection as the start list,
  retiring the Streaming folder.

## Slice 1 — file albums in clutter (built 2026-11-25)

- `scripts/update-jellyfin-albums.py` → `~/Sync/clutter/jellyfin-albums.json`
  (4,673 albums; the 272 outside every library are left out). It remembers the
  Jellyfin URLs (`api`, `web`) in the store; `--url` / `--web` or
  `CLUTTER_JELLYFIN` set them. Storage locations from
  `~/Sync/clutter/storage-map.json` (folder prefix → name, longest wins; else
  the library's name): music 2,229 · 1001 1,003 · classical 673 · hirez 309 ·
  compilations 229 · opera 171 · jazz 55 · diverse 4.
- `src/Jellyfin.hs` reads the store at start: every album is a `J<32 hex>`
  album (`AlbumKey` `J Text` now), format `Jellyfin`, tags `provider.jellyfin`
  and `storage.<name>`, cover and link from the store's `web` URL, length tags
  from Jellyfin's playing time.
- Lists `Server` (all) and `Server <storage>`; sidebar fold **On the server**.
- Membership, interim: a file album is left out of `All`, of a tag used as a
  list, and of the album page's strips **until it has a play**
  (`Provider.outsideCollection`). A file album a Discogs release names in
  Location is its stand-in, as with Tidal.
- Plays of a file album go to the log with `"src":"jellyfin","jellyfin":"<id>"`
  (and a release's `J` id rides along on its own plays).
- The Jellyfin link in the frontend comes from the server (`mjellyfin` in
  `/api/paramsq/all`); no host name in the code any more.
- Tested on a test server (8083): lists and counts, focus on a server list,
  a file album's data, facts, cover (HTTP 200), `All` unchanged in kind
  (4,749 albums, no unplayed file in it), test suite passes, sidebar seen in a
  screenshot. **Not tested:** a server list seen in the browser, the album
  page of a file album in the browser, pressing Album Played on one.
- Not in this slice: tracks for file albums ("no source for tracks"), the keep
  mark and the collection store, links from the inventory's matches, storage
  tags for records and streams, re-reading the store without a restart,
  re-resolving ids after a move, playing files.
- **To go live:** restart the server (`nix run` rebuilds; `src/Jellyfin.hs`
  is `git add`ed so the flake sees it), **then** install the bundle:
  `cp frontend/index.js.jellyfin frontend/index.js`. Not before — the new
  frontend needs `mjellyfin` from the new backend.

Goal (LATB 11-22): bring the music files on umini into clutter.
(a) albums on disk that are nowhere in clutter → Jellyfin albums with `J` keys;
(b) physical albums in Discogs whose rip is on disk but not linked → add the
`J<id>` to the Discogs Location field. First step: an inventory.

## What is there today (checked 2026-11-25)

- 30 of 2,037 Discogs releases carry a `J<id>` in Location (field 4). The id is
  Jellyfin's 32-hex item id (`J32aa55195f1bbdc563ad5f452fcedb0f`).
- clutter only *links* to Jellyfin: `albumJellyfin`, the `provider.jellyfin`
  tag, an "Open in Jellyfin" link. There is no Jellyfin reader and no store.
- `Types.AlbumKey` has `J Int`, and `parseKey` reads a number — a 32-hex id does
  not parse. A Jellyfin-only album cannot have a key yet. Matters for (a), not
  for the inventory.
- The link in `Render.purs` points at `http://usrv.local:8096` — to check
  whether that host name still reaches umini.
- `tok.json` has a Jellyfin API key (`jky`); `update-json` reads it and does
  nothing with it.
- Music datasets on umini (from `dotfiles/machines/umini.nix`): `z1/m/music`,
  `classical`, `opera`, `musicorigs`, `musicjohannes`, `musicam`, and
  `z0/m/hirez`. Which of them are Jellyfin libraries is not known here.

## The model (LATB 2026-11-25) — proposal below it not yet agreed

LATB's mental model, close to Roon's: a collection of **albums I care about**
(every record bought, plus a set of favoured ones) — not all of Tidal, not
every file ever ripped. Three layers:

1. **Album** — the recording and its metadata.
2. **Versions** — the media it comes on: a physical copy (Discogs release), a
   Tidal / Qobuz / Apple album, a file album (CD rip, hi-res download); several
   of each are possible.
3. **Access** — how to play one: the shelf it stands on, the stream, the file.

Today an album in clutter *is* its first version (a Discogs release or a Tidal
album) with the other ids hung on it in the Discogs Location field, and
`Provider.standIns` hides the twins. Files would add a third kind and many
more twins.

Proposed consequences (to discuss):

- **Being on disk does not make an album a member.** A file album comes in
  (i) as a version of an album already in the collection — a Discogs release
  *or* a Tidal favourite; (ii) as an album of its own only by a rule or a
  choice: bought downloads (the hi-res library?) by rule, anything else when
  favoured (Jellyfin's ♥ as the analogue of Tidal's favourites).
- **The inventory classifies** every Jellyfin album: version of an owned
  record / version of a Tidal favourite / bought download / unclaimed.
- **Links between versions move into clutter's own store** (groups of keys:
  `D… T… J… J…`). The Location field only works where a Discogs release
  exists; a Tidal + file album has nowhere to keep its link. Location stays an
  input. `standIns` becomes a reading of this store.
- **Album page gets a Versions block** (vinyl · cube 3 / CD · box 12 / file
  24-96 / Tidal), one row per album in the lists.
- Order: configuration → classified inventory → membership rule, with numbers
  → link store + Versions block (files as versions of known albums) →
  file-only albums (`J` keys) → playing files.

## Membership: what is in the collection (LATB 2026-11-25, reviewed)

Correction to the above: Tidal favourites are mostly "save for later" too, and
the files are whatever came along plus a small bought subset. Neither is a
sign of caring. LATB's rule:

- every physical record in Discogs is in, by default;
- no Tidal, Apple or Jellyfin album is in by default — LATB marks them;
- every album ever listened to is in.

Review — it holds, with three amendments proposed:

1. **Three states, not two**: the mark is *yes / no / not set*. In = marked
   yes, or not set and (physical or played). The explicit *no* is needed
   because listening is also how an inbox album gets judged — one trial play
   must not be irrevocable. (Today's "Remove from clutter" is that *no*.)
2. **The mark sits on the album, not on a version** — in the link store
   proposed above, which becomes the collection file
   (`~/Sync/clutter/collection.json`: key groups + mark + date). Tidal cannot
   hold it (favourites no longer mean it; no writes to Tidal).
3. **"Physical" needs the folder**: Discogs folders *Streaming* (219) and
   *Files* (51) are stand-ins, not records on a shelf. Until now they *were*
   the mark for streams. Nearly all are played (216 of the two folders'
   releases have plays), so they stay in by the listening rule.

Numbers (2026-11-25): 2,718 Tidal albums in the store stand alone; 33 of them
have a play (+11 played ones no longer in the store) → about 2,685 start in
the inbox. 1,504 Discogs releases have plays. Play log: 2,025 events on
Discogs keys, 50 Tidal, 1 Apple.

The Streaming folder: obsolete as a mechanism once the mark exists, but its
219 releases carry plays, list memberships and ratings on their `D` key.
Retiring it is a migration (re-key plays to the `T` album — events carry both
ids —, move list membership into clutter, then LATB removes the releases), not
a deletion. Same question for *Files* → `J` albums.

Word: proposed **keep / kept** for the mark, **Collection** for what is in,
**Inbox** for the rest (alternative: Roon's "Library").

UI: scope switch Collection · Inbox · All in the list toolbar, Collection the
default everywhere; a computed tag (as `played.*`, `length.*`) so the focus
machinery works on it; Keep / Not for me on the album page and as a bulk
action on a list; the page says why an album is in (on the shelf · played ·
kept). The Inbox is where files are browsed: by source (Tidal saved, files by
Jellyfin library).

## Agreed 2026-11-25 (LATB)

- **Three states** for the mark (keep / not for me / not set): yes.
- **Everything on the server stays browsable**, kept or not — rediscovery is a
  purpose of its own. So: all Jellyfin albums are loaded; a sidebar group
  ("On the server", by Jellyfin library, then artist) lists them regardless of
  the mark; playing one puts it in the collection by the listening rule.
- **clutter is the main database**, with links out to Discogs, Jellyfin,
  Tidal, Apple Music, later perhaps Qobuz.

### Storage (proposal)

Stay with plain files, in two kinds:

- **What only clutter knows** — plays, marks, links between versions, later
  list membership and ratings: **append-only event logs** (`.jsonl`, one line
  per change with a timestamp), as `listened/YYYY.jsonl` is today. The state
  is the log replayed at start. Nothing is ever rewritten, so a syncthing
  conflict is two files to concatenate, not a lost update, and every change is
  readable, greppable and reversible. One writer: the server.
- **What can be fetched again** — `tidal-albums.json`, the coming
  `jellyfin-albums.json`, `album-details/`, `wikidata.json`: snapshots, written
  whole (temp file + rename), rebuildable from the services.

Not SQLite for now: about 5–10 thousand albums and a few thousand events fit
in memory (the server holds them in IORefs already); a database file under
syncthing is the classic way to corrupt one; and the hand-editable, diffable
files are worth more than queries at this size. Revisit if clutter gets a
second writer or the stores pass ~100 MB. To keep that door open: all reads
and writes of the truth files go through one module.

Found 2026-11-25: Jellyfin 10.11.11 answers as `usrv.local`, `umini.local`
and `umini.taild2340b.ts.net` (port 8096). **`jky` in `tok.json` is empty** —
an API key has to be made (Jellyfin › Dashboard › API Keys) before anything
can be read. The sandbox blocks these hosts: each run needs LATB's OK, or he
runs the script himself.

## Inventory results (first run 2026-11-25)

`scripts/jellyfin-inventory.py --url http://usrv.local:8096`; output in
`cache/jellyfin-inventory/` (`summary.md`, `albums.tsv`,
`link-candidates.tsv`, `broken-j.tsv`). Disk cross-check not done yet.

Jellyfin music libraries: **Music** (`/media/music/Music`), **Music HR**
(`/media/music/HiRez`), **Opera** (`/media/music/Opera`). 4,945 albums.

| library | linked | physical | stand-in | Tidal | unclaimed | all |
|---|---:|---:|---:|---:|---:|---:|
| Music | 24 | 497 | 84 | 378 | 3,210 | 4,193 |
| Music HR | 1 | 79 | 15 | 48 | 166 | 309 |
| Opera | 3 | 0 | 0 | 1 | 167 | 171 |
| (no library) | 0 | 32 | 2 | 5 | 233 | 272 |
| all | 28 | 608 | 101 | 432 | 3,776 | 4,945 |

Grades — physical 510 sure / 33 likely / 65 ambiguous; stand-in 80 / 9 / 12;
Tidal 365 / 19 / 48. Sampled: *sure* looks right, *likely* mostly right
(Prince vs Prince And The Revolution), *ambiguous* mostly wrong (same title,
other artist) — to be read by hand or dropped.

Findings:

- The physical matches are mostly **vinyl** (521; CD 66, SACD 17): the file is
  the same album as a record on the shelf, not a rip of it. The "about a
  hundred CD rips" are the 83 CD/SACD ones.
- 436 of the 608 physical matches and 62 of the 101 stand-ins have plays;
  only 10 of the 432 Tidal matches do.
- **272 albums sit under `/media/z1/music/Music`, outside every library**;
  270 of them have the same relative folder as an album in *Music*, and most
  have no album artist — probably the same files seen through a second path,
  i.e. ghost entries in Jellyfin (not verified on umini), to be treated as
  duplicates and cleaned up in Jellyfin by LATB.
- 367 artist+title pairs occur more than once in Jellyfin (911 albums): other
  editions, MP3 next to FLAC, the ghosts. 231 clutter albums are matched by
  more than one Jellyfin album — the store must allow several `J` per album.
- 289 albums have no album artist; 2,186 no MusicBrainz id — matching rests on
  names.
- Broken `J` ids: D12825229 Metallica *…And Justice For All* (id gone), and
  D34763234 Ninajirachi, whose Location holds Jellyfin's *server* id, not an
  album id.
- Track counts came back 0 for every album (the query probably needs a user
  id) — to fix before track count is used as a tie-breaker.

## Storage locations (LATB 2026-11-25)

Roon's "Storage Location" (LATB's: Music, Classical, TIDAL Library, Music
Johannes, HiRez, Opera) as a way to browse → in clutter an extension of the
focus tags.

- A computed tag family, `storage.*`, next to `provider.*`: from the Jellyfin
  library, refined by a **configured map of path prefix → name** (portability:
  the map is configuration, not code). So `Music/_Classical`, `_1001`,
  `_Jazz`, `_Compilations` can be storage locations of their own **without
  moving a file**. For a physical record the storage location is its shelf —
  the existing Location; for a stream, `storage.tidal`.
- The Music library today (4,193 albums): artist folders 2,229 · `_1001`
  1,003 · `_Classical` 673 (of which `_Piano` 348) · `_Compilations` 229 ·
  `_Jazz` 55 · `_Diverse` 4. Of the 673 in `_Classical`, 647 are unclaimed.
- **Jellyfin has three music libraries only** (Music, Music HR, Opera). The
  datasets *Classical* and *Music Johannes*, which Roon has, are not in
  Jellyfin — clutter will not see them until they are added as libraries.
- Idea: `Music/_Classical` into a dataset of its own, named Classical; today's
  Classical (a few works in every interpretation) renamed. A file-server job,
  independent of clutter — but **a move changes the paths, and Jellyfin's
  album ids follow the path**: every `J` link to a moved album breaks (14
  linked albums are in `_Classical` today). Hence: **a link to a file album
  stores more than the id** — library-relative folder, artist, title,
  MusicBrainz id where there is one — and the Jellyfin update re-resolves a
  vanished id from those. With that the move can happen any time. To mind on
  umini: moving between datasets is a copy; z1 was 90 % full (10-23) and
  snapshots keep the old blocks; the new dataset needs its sanoid/syncoid
  entries and a Samba/Jellyfin/Roon path.

## Principles (LATB 2026-11-25)

- **Portability of the server**: nothing about this installation in the code.
  Jellyfin needs two settings — the URL the server uses for the API and the
  URL the browser opens (they can differ: LAN name vs tailnet name) — plus the
  key from `tok.json`. The frontend gets the browser URL through
  `/api/paramsq/all` (as it gets `muhq`), replacing the hard-coded
  `http://usrv.local:8096`. No Jellyfin configured → no Jellyfin links, no
  error.
- **Jellyfin's API is the base, not the file server.** The API gives the
  libraries and their folders (`/Library/VirtualFolders`), and every album
  with its id, tags, folder and MusicBrainz ids (`/Items`). It needs a URL and
  a key only — no ssh, no paths — so it works wherever the server runs. The
  `J` id is Jellyfin's anyway. Not the database file (`jellyfin.db`): its
  layout changes between Jellyfin versions.
- The disk scan is a **one-time cross-check** in the inventory (what did
  Jellyfin not pick up?), not something clutter depends on.

## Phase 1 — inventory (read-only)

Nothing is written to Discogs, Jellyfin or clutter's stores. Output goes to
`cache/jellyfin-inventory/` (snapshots, TSV lists) and a summary in
`docs/jellyfin-inventory.md`. One script, `scripts/jellyfin-inventory.py`,
re-runnable.

1. **Three snapshots**
   - *Jellyfin libraries*: `/Library/VirtualFolders` → each library's name,
     type and folders. This settles which datasets Jellyfin covers.
   - *Disk* (cross-check, second): one read-only `find` over ssh on umini —
     every folder that holds audio files: path, number of tracks, formats,
     size; the library folders, and the music datasets no library covers. No
     tag reading (Jellyfin has read the tags already).
   - *Jellyfin albums*: the API's album list (`/Items?IncludeItemTypes=MusicAlbum`,
     with Path, ProviderIds, ChildCount, RunTimeTicks): id, title, album
     artist, year, folder, track count, length, MusicBrainz ids.
   - *clutter*: the Discogs releases in `cache/draw*.json` (artists, title,
     year, format, Location) and the Tidal album store.
2. **Joins**
   - Disk ↔ Jellyfin by folder → folders Jellyfin does not know; Jellyfin
     albums whose folder is gone.
   - The 33 existing `J` ids → do they still resolve? (Jellyfin ids follow the
     path; hirez moved z1 → z0 on 11-08.)
   - Jellyfin ↔ Discogs by normalised artist + title, with year and track
     count as tie-breakers → candidates for (b), graded *sure / likely /
     ambiguous*. Where several Discogs releases match (vinyl and CD of the same
     album) the CD is proposed and the row is flagged.
   - Jellyfin ↔ Tidal store the same way → file albums that clutter already
     has as a stream (information for (a): new album, or a third id on a known
     one).
   - The rest → on disk, nowhere in clutter: the (a) list.
3. **Report**: a table of counts per dataset, and four lists for review —
   `link-candidates.tsv` (b), `new-albums.tsv` (a), `broken-j.tsv`,
   `not-in-jellyfin.tsv`.

Needs: one ssh to umini (asks LATB), the Jellyfin key from `tok.json`, network
access from this Mac to Jellyfin on umini.

## Phase 2 — link the rips (b)

From the reviewed `link-candidates.tsv`: append `J<id>` to the Location field
of each Discogs release (a write to Discogs, the same call clutter uses for
`T`/`A` ids). *Sure* rows in one batch after LATB's OK, the others one by one.
Repair the broken `J` ids the same way.

## Phase 3 — Jellyfin albums in clutter (a)

Design to be decided after the inventory, when the numbers are known:

- `J` keys as text (32 hex) instead of `Int`.
- A store `~/Sync/clutter/jellyfin-albums.json` filled by a script from the
  API (as `tidal-albums.json` is), and a `FromJellyfin` reader.
- Which albums come in: all of the (a) list, or only chosen libraries
  (`musicjohannes`, `musicam`, `musicorigs`?).
- Cover, tracks and length from Jellyfin; what the play button does.
- A file album that is also a Tidal album in clutter: one album with two ids,
  as with Discogs + Tidal (`Provider.standIns`).

## Open questions

- Which datasets count as LATB's library for this?
- Is Jellyfin or the disk the reference where they disagree?

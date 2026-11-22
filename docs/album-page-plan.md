# Album page in the style of Roon — plan

Status: **phase 0 built** (2026-11-22, uncommitted): `renderAlbumView` in `Render.purs` is one view
for Discogs and stream albums (hero, ▶ with the four modes, genre chips and tags that open the
"All" list with that focus — new action `ShowFocus` —, facts, Read more, Open in, play history
with a bar per year; the cover badges and the Obsidian note text are gone from this view), styles
`.av-*` at the end of `static/style.css`. Checked in headless Chrome on a test server (8082) on
*Getting Killed* (D35208358, wide and 430 px) and the Volodos album (T124439542); the chip, tag
and mode buttons were not clicked there.
**Phase 1 built** (2026-11-22, uncommitted): `GET /api/albumxq/:id` → `{xid, xfacts, xrelated}`.
`src/Details.hs` reads `cache/draw*.json` (label, catalog number, styles) and the Tidal album
store (length, tracks, quality, full date, EP/single, explicit) once at start — on its own, not
through `Release`, so an album added while the server runs has no facts until the restart.
`src/Related.hs` (pure) makes the strips from the albums in memory: other collection releases of
the same Discogs master, other albums sharing an artist (by store key or name, oldest first, at
most 24), and "similar": same genre, played before, most shared Discogs styles first, then
longest not played (12). A Tidal/Apple album that a Discogs release links to is left out as a
duplicate. The frontend asks for the details after the album (`loadDetail`), shows Length /
Quality / Label rows, the full date, style chips and the three strips; against a backend without
the endpoint it shows the phase 0 page. Checked on the same two albums on 8082. Not in phase 1:
an artist link to a clutter list. Phases 2–4 open. Mockup: `docs/album-page-mockup.html` (open in a
browser; switch design, example album and phase at the top). Source: the six Roon screenshots in
`docs/Screenshot 2026-11-22 at 10.*.png` (Geese, *Getting Killed*; Volodos, Schubert D. 959).

## Goal

Rebuild clutter's album view (`frontend/src/Render.purs`, `renderAlbumView`) from today's list of
`Title: / Artist: / Year:` lines into a page laid out like Roon's album page, built in the order
the data allows: first what the Album JSON already carries, then what is on disk, then what needs
a new lookup.

## What Roon's page has, and where clutter stands

Tier: 🟢 in the Album JSON today, frontend work only · 🟡 on disk (stores, `cache/`, server
memory), needs a server field · 🔵 needs a new lookup and a store · ⚪ not obtainable.

| Roon element | clutter source | tier |
|---|---|---|
| Large cover | `albumCover` | 🟢 |
| Release date | Tidal albums: full date in `albumReleased`. Discogs: year only; full date from the linked Tidal album in the store | 🟢 / 🟡 |
| Title (large serif), artist | `albumTitle`, `albumArtists` | 🟢 |
| Play now + ▾ menu | `▶ Node` exists; `/api/nodeplay` already takes `replace\|now\|next\|last`, the UI only sends `replace` | 🟢 |
| ♥ favourite | status: tag `tidal.collection`, `albumAdded`, `albumFavoriteUntil` (Tidal albums; for a Discogs release with a `T…` link, to check). Toggle = a write to Tidal | 🟢 status / 🔵 toggle |
| Listen later | nothing; could become a clutter tag | ⚪ for now |
| Genre chips | `genre.*` tags. Discogs styles are in `albumTags` too, but mixed with format, folder and rating tags, so they need their own field | 🟢 / 🟡 |
| Chip → list of that genre | focus `#genre.x` (`ToggleFocus`) | 🟢 |
| Review text | Roon's is licensed from TiVo: no. Substitutes: Wikipedia lead paragraphs (album page for 580 Discogs + 174 Tidal-only albums, artist page for most of the rest), Tidal's album review | ⚪ → 🔵 |
| Length | Tidal store `duration`, `numberOfTracks` (also for a Discogs release with a `T…` link) | 🟡 |
| Rating | `albumRating` | 🟢 |
| Format | `albumFormat`; Tidal quality from `mediaMetadata.tags` (930 of 2,806 Hi-Res, 54 Atmos). Sample rate and bit depth are not in the store | 🟢 / 🟡 |
| Dynamic range, Images | Roon's own | ⚪ |
| Added | `albumAdded` | 🟢 |
| Label, catalog number | Discogs `labels` in `cache/draw*.json` (all 2,037); Tidal `copyright` | 🟡 |
| Read more | `albumWiki`, `albumAllMusic`, per-artist links, search fallbacks (Wikidata phase 4) | 🟢 |
| Tracks tab | Tidal `albums/{id}/tracks`; Discogs `releases/{id}` tracklist | 🔵 |
| Work / movement grouping (classical) | heuristic on Tidal track titles (`Work: Movement`); Roon-grade composition data: no | 🔵 / ⚪ |
| Per-track plays, hearts, picks | clutter logs plays per album | ⚪ |
| Credits tab | Tidal credits; Discogs `extraartists` (same release call as the tracks) | 🔵 |
| Versions tab | owned versions: the release plus its linked Tidal / Apple / Jellyfin ids, and other collection releases with the same `master_id`. All pressings: Discogs `masters/{id}/versions` | 🟢 / 🟡 / 🔵 |
| Artist strip, "My library" | group the albums in memory by artist key | 🟡 |
| Artist strip, "Selected discography" | Tidal `artists/{id}/albums`, badge where the album is in the library | 🔵 |
| Recommended albums | Roon's recommender. Substitute: same genre from the library, least recently played first | ⚪ → 🟡 |
| Similar albums, similar artists | Tidal `albums/{id}/similar`, `artists/{id}/similar`; artist pictures are already in the store | 🔵 |
| Sticky title bar while scrolling | CSS | 🟢 |

What clutter has and Roon does not, to keep prominent: the full play history with times, the
shelf/box location, Discogs/Tidal/Apple/Jellyfin links, the *Album Played* button, the Obsidian
note text (collapsed by default).

Numbers behind two rows: 730 of 2,182 artists have more than one album in the library, and they
cover 3,391 of 4,843 album entries, so the "in my library" strip shows on about 70 % of pages.
1,876 of 2,037 Discogs releases have a master (versions).

⚠️ Not verified: the Tidal endpoints for tracks, review, credits and similar. The probe on
2026-11-22 reached `api.tidal.com` but the token had expired (401). Step one of phase 2 is to run
`./tidalauth` and probe them on two albums; the phase is sized after that.

## Designs (mockup)

- **A · Roon-style.** Cover left, date / serif title / artist / action row right; genre chips
  centered; text left and facts + links right; tabs Tracks · Credits · Versions; then play history
  and the album strips. Closest to the screenshots. With "now only" it is already a complete page.
- **B · Journal-first.** Three columns: cover and actions (sticky) · title, facts line, text,
  tracks · a right rail "Your plays" (count, plays per year, dates, location, more by the artist).
  Puts clutter's own data where Roon puts metadata. The rail is all 🟢/🟡.
- **C · Phone.** One column, sticky action bar, sections as accordions. This is how A or B should
  collapse on a narrow screen rather than a separate build.

Recommendation: **A, with B's "plays per year" bar in the play-history section, and C as the
narrow-screen layout of the same markup.**

## Phases

| phase | content | touches | done when |
|---|---|---|---|
| 0 | New layout from today's JSON: hero, chips, facts table, links, actions (▶ with the four modes), play history, collapsed note text; one view for Discogs and stream albums; narrow-screen layout | `Render.purs`, `style.css` | both views replaced; checked on a Discogs, a Tidal-only and a classical album |
| 1 | On-disk data: length, track count, quality, label / cat. no., full release date, styles, type (EP / single), owned versions; strips "in my library" and "more of this genre" | `Types.hs`, `FromDiscogs.hs`, `FromTidal.hs`, new detail endpoint, frontend | `/api/albumxq/:id` carries them; strips link to the albums |
| 2 | Probe Tidal; then `scripts/update-album-details.py` + store: tracks, credits, (review). Tracks and Credits tabs | script, `update-json`, new `src/Details.hs`, frontend | tabs filled for a Tidal and a Discogs album |
| 3 | About text: Wikipedia lead paragraphs for the album, else the artist; attribution line | `update-wikidata.py` or a sibling, store, frontend | text on *Getting Killed* (album page) and the Volodos album (artist page) |
| 4 | Discography, similar albums, similar artists from Tidal, with "in library" badges | script + store or on demand, frontend | strips filled |
| later | favourite toggle (Tidal write), play one track on the Node, "listen later" tag, all pressings from the Discogs master | | |

Phase 0 needs only a frontend bundle (`cd frontend && nix develop --command bash -c 'npm run
bundle'`) — no server rebuild or restart. From phase 1 on: `nix build` and a restart.

### Design points

- **Detail endpoint, not a fatter `Album`.** `Album` is sent thousands of times in list views.
  Phase 1 onward goes into `GET /api/albumxq/:id` (`AlbumDetail`: facts, versions, related ids,
  later tracks / credits / text), fetched when an album is opened. The Wikidata fields stay.
- **Lookups stay in scripts with a store in `~/Sync/clutter/`**, like `tidal-albums.json` and
  `wikidata.json`: the server's Tidal token dies 4 h after start, and a store survives a cache
  wipe. Cost of a first fill: Tidal 2,806 albums × 1–2 calls; Discogs 2,037 releases at 60 a
  minute ≈ 35 min, once.
- **Title font:** a system serif stack (Iowan Old Style, Palatino, Georgia) — no web font.
- **Wikipedia text** is CC BY-SA: show "Wikipedia" and the article link under it.

## Decisions (LATB, 2026-11-22)

1. **Design A.** Once the track list exists it **moves up** (above play history and the strips,
   directly under the hero/facts block).
2. **The Obsidian note text goes** from the page (the note is still written by *Album Played*).
3. About text: **Wikipedia leads**; a Tidal review later as a link or an alternative.
4. **Look up on demand, cache what was touched, with a "refresh" button** — no whole-library
   fill in `update-json`. Album pages open seldom, so tracks / credits / text / similar are
   fetched when a page is first opened and kept in a cache store (`~/Sync/clutter/album-details/`,
   one file per album key); *refresh* refetches that album. This supersedes the "lookups stay in
   scripts" design point above for phases 2–4. Prerequisite: the server must get a fresh Tidal
   token by itself (today it reads `tok.json` once at start and the token dies after 4 h) — do
   the refresh from `tidal-session.json` in the server, or have it call `./tidalauth` and re-read
   `tok.json` on a 401. Wikipedia and Discogs lookups have no such limit.
5. **No ♥ toggle.** Separate thread instead: a way to bring newly liked Tidal albums into
   clutter without `update-json` and a restart (shares the token prerequisite of decision 4).

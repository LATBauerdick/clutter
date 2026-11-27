# clutter backlog

Ideas to follow up on, one or two lines each, with the date they came up.
Nothing here is scheduled. When an idea is picked up it gets its own
`docs/<topic>-plan.md` and the entry here shrinks to a pointer; when it is
done the entry goes.

What is actually next lives in the project hub (`~/Notes/Claude/ws/clutter.md`
› Next).

## Album page

- 2026-11-23 · **About text, leftovers of phase 3** (built 11-23): the text is
  about the first linked performer in Discogs' order, so an orchestra comes
  before its conductor and a narrator can come first (*Ivan the Terrible* →
  Christopher Plummer) — a better pick, or a way to switch artist?; albums
  whose track fetch fails (gone from Tidal, Apple-only) get no text. See
  `album-page-plan.md`.
- 2026-11-23 · **Album summary written by a model** (LATB: "ask Siri AI"),
  for albums with neither a Tidal review nor a Wikipedia page of their own —
  or as a third text next to them. Candidate routes, none tried: a Shortcut
  with the "Use Model" action (on-device, Private Cloud Compute or ChatGPT)
  run by the server with `shortcuts run`; Apple's Foundation Models framework
  from a small Swift helper; the local model on fstu; the Claude API. To
  settle first: a model asked cold about an obscure album makes things up, so
  it should be given the facts clutter has (tracks, credits, label, year,
  the artist's Wikipedia lead) and summarize those; the text must be marked
  as generated and cached like the review; `shortcuts run` needs the server
  to have Automation rights (it runs in a Rex block).
- 2026-11-22 · **Phase 4: Tidal discography and similar artists.** Both
  endpoints (`artists/{id}/albums`, `artists/{id}/similar`) answered in the
  probe. See `album-page-plan.md`.
- 2026-11-22 · **"Open the player's app" button.** A button on the album page
  that opens the app belonging to the player in use: for the Node, the BluOS
  app, if possible on its now-playing screen. Open: whether BluOS has a URL
  scheme or deep link (Mac and iOS), and what the equivalent is for other
  players. Belongs with the configurable player work under Playback.


## List view and Album view
- indicate multi-disk media like Roon does in this snapshot Screenshot 2026-11-22 at 5.43.40 PM.png


## Lists and search

- 2026-11-23 · **Lengths still unknown: 473 Discogs releases** (407 vinyl)
  whose Discogs entry has no track durations and that name no Tidal album.
  Ways on: link them to a Tidal album (the parked lookup UI), another
  pressing of the same master that has durations, or type the length in by
  hand. `scripts/update-lengths.py --recheck` asks Discogs again.

- 2026-11-22 · **Saved searches / focus presets.** Today only "★ Never-played vinyl" is built in. A way to define and name such presets (NPV and others).
- 2026-11-22 · **"Gone from Tidal" list** as a pre-defined search over the  `tidal.gone` tag (201 of 2,807 albums on 2026-11-22).

## Tidal

- 2026-11-22 · **Find another edition** for an album gone from Tidal: most are
  still there as a different release. Search Tidal by artist and title, offer
  the candidates, re-link.
- 2026-11-22 · **Playlists.** Develop ideas for Tidal playlists in clutter;
  LATB has a Roon screenshot for `docs/`.
- 2026-11-22 · **Play with the Tidal app.** Make the Tidal app a player next
  to the Node, with minimal play control (play this album, pause/resume,
  stop). Known: `tidal://album/<id>` opens an album in the app. Open: how to
  start playback and control it from the server (the Mac app has no scripting
  dictionary that I know of; media keys or Tidal Connect are candidates), and
  which machine's app is meant when the browser is on an iPad or phone.
- Open question · After import, leave albums on Tidal's "Save for later" list
  (today) or let clutter take them off (a write to Tidal)?

## Apple Music

- 2026-11-26 · **Revive the Apple Music library** (LATB: "at some point"; he
  subscribes to Apple Music and may yet move from Tidal to it). The "Apple Music"
  list is empty because library access in `update-json` and in the provider
  (`FromAMusic.hs`, `readAMusicAlbums`) was built on a paid developer credential,
  which has expired; a full developer membership is not actually needed for
  library access. Today clutter holds no Apple album: the 819 Apple versions in
  the log are links only (from the old Discogs Location field), every one with a
  record, a Tidal album or files beside it. Low priority while most of the
  library is also on Tidal.
- 2026-11-26 · **Apple catalogue store** like the Tidal one, independent of the
  library: LATB links an Apple album for most music he plays, just as most Tidal
  albums came in (favoured to find the id, then un-favoured) — so a linked Apple
  album should be a held version as a Tidal album is. Public lookup (no
  credential): title, cover, year, tracks; 819 linked today. Versions lists held
  versions only (since 2026-11-26), so they show again once held. Playing Apple
  Music from clutter (the Node) would be separate. LATB: build it after his list
  of UI issues.

## Playback

- 2026-11-22 · **More playing controls for the Node**, configurable so that a
  hardware change is a configuration change: some kind of player plugin with
  a small common interface (play album, pause, stop, next, volume, now
  playing, "open its app"). TBD. The Tidal app player and the open-app button
  above are two more cases for the same interface.
- 2026-11-26 · **Cover art for the Node must stay below 600 KB** (LATB, after
  trying slice 17): the Node does not show larger artwork of files it plays.
  At some point: scan all artwork on umini (embedded and `folder.jpg` /
  `cover.*` in the album folders), list what is over 600 KB, and compress it
  (resize to e.g. 1000–1400 px, JPEG quality ~85) — on copies first, as the
  files are the archive; then re-index the Node. Not only folder images:
  LATB saw it on Argerich II (Great Pianists of the 20th Century), whose art
  is embedded in the FLAC files — so embedded pictures need the same check
  (`metaflac --export-picture-to`, re-embed a smaller one).

## Jellyfin

- 2026-11-22 · ~~**Bring the files on umini into clutter.**~~ Done 2026-11-25,
  and more: file albums, storage locations, the collection with its keep mark,
  links between versions in clutter's own log, the Discogs Location ids
  brought over. What was built, slice by slice, with its test status:
  `docs/jellyfin-plan.md`.

## Collection and versions — next steps (2026-11-25)

In the order proposed to LATB; the first is the recommendation.

- ~~**Review page for the unsure matches.**~~ Built 2026-11-25 (sidebar, On
  the server › Unsure matches; `docs/jellyfin-plan.md` › Slice 10): 193 to
  review. To check in a browser after the restart. Ideas if it is missed: a
  free search per block for an album the title rule does not offer; "None of
  these" for all title-only rows at once.
- ~~**Bulk Keep in a list.**~~ Built 2026-11-25 (`docs/jellyfin-plan.md` ›
  Slice 11): "Select" in the list toolbar, then Keep / Not for me / Take mark
  off for the selection; "All n shown" selects what focus and search leave.
  To check in a browser. Ideas: shift-click for a range; the keep state as a
  badge on the covers.
- ~~**Tracks for file albums**~~ Built 2026-11-26 (`docs/jellyfin-plan.md` ›
  Slice 17). Later: fill the tracks cache ahead of time (~50 ms a track).
- **Classical and Music Johannes** as Jellyfin libraries (LATB adds them),
  then `scripts/update-jellyfin-albums.py`, an entry each in
  `~/Sync/clutter/storage-map.json` if wanted, and the inventory again. Also a
  library scan in Jellyfin for the 272 ghost albums under
  `/media/z1/music/Music`.
- **The album layer.** One row per album in the lists, its records, streams
  and files underneath as versions; several editions of a stream kept side by
  side; plays and locations per version. The end state of LATB's model and
  the largest step — after some days of use. Today's stop-gaps: releases of
  one Discogs master share their files; one stream of a kind per release.
  **Next big issue (LATB 2026-11-26; maybe a session with Fable):** clutter
  owns the album, every source is a version of it.
  - Today the album is whichever source leads: a Discogs release (holds its
    streams and files), a Tidal album (holds files), or a file album (holds
    a stream). Links point from a version to that lead, so a file album
    cannot lead, and a Tidal album clutter does not hold cannot gather
    copies — e.g. Doggystyle: one file copy (1001) holds Tidal 280733973,
    another (music) is unlinked, and they cannot be joined.
  - Proposal: an album key of clutter's own in the collection log (e.g.
    `{"op":"album","id":"C17","artist","title"}` and
    `{"op":"version","album":"C17","key":"D…|T…|A…|Q…|J…"}`); Discogs
    records are versions like the rest (the physical copies, with their
    shelf). The album carries plays, the keep mark, its own tags and the
    choice of cover/lead; each version keeps its format, quality, tracks.
    A version in no group is its own album implicitly — no lines for the
    thousands of single albums.
  - Migration: today's groups become albums mechanically (release + its
    streams + files; Tidal album + files; file album + stream); plays keyed
    by D/T/J resolve to the album the key is a version of.
  - Consequences: one row per album in the lists; the Versions block reads
    the album; "Same album as…" merges two albums (and splits); Discogs
    master ids and the inventory's matches become suggestions for merging.
  - Open questions: id scheme; whether Wikidata/Wikipedia links sit on the
    album or a version; how "Remove from clutter" and Tidal's Save for later
    map onto albums; the old Location-field ids (already imported).
- Smaller:
  - Playing files: which player, through what (BluOS, Roon, Jellyfin).
  - Retire the Discogs *Streaming* folder (219 stand-in releases): plays and
    list memberships move to the Tidal album first — a migration.
  - Show a stream's title (Tidal / Apple lookup) before linking; today the id
    is taken as pasted.
  - Free search in "Same album as…" for what the title rule does not find.
  - A "Kept" list in the sidebar (today: focus `keep.yes` in Collection).
  - Collection as the start list.
  - Storage tags for records (the shelf) and streams (`storage.tidal`).
  - ~~Re-read the Jellyfin store without a restart; find a file album again
    after its files moved~~ — done (slices 14, 16). Left: a moved folder
    whose title changed too (untagged Classical albums renamed) — a by-hand
    "this is the moved …" on a not-found album.
  - A keep mark takes ~3 s (all tags are recomputed) — speed up if it annoys.
  - First real run of the scripts that now read the log
    (`update-lengths.py`, `update-tidal-albums.py`, the inventory, the
    inventory-links import): watch the next `./update-json`.
  - The disk cross-check of the inventory (what did Jellyfin not pick up?)
    was never run.

## Clean-up

- 2026-11-22 · **General clean-up / refactoring session.** Retire the old
  server-rendered pages (`/albums`, `/album`, the Lucid `Render*.hs`) with
  their `#navbar` CSS; remove other dead code.

## To verify (built 2026-11-22, not yet exercised)

- A brand-new album saved for later on Tidal → ⋯ › Update from Tidal.
- An uncached Tidal album opened more than 4 h after the last token refresh
  (automatic refresh on 401).
- Albums with more than 100 tracks; `update-tidal-albums.py --check-gone`.

Done:
- The new frame on a real iPad mini and iPhone. -- tested both, works fine
- Remove an album, re-save it on Tidal, update → it should come back -- works, removed albume re-appears at old “position”
- ⁣

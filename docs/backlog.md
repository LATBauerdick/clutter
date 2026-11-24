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

## Playback

- 2026-11-22 · **More playing controls for the Node**, configurable so that a
  hardware change is a configuration change: some kind of player plugin with
  a small common interface (play album, pause, stop, next, volume, now
  playing, "open its app"). TBD. The Tidal app player and the open-app button
  above are two more cases for the same interface.

## Jellyfin

- 2026-11-22 · **Bring the files on umini into clutter.** A longer project of
  its own. First step: an inventory of disk vs Jellyfin vs clutter. Then
  (a) hundreds of albums on disk that are nowhere in clutter → Jellyfin
  albums (`J` keys); (b) about a hundred physical albums that are in Discogs
  and have a FLAC rip on disk that is not linked → add the `J<id>` to the
  Discogs Location field.

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

# The album layer — plan

Status 2026-11-26: **model and spec agreed with LATB (OK to all five questions);
slices 0–6 and 8 built, committed and live; the log was migrated on 2026-11-26.**
Slice 6's last step (removing the stand-in code) waits until LATB deletes the
Discogs folders Streaming and Files; slice 7 needs its own plan. Status per slice
at the end of Slices. Dry-run report of the migration:
`~/Notes/Claude/2026-11-26-clutter-album-layer-dry-run.md`. Follows `backlog.md` ›
The album layer and `jellyfin-plan.md` › the model (album → versions → access).

## Principle

clutter is the database. Discogs keeps two jobs and loses the third:

- **Keeps:** the catalogue — what a release is (title, label, format,
  tracklist, cover), including later corrections to a release, which clutter
  takes in (the check after an album page opens) — and the fact that LATB owns
  a record of it.
- **Loses:** being LATB's database. Everything he entered there (shelf and
  order, folders, lists, custom fields, ratings) is imported once and then
  lives in clutter's own files.

The Discogs folders *Streaming* and *Files* (stand-in releases) are no longer
needed and will be deleted — after anything recorded against a stand-in
release points at the real album inside clutter's log, since Discogs cannot
say what that id was once it is gone.

## Terms

| Term | Meaning |
|---|---|
| **Album** | The piece of music as clutter knows it, independent of how LATB has it. |
| **Version** | One way LATB has an album. Three kinds: record, stream, file album. |
| **Record** | A physical item he owns: vinyl, CD, SACD, box. Has a shelf and a position. |
| **Release** | The Discogs catalogue entry a record is a copy of (label, year, pressing). Strictly the Discogs sense: one owns records, not releases. |
| **Stream** | An album on Tidal, Apple Music or Qobuz. Where a service has several (remaster, deluxe), each is an **edition**. |
| **File album** | A folder of files on the server. |
| **Shelf** | Any place records live: Shelf, Cube, Box, Basement, Incoming, Lost&Found. **Position** is the place within it. |
| **Collection** | The albums LATB counts as his: with a record, marked keep, or played. clutter's word; Discogs's "collection" is just "his records". |
| **Inbox** | Albums present but not yet judged. |
| **Favourite** | A stream saved in Tidal or Apple Music — a fact about that stream in that service, not about the album. |
| **Library** | A Jellyfin library (Classical, Music, HiRez, …). **Storage location** stays the word for where the files are. |
| **Source** | Discogs, Tidal, Apple Music, Jellyfin: where clutter learns what a version is. |
| **Tag (curated)** | A label LATB puts on an album by hand and can take off again. |
| **View** | A named focus (a saved search over tags), shown as a list in the sidebar. |

## The model

| | Carries |
|---|---|
| **Album** | its versions; the lead version (gives title, artist, cover); curated tags; rating; keep mark; plays in total |
| **Record** | the release it is a copy of; shelf and position (for a box: the number on its label); media and sleeve condition; notes; acquisition number; date added |
| **Stream** | service; edition; whether it is a favourite there |
| **File album** | library; storage location; quality |
| **Play** | date and the version played |
| **Shelf** | its records, in order |
| **View** | a name and a focus |

- An album with one version is a full album like any other.
- A version belongs to one album. Several releases of one album (original
  pressing and reissue, vinyl and CD) are several record versions; several
  editions of a stream are several stream versions (today's limit of one
  stream of a kind per release goes).
- **Plays:** the log says which version was played; the count is the album's,
  across all versions.
- **Rating** is about the music, so it is the album's and covers every
  version. clutter's own; seeded once from Discogs, never written back.
- **Curated tags replace genre folders and the special lists.** There is no
  separate genre field: "Piano", "Symphonic", "Petra" are all tags of LATB's,
  and an album may have several. They get a prefix of their own, apart from
  the computed ones (`format.`, `played.`, `storage.`, `folder.`, `genre.` from
  Discogs …), and are the only tags that are edited by hand. Aim: a small
  hand-curated set that works across records, files and streams ("Piano" for
  the classical piano music wherever it is), to focus on when looking for
  something to play.
- **Views replace lists** other than shelves: Genre and list views become
  pre-defined focuses; LATB wants to define a few himself later.
- **Shelves are the one real list:** ordered, and their rows are records, not
  albums. A row in a shelf view is the record at that position; Discogs says
  what that record is. Records move between shelves and are reordered within
  one — in clutter, not in Discogs (that was a kludge). Rare, but needed.
- **Two records of the same release** are not modelled: a version has one
  record; the second copy goes to the Basement and is a note on the record.
- **Files ripped from a CD** are a version of the same album as that record
  where LATB still owns it; no provenance beyond that.

## What LATB entered in Discogs, and where it lands

From the cache of 2026-11-22: 2,037 collection entries, 2,033 releases; 1,767
records outside Streaming (219) and Files (51).

| In Discogs | Count | Lands as |
|---|---|---|
| Folders Pop 509, Piano 237, Symphonic 229, Opera&Vocal 169, Chamber 100, Concertos 69 | 1,313 | curated tag on the album; `folder.xxx` stays as it is for now |
| Folders CD 172, CDR 280 | 452 | nothing (the format is known from the release); these albums start without a curated tag |
| Folders Streaming, Files | 270 | retired, see Principle |
| Shelf lists (Cube, Shelf, Box, Basement, Incoming, Lost&Found) | 31 lists, 1,692 records | shelves; read from Discogs until clutter can edit them, then imported once |
| *Box CDs* list comments (539, 540, …) | 290 | the record's position in the box (the label number) |
| Lists *Petra* (35), *Darko/Lavorgna* (13) | 48 | curated tags |
| Lists *YYYY Listened* (7) | — | done: the play log |
| Field 1 media condition, field 2 sleeve condition | 1,056 each | record |
| Field 3 notes ("Johannes", "Washed", "SQ VG+", …) | 72 | record |
| Field 4 location (stream ids) | 1,177 | done: links in `collection.jsonl` |
| Field 5 (`2019060802`: date + sequence of when the record was bought or received) | 476 | record: acquisition number |
| Field 7 plays (counter) | 1,638 | superseded by the play log |
| Rating (5★ 159, 4★ 234, 3★ 1, 2★ 16) | 410 | album rating |
| Date added | all | record |

Found on the way, for a clean-up list at import:

- 4 releases owned twice: Chopin Études (4157426) and Ravel Complete Piano
  Music (5126610), both copies in Piano; Ax Handel Variations (10798368), CD
  and CDR; Levit People United (21619750), one of them a stand-in.
- 6 records are in two shelf lists at once; 79 records are on no shelf (70 in
  the CD folder).
- 89 Discogs masters with two or more of LATB's releases (179 records): the
  albums that get several record versions.

## Where the links stand today

`collection.jsonl` holds 2,840 links forming 1,307 groups of two or more keys;
all other albums have one version. Most groups: record + Tidal + Apple (443),
file album + Tidal (281), record + Tidal + Apple + files (293). About 45
groups hold two or more releases, and 76 streams are held by more than one
release (the vinyl and the CD of an album) — separate rows today, one album
in this model. Play lines carry `src` and the Discogs, Tidal and Apple ids,
so they resolve to a version without being rewritten.

## Left for later (agreed as not in the first version)

- Editing shelves in clutter: move a record to another shelf, reorder, add
  shelf space. Until then the shelf lists keep coming from Discogs.
- An editor for views; the first version ships a few fixed ones. Losing the
  special Discogs lists for a while is acceptable.
- A rating control in clutter (and then clearing the ratings at Discogs,
  where LATB's scale skews theirs).
- Assigning curated tags to the albums that have none.

## Spec for the open points — proposed 2026-11-26, agreed by LATB the same day

Proposed by Claude; LATB answered OK to all five questions at the end
(2026-11-26). It is written to be built from, slice by slice.

### Ids

- An album with one version has no id: it is named by that version's key
  (`D123`, `T456`, `J…`).
- An album gets an id of its own when its second version joins: `C` + 6
  characters from `0-9a-hjkmnp-tv-z` (lower case, no i l o u), random, checked
  against the ids already in the log. `Types.AlbumKey` gains `C Text`;
  `parseKey` takes `C` + exactly those 6 characters.
- The id is for the log and the server only. URLs, the lists and the API keep
  version keys: an album's row is its lead version's key, and
  `/album/<any version key>` opens the album with that version chosen.
- Any log line that speaks about an album may name it by its `C` id or by any
  of its version keys; the reader resolves the name through the membership
  **as the whole log leaves it**, not as it was when the line was written. So
  what was recorded against a version key follows that version through a
  split; what was recorded against a `C` id stays with that album.

### Log lines (`collection.jsonl`, appended, never rewritten)

All carry `ts`; `artist` and `title` are added for a human reader and never
read back; `via` says who wrote the line (`clutter`, `migration`,
`discogs-master`, `discogs-import`).

| Line | Meaning |
|---|---|
| `{"op":"format","format":2}` | From here the log is in the album format: `link` and `unlink` lines, wherever they are, are no longer read. Written once by the migration. (Was `"version":2` until slice 0 found that today's readers take any `version` field for a key — `collection_log.read` crashed on it.) |
| `{"op":"version","album":"C…","key":"D123"}` | `key` is a version of that album. The last `version` or `split` line for a key counts. |
| `{"op":"split","key":"D123"}` | `key` is its own album again. |
| `{"op":"merge","from":"C…","into":"C…"}` | Album `from` is now `into`: its versions, and everything recorded against its id. Chains followed (merge b into c, then a into b: a ends in c); a loop stops where it closes; a later merge from the same id replaces the earlier; only the exact reverse line (b into a after a into b) takes the earlier one off. Unlike `moved` lines, a merge into an id does not undo a merge from it. |
| `{"op":"lead","album":…,"key":"D123"}` | That version leads; without `key`: back to the rule. Last line for the album counts. |
| `{"op":"alias","from":"D9340172","to":"T184781700"}` | `from` is a key clutter no longer holds (a stand-in release); everything recorded against it — plays, marks — is read as `to`. Chains followed as for `merge`: only the exact reverse line takes an earlier alias off. |
| `{"op":"keep","key":…,"value":…}` | As today; `key` may be a `C` id. Last line for the album counts. |
| `{"op":"tag","album":…,"tag":"piano","value":true}` | Put a curated tag on the album, or take it off (`false`). Folded in order. |
| `{"op":"rate","album":…,"stars":4}` | The album's rating, 1–5; 0 takes it off. Last line counts. |
| `{"op":"ref","album":…,"discogs":"release/123"}` | A Discogs release or master (`"master/456"`) that describes the album without being owned; `"value":false` takes it off. Folded in order, after the references the aliases from a `D` key give (slice 8, see Discogs references). |
| `moved`, `skip`, `notsame` | Unchanged. |

Where a version key is meant — `key` of `version`, `split` and `lead`, both ends of
`alias` — a `C` id is not read (it would make an album a version of an album).

How the operations are written:

- Join a one-version album to an album with an id: one `version` line.
- Join two one-version albums: mint an id, two `version` lines.
- Join two albums that both have an id: one `merge` line (the one opened
  first is `into`).
- Take a version out: `split`. An album left with one version keeps its id.
- After a merge, single-valued facts (keep, rating, lead) are the last line
  in the log for either album; curated tags are those of both.

New `keep`, `tag`, `rate` and `lead` lines are written against the `C` id if
the album has one, else against its version key.

### Reading plays

The play log is not rewritten and new lines keep today's format. A play's
version is the key its `src` names (`discogs` → `D`, `tidal` → `T`, `apple` →
`A`, `jellyfin` → `J`), read through `moved` and `alias`. If clutter does not
hold that key, the other ids on the line are tried in the order discogs,
tidal, apple, jellyfin. The play counts for the album that version belongs
to; plays per version are a count over the same lines.

### Lead version

The lead gives the album its title, artist, cover, year, date added, and the
key of its row. Rule, unless a `lead` line says otherwise:

1. a record (if several: the one added to the collection first),
2. else a Tidal album (if several: the one that became a version first),
3. else a file album (likewise),
4. else an Apple Music, then a Qobuz album.

### Tags

- Album-level tags — `played.*`, `collection.*`, `keep.*`, `rated.*`
  (from clutter's rating once imported; from Discogs until then), `length.*`
  (the lead's), and the curated ones, prefix `my.` (`my.piano`, `my.petra`) —
  are on every version of the album.
- An album's **row** (every list but a shelf) matches a focus on the union of
  its versions' own tags: an album with a vinyl and a CD is found under both
  formats, and under `provider.tidal` and `storage.*` if it has those.
- A **record's row in a shelf list** matches on that record's own tags
  (format, folder, …), the album-level tags, and the album's `provider.*` and
  `storage.*` tags (what else it is available as) — not on another record's
  format.
- Collection rule, on the album: in, if marked keep; or not marked and it has
  a record or has been played; out, if marked no; inbox otherwise.

This is the delicate part of the server change: today there is one tag set
per key. Build it so that both statements above hold and are tested; how
(a second map for album rows, or two tag sets) is the builder's choice.

### Migration

Script `scripts/album-layer-migrate.py`; `--dry-run` is the default and
writes nothing; `--write` first copies the log to
`collection.jsonl.before-album-layer`, then appends.

1. Read the log as today's server does (last line wins, moves followed):
   file links and stream links.
2. Albums = the connected groups over those links.
3. **Stand-in releases** (Discogs folders Streaming and Files): not versions.
   Each gets an `alias` to the lead of the rest of its group (the stream, or
   the file album). A stand-in whose group has nothing else stays as it is
   and is listed in the report.
4. **Same master:** releases of one Discogs master that are not already in
   one group are joined (`via: discogs-master`), each such join listed in the
   report. (89 masters have two or more of LATB's releases; how many are not
   joined yet is for the dry run to count.)
5. Each group of two or more keys gets an id and its `version` lines, then
   one `format` line at the end.
6. Not migrated: the Apple Music album a Tidal album names through the store
   lookup stays what it is today — an "also on Apple Music" fact of that
   stream, a version only once a line in the log says so.

The dry-run report: albums by shape (which kinds of version, how many);
then, each with artist and title: groups with releases of different masters,
groups of more than six keys, groups with two or more streams of one
service, the master joins of step 4, stand-ins left alone, and the keep
marks that fall on one album twice. It ends with the lines `--write` would
append, counted by `op`.

### The other open points

- **Wikipedia/Wikidata:** the album's store keys are those of all its
  versions, the lead's first.
- **Remove from clutter** acts on a version (as today on a Tidal album); an
  album is gone when its last version is; its plays stay in the log.
- **Tidal's Save for later** brings in a Tidal album as an album of its own;
  if it looks like one clutter has, that is a suggestion under "Same album
  as…", never a join by itself.
- **A new record** whose master is that of a record already in an album: the
  same — first suggestion under "Same album as…", one click.

### Discogs references — proposed 2026-11-26, LATB asked for the write-up

A Discogs release or master that **describes** an album without being owned: a
library CD ripped to files, a Tidal opera whose cast LATB wants from Discogs, the
releases in the old Files folder. Not a version (no access) and not a record
(nothing owned); nothing is written to Discogs, and no release is added to the
Discogs collection for it (LATB adds releases only for physical media).

- **Log line:** `{"op":"ref","album":…,"discogs":"release/123"}` or
  `"master/456"`; `"value":false` takes it off. An album may have several
  (a master and the release of the recording); folded in order, like `tag`.
  `album` is a `C` id or a version key, read through the membership as for
  `tag`. The page writes it against the album's `C` id where it has one (as for
  tags), so at a split it stays with the album; a reference an alias gives
  follows the version the alias names.
- **Writing it:** the album page's link box takes a Discogs URL
  (`discogs.com/release/…`, `/master/…`, with or without the slug) beside the
  stream links; `parseStreamRef` today knows Tidal, Apple and Qobuz only. The
  reference shows in the Versions block's area as "Discogs release … / master …"
  with an × to take it off (confirmation as for unlinks) and an Open-in link.
- **What the page shows:** for a release, its credits (`extraartists`, the
  tracks' own credits: the cast and roles of an opera), label, catalogue number,
  year, country; for a master, the same from its main release, with the master's
  year. Fetched when the page opens, after drawing, cached per album with the
  other details and refreshed by the refresh button (as Wikipedia and Tidal's
  review). Where the album has a record, the record's own release comes first and
  a reference adds only what differs. The tracklist from a reference is shown
  only when asked (as Tidal's for records today).
- **Old stand-ins:** an `alias` from a `D` key in Streaming or Files is read as a
  reference to that release as well (269 stand-ins, 51 of them in Files, all
  aliased since slice 6). The 18 albums in Files that have only file versions then show their
  Discogs data at once, without a new line. When LATB deletes the two folders the
  aliases stay in the log, so this keeps working.
- **Buying the record later:** a new record whose release or master is
  referenced by an album is the first suggestion under "Same album as…" (one
  click), as for a same-master record today. (An old stand-in's release joins by
  its alias already.)
- **Not in it:** search on Discogs from clutter (paste the URL found on the web);
  writing credits into clutter's own data; Discogs data in the lists or focuses
  (no `discogs.*` tags from references in the first version).

### Slices, each with its check

| # | What | Done when | Model |
|---|---|---|---|
| 0 | `album-layer-migrate.py`, dry run only | Runs on the real log without writing; every key is in exactly one album; the 1,307 groups are accounted for (before stand-ins and master joins); a second run prints the same; Doggystyle's two file copies are shown as two albums still (they are joined by hand in slice 3) | Opus |
| — | LATB reads the report; oddities fixed by hand or by rule | | LATB, Fable if a rule changes |
| 1 | Readers of the new lines: `Collection.hs` (and a new pure module for membership and lead) and `scripts/collection_log.py` for the scripts | Checks on a scratch log, in `cabal repl` and Python, give the same answers: last line wins, merge chain, merge back, split, alias chain, a loop ends, names by `C` id and by version key, `format` switches the old lines off | Opus |
| 2 | Server on the album layer, same pages as before | On a scratch copy of the log, migrated, server on 8085: every list has the same rows as the live server except the joined ones (a script compares, the differences are exactly the report's joins and aliases); a record's shelf row and an album's row match the focuses stated under Tags; plays in total per album equal today's sum; Versions block lists every version. `standIns`, the master sharing in `attachLinks` and `masterKey` are gone | Opus builds, **Fable reviews the diff before the commit** |
| 4 | The scripts read the log through `collection_log.album_log()` (`update-lengths.py`, `update-tidal-albums.py`, `jellyfin-inventory.py`, `import-inventory-links.py`, `update-jellyfin-albums.py`'s moved-folder search) | `./update-json` runs clean against the migrated scratch log | Opus |
| — | LATB runs `--write` on the real log, restarts | | LATB |
| 3 | Join, split and lead on the album page | "Same album as…" joins any two albums (a file album to anything, a second edition of a stream); × on a version splits it off; "lead" on a version; each writes the lines above; Doggystyle's second copy joined on a scratch log, then split, plays unchanged | Opus |
| 5 | Curated tags and rating: `tag` and `rate` lines; `scripts/import-discogs-curation.py` (genre folders, Petra, Darko/Lavorgna → `my.*`; ratings), dry run first; on the album page tags to add and take off, stars; a fixed view per `my.*` tag in the sidebar | Import counts equal the inventory above (1,313 + 48 tags less albums joined, 410 ratings); a tag set on a record's page shows on its Tidal version's page | Opus |
| 6 | Stand-ins retired | No play and no mark resolves to a release in Streaming or Files; then LATB deletes the two folders at Discogs; `CLUTTER_STANDIN_FOLDERS` and its code go | Opus, LATB |
| 8 | Discogs references (section above): `ref` lines in both readers, the link box takes Discogs URLs, credits on the album page, old stand-in aliases read as references, a referenced release or master suggested for a new record | On a scratch log: a release URL and a master URL pasted on a file album and on a Tidal album each write one line and show credits (an opera's cast with roles); × takes it off; at a split the reference stays as written (C id: the album; alias: its version); one of the 18 file-only Files albums shows its Discogs data with no new line; both readers agree in `album-layer-check.py` | Opus builds, reviewed before the commit |
| 7 | Later: record data into clutter (condition, notes, acquisition number, shelf and position), shelves edited in clutter, a view editor | own plan | — |

Slice 4 comes before `--write` (moved there after the review of 2026-11-26): once the
log has its format line, `collection_log.read()` returns no links, so scripts still on
it would see none.

Build and test recipe (scratch copy, sandbox, ports, headless Chrome): the
hub `~/Notes/Claude/ws/clutter.md` › Now, last bullet.

Status:

- Slice 0 — committed `45e833c` (2026-11-26).
- Slice 1 — built 2026-11-26, committed `5bed7e0`. `src/AlbumLayer.hs` (pure: events, membership,
  merges, aliases, keep/tag/rate/lead per album, the lead rule, a play's
  version), `Types.AlbumKey` gains `C`, `Collection.readAlbumLog` reads the
  file, `Collection.readCollection` drops link/unlink lines once a format
  line is in the log; `collection_log.py` gets `album_log()` with the same
  answers, and `read()` returns nothing after a format line. Check:
  `python3 test/album-layer-check.py` (both readers against answers worked
  out by hand, 36 each, OK). Also on a migrated scratch copy of the live log:
  1,296 albums with ids, 3,883 versions, 262 aliases, the same in both
  readers; no key in two albums; Doggystyle still two albums; old readers
  read no links. Not yet used by the server (slice 2).
  Found while testing: a move J1 → J2 plus an alias J2 → J1 looped forever in
  the first version of `canonical`; fixed, and the check covers it.
  Review by Fable, 2026-11-26, four points fixed: (1) a later merge or alias
  into an id undid an earlier one from it (merge b → c, then a → b left a's
  versions in b) — merges and aliases now drop an earlier line only for its
  exact reverse (`foldLinks`; moves keep `foldChain`); (2) a `C` id where a
  version key is meant is not read; (3) the album table is computed once in
  the fold (`alAlbums`; Python `self.albums`) — 1,296 `lead_of` + `versions`
  on the migrated copy take 1 ms; (4) slice 4 moved before `--write`. The
  check now has 59 answers, both orders of merge and alias and the five
  refused lines; both readers OK, and with the old rules put back the Python
  half fails on exactly those cases.
- Slice 2 — built 2026-11-26, committed `cd64d28`; **Fable reviewed it the same day**
  (by reading, nothing run), fixes below, compare rerun clean of anything new.
  Fixed: (1) `albumrefresh` compared Discogs's answer with the album's union
  tags, so every joined record looked changed and was taken in again on
  each page open — it compares with `baseR` now; (2) `rated.*` went into the
  lead's union before the rating was chosen, so an album could carry two
  ratings — album-level tags stay out of the union (0 rows with two ratings
  now); (3) the Apple twin of a Tidal album had the Tidal row but its own
  name (marks, plays, collection), so a row could be in Collection and Inbox
  at once — the twin now counts as the Tidal album's album; (4) a stand-in's
  rating did not reach its album's row — stand-ins (`Layer.lyStandIns`)
  donate `rated.*` (focuses `rated.like`/`rated.dislike` equal on both
  servers); (5) a file album linked from an album with no record (files +
  Tidal) moved alone and left the Tidal album stranded — it now brings that
  album along, and the answer says so; (6) a stream link that brings
  another album along says so (undone only with slice 3); (8) new ids avoid
  every id the log has used (`Layer.lyIds`: albums and both ends of merges;
  an id emptied entirely by splits is still not seen — chance of a clash
  about 1e-6); (10) bulk keep groups by album with a Map. Left as notes:
  merges go into the page's album (the spec says the one opened first —
  the same on a page); the Tidal list is ordered by each version's date
  before rows are taken; a split that does nothing on a stand-in page still
  answers "Took off".
  Before the review:
  The server keeps the albums as the readers give them (`Env.baseR`) and
  computes everything else from them in one place (`Env.derive` / `rebuild`,
  run after every log line, play or reload): `Collection.buildLayer` (album
  of every key, its row = the lead among the versions held, a play's album),
  `attachAlbums` (the lead gets the album's streams, file albums, the union
  of its versions' tags, the Wikidata keys of all; the other versions keep
  their own tags plus the album's `provider.*`, `storage.*`, `tidal.*`),
  `attachAlbumPlays` (plays by album, on every version), `spreadAlbumTags`
  (`length.*` the lead's, `rated.*` the first rated record's), and
  `attachCollection` by album. Lists map each key to its row and show an
  album once (`Provider.rowsByAdded`, `App.decodeListQuery`); a shelf list
  keeps its records and filters on a second tags index
  (`Collection.shelfIndexTags`, `Env.shelfTagsR`). `standIns`,
  `attachLinks` (with the master sharing), `masterKey`, `linkTidalTags`,
  `readCollection` and `playAlbumId` are gone. Writes: keep marks go against
  the C id; a stream linked on a page joins its album (a stream that is a
  version of another album brings that album along — the same stream, the
  same album) and a stream of the same service leaves it (`split`), as the
  page promised before; a file album linked moves alone (`split`, then
  `version`); unlinks are `split`s. Without the format line the server
  warns at start and refuses links. `reviewedFiles` counts `version` and
  `split` lines, and the review leaves out file albums in an album with an
  id. Versions block: `AlbumXJ.xversions` (every other version, lead first;
  an un-held stream by key) and rows for those the block did not show
  (other records, more editions, Apple Music, Qobuz); `xof` for every
  version that is not its album's lead.
  Found while testing: an album none of whose versions clutter holds (a
  Tidal and an Apple album that are no favourites, 143 albums) had lost its
  row, since the migration aliases its stand-in away; the stand-in now shows
  it (the first one, if two), and a play of such a version counts.
  Check (`test/album-layer-compare.py --versions --tags --focus …`), HEAD
  server with today's log on 8086 against this one with a migrated copy on
  8085: every list the same albums, each once, 700 old rows joined, all in
  albums with ids; shelves identical; plays per album equal (2,080 on 1,498
  albums), Listened lists equal; Versions complete for all 1,296 albums;
  24 albums with two records of different formats: the album row matches
  either format, a shelf row only its own. Differences left, all expected:
  `storage.music`/`storage.hirez` gone from 5 rows that had them only
  through the master sharing (Files/Streaming stand-ins, and Cream's
  *Wheels Of Fire*, whose hirez copy is its own album until joined by hand
  in slice 3); `played.never` off 38 albums whose other version was played
  (joins); `provider.applemusic`/`tidal.collection` on 3 album rows from
  their Tidal version's own tags (the union). `python3
  test/album-layer-check.py` OK (Python 59, Haskell 57: the server's old
  reader is gone). Page checked in headless Chrome (Blonde On Blonde from
  four versions, a stand-in row).
- Slice 3 — built 2026-11-26 (session 888b4579), committed `736c8b2`; built before slice 4
  at LATB's word (slice 3 does not need it; slice 4 is still needed before `--write`).
  Server (`App.hs`, `API24`): `/api/albumjoin/:aid?other=K` joins K's whole album to
  aid's (`joinLines`: a version line, a new id and two, or a merge into aid's;
  nothing is replaced, so a second edition of a stream or a second file copy stays
  beside the first); `/api/albumsplit/:aid?version=K` (a split line; refused for an
  album of one version or a key not in it); `/api/albumlead/:aid?version=K` (a lead
  line against the C id; refused for a version clutter does not hold; without
  `version`: back to the rule); `/api/albumfind/:aid?q=` (album rows whose title and
  artist words begin with all the words asked, not aid's album, 20 by title). Each
  answer (`RowJ`) carries the album's row afterwards, which the page shows.
  `Collection.appendLead`; `Layer.lyLeadSet` (albums whose lead a lead line chose);
  `AlbumXJ.xlead`, `xleadSet`. Suggestions (`candidatesFor`): any album row may be
  suggested for a file album or a stream (also another file album, another edition);
  a record only another record of the same Discogs master (by title, 182 record
  pages got suggestions, mostly other recordings of the same opera), never a
  Streaming/Files stand-in; numbers count as words ("Mind Maps 6" is not "7"). The
  unsure-matches review keeps out file albums as targets (it links with albumlink).
  Page: "Same album as…" on every album row: suggestions with **Join**, and a find
  box (Find → rows with Join); Versions: × on every version of an album of two or
  more (a split, confirmed), "lead" on a held version, "leads" / "leads (rule)" on
  the lead, ↺ back to the rule. The stream box (Link a stream) is unchanged: it
  still puts a stream of the same service in place of the one the album has.
  Check, on a migrated scratch copy of today's log, server on 8085, by the API and
  in headless Chrome (`clutter-w/slice3.mjs`): Doggystyle's second copy suggested on
  the first's page and joined (one version line into the album with its Tidal
  album; both copies' rows then carry the album's play and both storages), lead to
  the second copy and ↺ back, × splits it off again: both copies' tags and plays
  exactly as before. Cream's *Wheels Of Fire* hirez copy is suggested on its own
  page and joins the record (storage.hirez back on the record's row). A merge of
  two albums with ids works; taking a wrongly merged album apart again is a ×
  per version (no one-click undo of a merge). Refusals checked: lead twice, lead
  of an un-held version, rule twice, split twice, join of an un-held album.
  `test/album-layer-check.py` still OK (59 / 57). Not done: the bundle is not
  installed in `frontend/index.js` — the old live server has no `/api/albumjoin`,
  so Join would fail there; build it (`make run` in `frontend/`) with the restart
  after `--write`.
  **Fable reviewed it the same day** (by reading; Haskell type-checked): the lines
  written match the spec in every combination, JSON names match; four modest bugs,
  all fixed: (1) a stand-in's page of a stream clutter does not hold offered × and
  wrote split lines that change nothing, with a "Took … off" answer; (2) pages that
  are no version (a stand-in, an Apple twin) offered × and lead on their own row —
  the server now sends `xmembers` (`membersOf`: an album with an id, its versions
  without stand-ins; none for an album of one version), which the page and both
  endpoints use; (3) an Apple album that a Tidal version names cannot be split off
  (it would be that Tidal album's twin again), nor the Tidal album from an album of
  just those two — refused; (4) a split that leaves the rest of the album without
  a version clutter holds (keep mark and plays would go to an album nobody sees)
  is refused, naming the other version to take off instead. Nits fixed: × on the
  page's own version shows the album that remains; a late Find answer only lands
  on the page it was asked from; "has T…" no longer shown on a Tidal candidate
  (its own id); the join's version count leaves out stand-ins. Left as is: split
  and lead do not add a Back entry; the review's 8 suggestions are taken before
  its file albums are left out; a lead line whose version was split off leads
  again if that version rejoins (as the spec reads). Rechecked on a fresh
  migrated copy: 217 Streaming-list pages (199 with members, 13 stand-ins of an
  un-held stream now without × and lead), every refusal, and the page run.
- Slice 4 — built 2026-11-26 (session 859f28b2), committed `d559098`. `collection_log.py`
  gains `has_format()`, `release_links()` (the old `read()` shape per key —
  `streams` {kind: id}, `files` [jids] — from `album_log()` in the album format, from
  `read()` before it, so the scripts work on both sides of `--write`; a stand-in
  aliased away gets its album's versions) and `release_of_tidal()` (Tidal → release
  for every Tidal version, so an album with two Tidal editions keeps both);
  `file_refs()` also reads `album`, `from`, `to`. On it: `jellyfin-inventory.py`
  (FILES_OF), `update-lengths.py`, `update-tidal-albums.py`; `update-jellyfin-albums.py`
  through `file_refs()`. `import-inventory-links.py` and `import-location-links.py`
  write link lines and now refuse a log with a format line. Check, live log against
  a migrated copy, stores copied, all dry: `update-tidal-albums` and
  `update-wikidata` print the same; `update-lengths` 105 releases with a Tidal
  length instead of 101 (joins); Tidal → release 860 = 860, none lost, 38 to another
  release of the same album; release → Tidal 897 → 916, 10 now name the other
  Tidal edition of their joined album (fine for lengths: the server takes the
  lead's); file albums per release 445 → 471 releases, no old link missing;
  `jellyfin-inventory --offline` same counts and candidates, 35 rows name the other
  record of a joined album; `file_refs` 1,139 = 1,139. `album-layer-check.py`
  Python 59 OK (no Haskell changed). Not run: the network half of `./update-json`
  (Discogs and Tidal fetches, unchanged), and the scripts' writes.
  Reviewed the same day by a fresh Opus reviewer, with slice 0's `--write` path (never
  reviewed before). Slice 4: no bug; `release_of_tidal` takes `keep` (a release left
  the collection no longer hides its album's other release); `_lines` keeps only JSON
  objects. `--write`: the torn-end check never read the last byte (every run added a
  blank line) — fixed; the lines go in as one `O_APPEND` write and are read back
  (Python's buffered writer cut them into 8 KB pieces a server line could land
  between); refused: a log with migration lines but no format line (a partial run),
  a log that changed while the script ran, links to keys clutter does not hold
  (unless `--force`). Docstring: stop clutter first. Rechecked on copies of the live
  log: clean write (no blank line, same lines as before apart from `ts`), torn end
  (newline restored, same lines), second run and partial run refused, the comparison
  and dry runs unchanged. `--write` ran on the real log 2026-11-26 18:17.
- Slice 5 — built 2026-11-26 (session 859f28b2), committed `bccbfed`. LATB decided: tags
  `my.pop`, `my.piano`, `my.symphonic`, `my.concertos`, `my.chamber`, `my.opera` (all of
  Opera&Vocal), `my.petra`, `my.darkoLavorgna` (read as `my.darkolavorgna`: tags are
  lower case), and `my.vocal` offered but unused; records that disagree on the rating →
  the higher. `scripts/import-discogs-curation.py` (dry run default; `--write` copies
  the log to `collection.jsonl.before-curation`, appends with
  `collection_log.append_lines()` — one `O_APPEND` write, read back — and writes
  `~/Sync/clutter/curated-tags.json` (`$CLUTTER_CURATED_TAGS`: the tags clutter offers,
  with names, in the sidebar's order) if missing; refuses a second run and a log without
  the format line). Server: `AlbumLog.alRated` / `Layer.lyRated` (a rate line in the
  log); `Collection.attachCurated` after `spreadAlbumTags` puts the album's `my.*` tags
  and, once `lyRated`, its rating (`ratedTags`, the Discogs mapping) on every version
  and stand-in; until then the Discogs ratings stay. `/api/albumtag/:aid?tag&value`,
  `/api/albumrate/:aid?stars` (refused before the import: a lone rate line would hide
  every Discogs rating), `/api/curated` (the file's tags, then any `my.*` in use, with
  row counts). Page: Rating row with five clickable stars (the set star again: off), My
  tags row with chips (name → the view, × → off) and a "+ tag" picker; sidebar group
  "My tags": one view per tag (All, focus `#my.…`) with counts.
  Check, copies of the live log, server on 8085: without rate lines every rating and
  tag count equals the live server's, rating refused; after the import (1,732 lines:
  1,340 tags on 1,303 albums, 392 ratings; a second run refused) the `my.*` counts
  equal the report (Pop 502, Piano 231, Symphonic 225, Concertos 66, Chamber 99, Opera
  169, Petra 35, Darko/Lavorgna 13, Vocal 0), 5★ 149 → 150 (Blonde On Blonde, the
  higher rating); `rated.not` now covers every unrated album, streams and files too
  (11,907 rows, was 1,552 Discogs releases). A tag set on Blonde On Blonde's record
  shows on its Tidal version and back; rate 3 / 0 / 5 on either; a bad tag refused.
  Headless Chrome: stars, picker, ×, sidebar view (231 rows, `#my.piano` chip, count
  refreshed after tagging). Reviewed the same day by a fresh Opus reviewer; fixed: a
  record's shelf row kept its own Discogs `rated.*` beside the album's (`shelfIndexTags`
  now takes album-level tags only from the album: 32 shelf lists, no row under two
  ratings, also after re-rating); the list cards' stars (`albumRating`) follow clutter's
  rating once the log has one; the import writes `my.darkolavorgna` in lower case, takes
  the log's stat before its first read, and writes `curated-tags.json` beside the log
  (a test run on a copy no longer creates the live file); sidebar counts reload after a
  join or split; `album-layer-check.py` checks the rated flag in both readers (Python
  60, Haskell 58 OK). Left as the spec reads (for LATB): a tag or rating written while
  an album had one version follows that version when it is split off later — the album
  it leaves loses it. Not done: tags on the list cards; a tag editor (rename, order, new
  tags only by editing `curated-tags.json`); tagging or rating from a version tab
  returns to the album tab. Imported on the real log 2026-11-26.
- Slice 6 — started 2026-11-26 (session 859f28b2), committed `84eb246`. `scripts/standin-retire-check.py`
  (read-only): Discogs without the Streaming and Files folders, as the server would
  read it (a Tidal store entry counts only with album data); `--tidal-out FILE` lists
  the un-held Tidal albums. First run: 269 stand-ins (262 aliased), 151 albums would
  vanish — 144 for an un-held Tidal album (143 distinct), 1 Apple, 7 Files stand-ins
  never aliased — with 3 plays, 9 tagged, 23 rated. LATB decided (all as recommended):
  take the Tidal ones into the store; alias the 7 to their file albums (all found in
  Jellyfin); let the Apple one (The Lincoln Trio) go. `update-tidal-albums.py --take
  FILE` (fetch into the store like a played non-favourite, `taken` date; already-held
  ids left alone): 124 of 143 fetched (store copied first to
  `tidal-albums.json.before-take`), 19 answer 404 (gone from Tidal). Searched with the
  user token (`api.tidalhifi.com/v1/search/albums`): 11 clear matches under a new id
  (taken: 4 fetched, 7 held already), LATB OK to alias old → new; 3 uncertain and 5 not
  found — LATB looks them up in Tidal. Prepared, not written: 18 alias lines (11 T → T,
  7 D → J) in `cache/standin-retire/alias-lines.jsonl`, for when clutter is stopped.
  Simulated on a copy of the log with them: 269/269 aliased, 0 plays lost, 9 albums
  would vanish (LATB's 8 plus the Apple one; 1 rated: Nikolayeva 4★). LATB's links
  (session 6bbf0c46): Istomin/Rose/Stern → T34219983 (Isaac Stern Collection, Trio
  Recordings Vol. 1), Böhm Macbeth → T20701447, Nikolayeva Goldberg → T366233911
  (Remastered 2024); taken (store copied first), 3 alias lines added → 21. The other 5
  and the Apple one go (LATB). Simulated: 0 plays lost, 6 albums would vanish, none
  marked, tagged or rated — that is the accepted "nothing lost". 21 alias lines written
  to the real log with clutter stopped (copy `collection.jsonl.before-standin-retire`);
  check on the real log: the same 6, 0 plays lost; Python album-layer check OK; the three
  new ids join the old albums (Nikolayeva keeps 4★). Still to do:
  LATB deletes the two folders at Discogs, then the stand-in code goes (server:
  `standInFolderTags`, `CLUTTER_STANDIN_FOLDERS`, `lyStandIns`, stand-in rows and
  rating donation, `candidatesFor`'s exclusion, `attachCollection`'s record test;
  scripts: the migration's stand-in step). LATB 11-26: the two folders stay at
  Discogs for now (no new releases go into them); the stand-in code stays with them.
- Slice 8 — built 2026-11-26 (session 6bbf0c46), committed `84eb246`, LATB asked for it the same
  day. Readers: `AlbumLayer` (`Ref`, `DRef`, `parseRef`, `refsOf`, `alRefs`: aliases from a
  `D` key first, then `ref` lines folded) and `collection_log` (`parse_ref`, `refs_of`);
  `album-layer-check.py` 73/71 answers OK (refs by key and C id, taken off, alias-given,
  split, unreadable lines). Server: `Collection.appendRef`, `parseDiscogsRef` (discogs.com
  URLs incl. `/de/…`, query and slug; `release/…`, `r…`, `m…`), `Layer.lyRefs`,
  `lyRefIndex`; `/api/albumref/:aid?ref=…&value=…` (refuses: not Discogs, already there,
  not there, a release that is a record of the album), `/api/albumrefq/:aid[?refresh=1]`
  (`Tracks.discogsRef`, cached `album-details/discogs-release-N.json` /
  `discogs-master-N.json`; a master shows its main release with the master's year),
  `AlbumXJ.xrefs`; `candidatesFor` puts an album whose reference names a record's release
  or master first. Page: side block "Discogs" (each reference ↗ and ×, Add box), main
  "From Discogs" (label, catalogue number, format, country, year; credits open, tracklist
  folded); a Discogs address in the stream box goes to the same place. On the live log
  269 references come from the stand-in aliases (268 albums). Tested on 8085 with a copy
  of the live log: Macbeth (T20701447) shows the Orfeo release's cast, a master URL added
  and taken off (confirm), refusals, Eric Burdon's file album shows its release with no new
  line, a reference on another album suggested on Blonde On Blonde's record page (release
  and master), split keeps a C-id reference with the album; headless Chrome
  (`s8shot.mjs`). Discogs answers a bad token with 401: a release needs no token.
  Reviewed the same day (fresh Opus reviewer, slices 6 and 8 together): nothing serious;
  fixed: `"value":null` read alike in both readers (not read; Haskell `.:!`), the Discogs
  and stream boxes emptied when another album opens, the stream box routes a Discogs
  address case-insensitively (also `release/…`, `master/…`), `Release/123` and
  `/master/view/456` parsed, `refresh` only for `1`; check 74/72 OK, rechecked on 8085.
  Left as noted: any alias from a `D` key gives a reference (only stand-in aliases
  exist); references fetched one after another, errors not cached; the check script
  never counts Apple albums held (The Lincoln Trio's is not held: verified).
  Committed `84eb246`. First UI change (LATB, 11-26): one box "Link a stream or Discogs
  release" (on a stream's own page "Link a Discogs release": no streams listed there)
  instead of two; it takes `D<id>` as a Discogs release (refused if that release is a
  record of the album) besides `T`, `A`, `Q`, `r`, `m` and links; tested on 8085;
  committed `d2af312`. Then (LATB, 11-26): ↗ in Versions went to the start page — a
  link to `#` was followed and read as history entry 0; `GetStuff.initHistory` now
  stops the browser following `a[href="#"]` (all eight such links). Versions lists
  only the versions clutter holds (a Tidal album held or not); an Apple Music or
  Qobuz album that is only linked stays under "Open in" and in the link block. An
  Apple catalogue store, so linked Apple albums are held like Tidal's: `backlog.md` ›
  Apple Music, after LATB's list of UI issues.

### Questions for LATB — answered 2026-11-26: OK to all five

1. Album ids stay inside the log and the server; pages and lists keep the
   version keys. OK?
2. The migration joins releases of the same Discogs master by itself (listed
   in the report first); afterwards a same-master record is only suggested.
   OK, or suggestions only from the start?
3. Lead order — record, Tidal, files, Apple, Qobuz — and the album's "date
   added" is its lead's. OK?
4. Prefix `my.` for the curated tags?
5. The Apple Music album found for a Tidal album by lookup is not a version
   until linked. OK?

### Slice 9 — face and play default (proposed 2026-11-27; LATB chose "best quality")

"Lead" did two jobs under one word. Split:

- **Face** (was "lead"): what an album's card shows in the lists — title,
  artist, cover, year, date added, playing time, row key. The rule stays
  (record › Tidal › files › Apple › Qobuz); a `lead` line still overrides it.
  The page no longer says "leads (rule)": the face row gets a small grey
  "cover" mark, any other held row a "use as cover" button, ↺ back to the rule.
- **Play default**: the version the album page starts on (facts, tracks, ▶).
  - Opened from a version list — `Server …`, a shelf, Tidal, Discogs — the
    version opened, as today.
  - Opened from an album list (All, Collection, Inbox, My tags, Listened,
    search, a strip): the playable version of the best quality.
- **Quality rank**: Hi-Res (24-bit or above 48 kHz; DSD; Tidal
  `HIRES_LOSSLESS`) › Lossless (16-bit ≤ 48 kHz; Tidal `LOSSLESS`) › lossy ›
  unknown. A tie goes to Tidal (it starts faster on the Node — the 11-25
  decision, kept where quality does not decide). A record is not playable and
  never the play default.
- **Where quality comes from**: Tidal from its store and the details cache
  (`mediaMetadata.tags`); files from the tracks cache (`atQuality`), which the
  server fills in the background for file albums that share an album with
  another playable version (670 albums today, one Jellyfin request each,
  one at a time) and for new ones after each update. This is also the backlog
  item "tracks cache for file albums ahead of time", for those albums.
- **Versions block**: rows in a fixed order — face first, then the rule's
  order — each one line, with its quality ("Hi-Res", "FLAC 24/96"); the
  version on show is highlighted, not moved.

**Built 2026-11-27 — what changed from the proposal while building:**

- Filling the tracks cache ahead of time also fetched each album's Tidal
  review and Wikipedia lead (Wikipedia answered 429 within a minute). The fill
  now asks Jellyfin for the tracks alone and keeps only the quality, in
  `~/Sync/clutter/file-quality.json` (`$CLUTTER_FILE_QUALITY`; `""` = Jellyfin
  gave none, not asked again); the tracks cache stays as it was and still wins
  where it has a quality. 1,040 file albums share an album with another
  version (not 670: that was files + Tidal only). Stops after ten failures in
  a row with none done (Jellyfin not reachable); writes every 50 albums.
- Only what ▶ can play takes part: files whose folder the player has
  (`FileV.fvNode`, from its shares) and a Tidal album not gone. Without the
  share, a Hi-Res file album does not win over Tidal.
- A record's page stays on the record (its Discogs tracklist — the 11-22
  decision); its ▶ plays the best version instead of Tidal by default.
- Version lists are `Tidal`, `Apple Music` and `Server …`; an album page reached
  from another page (a strip, ↗, a join) also keeps the version opened. A
  shelf is a list of records, so it counts as an album list (the record's page
  stays on the record anyway).
- Labels: "album cover" (grey) on the face row, a "cover" button on the others.
- Code: `frontend/src/Quality.purs` (rank, best version), `AlbumComponent`
  (`autoFrom`, `loadDetail auto`), `Render` (Versions rows, record ▶),
  `App.fillFileQuality`, `Tracks.cachedFileQuality` and the store.
- Tested on 8085 with a copy of the live log and tracks cache, a stand-in
  Jellyfin and a stand-in Node, in headless Chrome: I Love My Computer (Tidal
  Hi-Res vs FLAC 24/44.1 → Tidal), Red (FLAC 24/96 vs an un-held Tidal album →
  files, Tidal once the Node lacks the share), From Her To Eternity with its
  files set to 24/96 (→ files from All, Tidal from the Tidal list and without
  the share), Blonde On Blonde (record page, ▶ Tidal on a Hi-Res tie).

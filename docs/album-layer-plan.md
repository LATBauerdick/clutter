# The album layer — plan

Status 2026-11-26: **model agreed with LATB (he read and agreed to everything
down to "Left for later"); the spec below it — ids, log lines, migration,
slices — agreed too (OK to all five questions, 2026-11-26). Slice 0 built 2026-11-26
(`scripts/album-layer-migrate.py`, dry run; report in the vault,
`~/Notes/Claude/2026-11-26-clutter-album-layer-dry-run.md`); nothing else built.** Follows `backlog.md` › The album layer and
`jellyfin-plan.md` › the model (album → versions → access).

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

Where a version key is meant — `key` of `version`, `split` and `lead`, both ends of
`alias` — a `C` id is not read (it would make an album a version of an album).
| `{"op":"keep","key":…,"value":…}` | As today; `key` may be a `C` id. Last line for the album counts. |
| `{"op":"tag","album":…,"tag":"piano","value":true}` | Put a curated tag on the album, or take it off (`false`). Folded in order. |
| `{"op":"rate","album":…,"stars":4}` | The album's rating, 1–5; 0 takes it off. Last line counts. |
| `moved`, `skip`, `notsame` | Unchanged. |

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

Slice 4 comes before `--write` (moved there after the review of 2026-11-26): once the
log has its format line, `collection_log.read()` returns no links, so scripts still on
it would see none.
| 5 | Curated tags and rating: `tag` and `rate` lines; `scripts/import-discogs-curation.py` (genre folders, Petra, Darko/Lavorgna → `my.*`; ratings), dry run first; on the album page tags to add and take off, stars; a fixed view per `my.*` tag in the sidebar | Import counts equal the inventory above (1,313 + 48 tags less albums joined, 410 ratings); a tag set on a record's page shows on its Tidal version's page | Opus |
| 6 | Stand-ins retired | No play and no mark resolves to a release in Streaming or Files; then LATB deletes the two folders at Discogs; `CLUTTER_STANDIN_FOLDERS` and its code go | Opus, LATB |
| 7 | Later: record data into clutter (condition, notes, acquisition number, shelf and position), shelves edited in clutter, a view editor | own plan | — |

Build and test recipe (scratch copy, sandbox, ports, headless Chrome): the
hub `~/Notes/Claude/ws/clutter.md` › Now, last bullet.

Status:

- Slice 0 — committed `45e833c` (2026-11-26).
- Slice 1 — built 2026-11-26. `src/AlbumLayer.hs` (pure: events, membership,
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
- Slice 2 — built 2026-11-26, uncommitted; **Fable reviewed it the same day**
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

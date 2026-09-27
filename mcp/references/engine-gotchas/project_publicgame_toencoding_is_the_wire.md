---
name: project_publicgame_toencoding_is_the_wire
description: "PublicGame defines both toEncoding and toJSON with duplicated where-blocks; only toEncoding reaches the client, so fixes applied to toJSON are dead code"
metadata: 
  node_type: memory
  type: project
  originSessionId: 9b1cdf5f-c5e5-466a-bf34-6af1f23d3667
  modified: 2026-08-05T07:30:28.337Z
---

`instance ToJSON gid => ToJSON (PublicGame gid)` (`Arkham/Game.hs`) implements **both**
`toEncoding` and `toJSON`, each with its own copy of the `where` block. Only `toEncoding`
ever reaches the client: `library/Orphans.hs` has
`instance ToJSON a => ToContent a where toContent = toContent . toEncoding`, and
`GetGameJson` (`Api/Handler/Arkham/Games/Shared.hs`) uses `genericToEncoding`, which calls
the nested value's `toEncoding`.

The two copies had silently drifted for a year: `eec501adab` (2025-09-06) added `toEncoding`
as a copy of the then-current `toJSON`, then `31616d0a35` (2025-09-30) fixed
`otherInvestigators` **only in `toJSON`** to read `otherCampaignPlayers` (real xp/trauma)
instead of `otherCampaignAttrs.decks` + `lookupInvestigator` (fresh, 0 xp). The client kept
getting 0-xp placeholders — the Dream-Eaters bug in #5338.

**Why:** aeson prefers `toEncoding` when serialising to bytes; a `toJSON` you can read in
ghci is not what the browser receives. Verifying by eyeballing `toJSON` proves nothing.

**How to apply:** never edit one half of that instance alone. Extract shared logic into a
top-level helper both call (`publicOtherInvestigators`, `asPublicInvestigator` now do this).
To verify what the client actually gets, hit the API — `curl .../api/v1/arkham/games/<id>
-H "Authorization: Token <t>"` — rather than reasoning from the source. `arkham-replay`
cannot help here: it dumps `Game`, not `PublicGame`.

Local API verification recipe: POST `/api/v1/authenticate` with
`{"email","password"}` → token; auth header is `Authorization: Token <t>` (**not** Bearer);
`POST /api/v1/arkham/games/import?multiplayerVariant=Solo` takes a **multipart** upload
(`curl -F "file=@export.json"`), not a raw JSON body.

Related: [[reference_blob_verification]], [[project_dream_eaters_split_campaign_log]]

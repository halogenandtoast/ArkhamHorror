---
name: project_investigatorfromattrs_reseeds_metadata
description: "investigatorFromAttrs re-seeds `With` metadata from the passed attrs, so rebuilding an existing investigator with it destroys Body of a Yithian / Shattered Self snapshots — use overAttrs"
metadata: 
  node_type: memory
  type: project
  originSessionId: 5fd79dcb-efa5-4ba5-b402-cf04ce7e9051
  modified: 2026-07-31T05:18:40.914Z
---

`investigatorFromAttrs :: InvestigatorAttrs -> a` **constructs a fresh entity**, so for
investigators carrying `With` metadata it re-seeds that metadata from the attrs you hand
it. `BodyOfAYithian.investigatorFromAttrs attrs = attrs \`with\` YithianMetadata (toJSON
attrs)` (and `ShatteredSelf` likewise) — calling it on an *existing* Yithian overwrites
the snapshot of the investigator they used to be with a copy of the Yithian itself.

The `TransfiguredForm` branch of `instance RunMessage Investigator`
(`Arkham/Investigator/Runner.hs`) did exactly that after every message, so playing
Transfiguration (2) as a Yithian permanently destroyed the original body. `EndOfGame`
then reverts the form, dispatch returns to `BodyOfAYithian`, and the game dies with
`error "the original mind of the Yithian is lost"` (#5316).

**Why:** rebuilding an entity you already have is a construction, not an update; only
`overAttrs` knows to keep the `b` in `a \`With\` b`
(`Arkham/Classes/Entity.hs`: `overAttrs f (a \`With\` b) = With (overAttrs f a) b`).

**How to apply:** to swap attrs into an investigator value you already hold, use
`overAttrs (const newAttrs) existing`. Reserve `investigatorFromAttrs` for genuinely new
entities (parsing `otherCampaignPlayers`, projecting a transfigured *form* whose meta is
meant to start uninitialized). The same trap applies to every `With`-metadata
investigator (Tony Morgan, Ursula, Leo, Joe Diamond, Luke, Lily Chen, Wilson, parallel
Jenny, Subject 5U-21) — they were silently resetting their metadata every message while
transfigured.

Recovery for already-corrupted saves: an alternate body keeps the original
`investigatorId`, so `rebuildFromAlternateBody` (`Arkham/Investigator.hs`) restores the
printed identity from `lookupInvestigator attrs.id`; `returnToBody`,
`returnFromShatteredSelf`, and both card runners fall back to it instead of `error`ing.
`yithianOriginalCardCode` reads the snapshot for tests.

Related: [[project_transformed_investigator_identity]], [[project_truemagick_metadata_lifetime]]

---
title: mercury_kind_codec
description: "`KindCodec` serializes a tree of promoted kinds as namespaced text (`HigherX:MyKindA`) with a default instance via `Show`, and round-trips back to a `SomeSing`"
---

**Source:** `local-packages/mercury-glassbox/src/Mercury/Common/KindCodec.hs`.

```haskell
class KindCodec a where
  kindCodec :: KindCodecDict a
  default kindCodec :: (Enumerable a, Show a) => KindCodecDict a
  kindCodec = fromFn tshow          -- flat enums cost one empty instance

  encode :: a -> Text
  decode :: Text -> Maybe a

encodeS :: (KindCodec k, SingKind k, DemotedEquality k) => Sing a -> Text
decodeS :: (KindCodec k, SingKind k, DemotedEquality k) => Text -> Maybe (SomeSing k)
```

Nested kinds encode with a namespace separator rather than nested JSON objects:

```haskell
instance KindCodec MyHigherKind where
  kindCodec = KindCodec.create encoder decoder
    where
      encoder = KindCodec.mkNestedEncoder \case
        HigherX k -> KindCodec.nestE "HigherX" k
        HigherY   -> "HigherY"
      decoder = KindCodec.mkNestedDecoder
        [ ("HigherX", KindCodec.nestD HigherX), ("HigherY", const (pure HigherY)) ]
```

Two ideas: the **default method via `Show`** means the common flat-enum case costs an
empty instance while the nested case stays explicit; and `DemotedEquality k = (Demote k ~ k)`
is a tidy constraint synonym asserting a promoted kind demotes to its own type, which
makes singleton round-tripping ergonomic.

The design note is honest about scope: "simplistic right now, but enough to make our
framework-internal boilerplate readable enough for a human to verify easily" — the
encoding exists to make persisted tags *greppable*, not to be clever.

**How to apply here:** the persistence layer already learned the hard way that structural
snapshots of rich types go stale (`project_persisted_carddef_snapshots_go_stale`) and that
tag-shaped decoders silently drop payloads (`project_other_modifier_decoder_drops_contents`).
A flat, namespaced, human-greppable text encoding for tag-like sums — `Placement`,
`Window`, `Modifier` tags — is easier to diff in a saved game than nested JSON objects.
Related: [[mercury_typelevel_state_machine]].

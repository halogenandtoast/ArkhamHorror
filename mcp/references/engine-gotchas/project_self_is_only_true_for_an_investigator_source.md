---
title: project_self_is_only_true_for_an_investigator_source
description: Criteria.Self asks whether the ability's source IS the acting investigator, so on an asset, event or skill it is always false and quietly switches the ability off
---

```haskell
Criteria.Self -> case source of
  InvestigatorSource iid' -> pure $ iid == iid'
  _ -> pure False
```

`Self` is about an **investigator card's own ability**. On any other card the
source is an `AssetSource`/`EventSource`/`SkillSource` and the criterion is
false, so the ability it guards never fires -- which looks exactly like a card
that has nothing to do.

It reads as "this copy, not the other one", which is what put it on Feats of
Daring, a custom asset with two permanents in play. The thing that actually
distinguishes one copy from another is its own state -- what it remembers, what
is attached to it -- not `Self`.

The custom-card checker now rejects `Self` in the meta of a non-investigator
card.

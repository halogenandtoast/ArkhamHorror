---
title: project_per_investigator_limit_only_knew_one_window
description: PerInvestigatorLimit read the seat out of the window, but only understood CommittedCard — on any other window it answered False, so the limit switched the ability off entirely
---

`PerInvestigatorLimit PerRound 1` means "once per investigator per round", and
which investigator depends on the window. The check handled exactly one window
type:

```haskell
case windowType window of
  Window.CommittedCard iid' _ -> …
  _ -> pure False
```

`False` from a limit check means **not performable**, so the ability was never
offered on any other window. Minh Thi Phan is the only official card with this
limit and her window is `CommittedCard`, which is why it went unnoticed.

A card with a `PerInvestigatorLimit` on, say, `InitiatedSkillTest` is therefore
not rate-limited -- it is switched off, and nothing says so. Feats of Daring
(Circus Ex Mortis) read as a correct card, published a correct ability, and was
never once offered.

Now the seat comes from `windowInvestigator`, which reads the first
`InvestigatorId` the window carries (`listToMaybe . toListOf biplate`) rather
than enumerating windows -- there are hundreds, and a hand-written list goes
stale the moment one is added. A window that names no investigator still has no
bucket, which is the honest answer for it.

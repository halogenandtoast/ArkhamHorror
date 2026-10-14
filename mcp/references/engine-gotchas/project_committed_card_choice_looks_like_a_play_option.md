---
title: project_committed_card_choice_looks_like_a_play_option
description: Committing a card and playing it are both TargetLabel + CardIdTarget, and no fast player window opens between initiating a test and starting it
---

At test initiation the only question is the **commit** window: `TargetLabel`
choices over `CardIdTarget`, each carrying `SkillTestCommitCard_`, plus
`StartSkillTestButton`. A *play* option in a player window is also `TargetLabel`
over `CardIdTarget`, carrying `InitiatePlayCardWithWindows`. The target and the
label are identical, so only the message inside the choice tells them apart.

Two consequences for anything that drives a game from outside the UI:

* A card that moved from hand to discard was not necessarily played. A committed
  card leaves the hand and is discarded when the test ends, exactly like a fast
  event played in a window, and both log lines are one `log.commits` apart.
* **No fast player window opens between initiating a test and starting it.** The
  commit question is followed by `SkillTestApplyResultsButton`, then the ordinary
  player window. A card whose play window is `InitiatedSkillTest` is offered
  *inside* that commit question, so it is only there when its matcher really
  fits — a "when you initiate a fight or evade test" card is correctly absent
  from an investigate, and picking the same-named commit option instead reads as
  a card that worked.

This matters most for the Circus Ex Mortis events, which all carry skill icons
and so appear in both lists. Match on `InitiatePlayCard` in the chosen choice's
messages before believing a card was played, and read `log.commits` in the game
log to tell the two apart after the fact.

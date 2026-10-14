---
title: project_anywindow_as_a_play_window_makes_the_card_unplayable
description: cdFastWindow = AnyWindow offers the card everywhere including inside its own play, so the play never completes — a bare "Fast." is FastPlayerWindow
---

A card whose printed text is just "**Fast.**", with no "Play when ..." clause,
wants `FastPlayerWindow` -- what all 19 official unrestricted fast events use.
(The other common one is `DuringTurn You`, for "Fast. Play during your turn.",
on 56 of them.)

`AnyWindow` looks like the right way to say "any time", and is not: it also
matches the windows opened **while the card is being played**, so the play never
finishes. The card is offered in the player window, clicking it does nothing at
all, and no error is raised -- the same shape as
[[project_always_true_forced_on_anywindow_makes_a_card_unplayable]], from the
same cause.

No official card uses `AnyWindow` as a play window. Four custom printings did
(Firewalking ×2, Multitasking (1), Prestidigitation (1)) and every one of them
was dead on the table while reading as a finished card.

Found by sweeping every printing with a driver that plays the card and diffs
the game state: an offered card whose diff is empty is the tell, and it is the
only tell.

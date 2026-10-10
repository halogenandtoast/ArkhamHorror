module Arkham.Homebrew.AgesUnwound.Acts.TheFirstCircle (theFirstCircle) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.Helpers (isAtOrPastFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Ritual))

newtype TheFirstCircle = TheFirstCircle ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- | Act 3c, the last act of the past-selves deck.
theFirstCircle :: ActCard TheFirstCircle
theFirstCircle = act (3, C) TheFirstCircle Cards.theFirstCircle Nothing

{- | "Forced - After you end your turn at a [[Ritual]] location: Test [willpower]
or [agility] (3). Take 1 horror for each point you fail by. /
Objective - When /the investigators fell to the Myriad,/ advance. /
Objective - When /the investigators broke the first circle,/ advance."

Ability 1 is the printed Forced. Ability 2 is the past deck's once-a-round poll;
see 'Arkham.Homebrew.AgesUnwound.Acts.WhatCameBefore' for why it is silent.
-}
instance HasAbilities TheFirstCircle where
  getAbilities (TheFirstCircle a) =
    [ restricted a 1 (exists $ YourLocation <> LocationWithTrait Ritual)
        $ forced
        $ TurnEnds #after You
    , mkAbility a 2 $ SilentForcedAbility $ MythosStep AfterCheckDoomThreshold
    ]

instance RunMessage TheFirstCircle where
  runMessage msg a@(TheFirstCircle attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseOrRunOneM iid $ for_ [#willpower, #agility] \skill ->
        skillLabeled skill $ beginSkillTest sid iid (attrs.ability 1) iid skill (Fixed 3)
      pure a
    -- "Take 1 horror for each point you fail by" -- one assignment of N, and
    -- nothing in the loop makes its own choice, so it needs no 'doStep'.
    FailedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      assignHorror iid (attrs.ability 1) n
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      conditionMet <-
        orM
          [ isAtOrPastFor TheInvestigatorsFellToTheMyriad
          , isAtOrPastFor TheInvestigatorsBrokeTheFirstCircle
          ]
      when conditionMet $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      {- "All Caught Up: Remove the remainder of this act deck from the game."

      This is the last act in the deck, so the remainder is this card. Removing it
      leaves act deck 2 with nothing in play, which is also what makes /Preserve
      Causality/ and /You Must Not Be Seen/ go back to surging: with your past
      self gone there is no causality left to preserve. -}
      push $ RemoveCompletedActFromGame (actDeckId attrs) attrs.id
      pure a
    _ -> TheFirstCircle <$> liftRunMessage msg attrs

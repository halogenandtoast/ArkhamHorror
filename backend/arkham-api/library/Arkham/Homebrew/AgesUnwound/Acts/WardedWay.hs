module Arkham.Homebrew.AgesUnwound.Acts.WardedWay (wardedWay) where

import Arkham.Ability
import Arkham.Act.Import.Lifted
import Arkham.Homebrew.AgesUnwound.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.AgesUnwound.CardDefs.Treacheries qualified as Treacheries
import Arkham.Homebrew.AgesUnwound.Helpers (isAtOrPastFor)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.Homebrew.AgesUnwound.Scenarios.AWorldTornDownAgain.Helpers (advanceToActThreeD)
import Arkham.Matcher

newtype WardedWay = WardedWay ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Act 2c, reached when the investigators snuck into the back of the school a
year ago --- which is also the branch on which Scenario III recorded what happened
to the arcane ward.
-}
wardedWay :: ActCard WardedWay
wardedWay = act (2, C) WardedWay Cards.wardedWay Nothing

{- | "Objective - When /the investigators unleashed chaos,/ advance. /
Objective - When /the investigators dispelled the ward,/ advance. /
Objective - When /the investigators fell to the Myriad,/ advance to Act 3d."
-}
instance HasAbilities WardedWay where
  getAbilities (WardedWay a) =
    [mkAbility a 1 $ SilentForcedAbility $ MythosStep AfterCheckDoomThreshold]

instance RunMessage WardedWay where
  runMessage msg a@(WardedWay attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      conditionMet <-
        orM
          [ isAtOrPastFor TheInvestigatorsUnleashedChaos
          , isAtOrPastFor TheInvestigatorsDispelledTheWard
          , isAtOrPastFor TheInvestigatorsFellToTheMyriad
          ]
      when conditionMet $ advancedWithOther attrs
      pure a
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      {- Both ward entries are written by Scenario III's back-door branch, which is
      the only branch that reaches this act, so re-reading the log here is stable.
      /Backfire/ can record "unleashed chaos" during Scenario VI as well, but only
      on the rear-corridors branch -- which puts act 2c /Hellish Hound/ in play,
      not this act. -}
      chaos <- isAtOrPastFor TheInvestigatorsUnleashedChaos
      dispelled <- isAtOrPastFor TheInvestigatorsDispelledTheWard

      if chaos
        then do
          {- "Chaos: Shuffle each set-aside copy of Unleashed Chaos into the
          encounter deck. Advance to Act 3c - The First Circle." -}
          shuffleSetAsideIntoEncounterDeck
            $ mapOneOf
              cardIs
              [ Treacheries.unleashedChaosIAccelerationI
              , Treacheries.unleashedChaosIProliferationI
              , Treacheries.unleashedChaosIMutationI
              ]
          advanceToAct attrs Cards.theFirstCircle C
        else
          if dispelled
            then advanceToAct attrs Cards.theFirstCircle C
            else advanceToActThreeD attrs
      pure a
    _ -> WardedWay <$> liftRunMessage msg attrs

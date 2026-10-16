module Arkham.Homebrew.TheInvisibleMan.Acts.FightingChance (fightingChance) where

import Arkham.Act.Import.Lifted
import Arkham.Classes.HasGame
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Acts qualified as Cards
import Arkham.Homebrew.TheInvisibleMan.CardDefs.Stories qualified as Stories
import Arkham.Homebrew.TheInvisibleMan.Helpers
import Arkham.Homebrew.TheInvisibleMan.ScenarioDeckKeys
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype FightingChance = FightingChance ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

fightingChance :: ActCard FightingChance
fightingChance =
  act (2, A) FightingChance Cards.fightingChance (Just $ GroupClueCost (PerPlayer 1) Anywhere)

{- | Act 2d, "Outfoxed". Nothing records which options have been taken: each
one is legible in the Invisible Combat deck itself, which is also what makes
the fourth conditional on the third.
-}
instance RunMessage FightingChance where
  runMessage msg a@(FightingChance attrs) = runQueueT $ scenarioI18n $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      lead <- getLead
      (everywhere, grasping, signs, number) <- outfoxedState
      scope "fightingChance" $ chooseOneM lead do
        when everywhere $ labeled "removeEverywhereAndNowhere" do
          removeFromScenarioDeck InvisibleCombatDeck Stories.everywhereAndNowhere
        when grasping $ labeled "removeGraspingAtStraws" do
          removeFromScenarioDeck InvisibleCombatDeck Stories.graspingAtStraws
        unless signs $ labeled "shuffleInSubtleSigns" do
          shuffleSetAsideIntoScenarioDeck InvisibleCombatDeck (cardIs Stories.subtleSigns)
        when (signs && not number) $ labeled "shuffleInGotHisNumber" do
          shuffleSetAsideIntoScenarioDeck InvisibleCombatDeck (cardIs Stories.gotHisNumber)
      doStep 1 msg
      pure a
    DoStep 1 (AdvanceAct (isSide B attrs -> True) _ _) -> do
      -- "If each option has been chosen, remove this act from the game.
      -- Otherwise, flip back to act 2c."
      (everywhere, grasping, signs, number) <- outfoxedState
      if everywhere || grasping || not signs || not number
        then advanceToAct attrs Cards.fightingChance A
        else push $ RemoveCompletedActFromGame (actDeckId attrs) attrs.id
      pure a
    _ -> FightingChance <$> liftRunMessage msg attrs

-- | Which of the four options are still open, in printed order.
outfoxedState :: HasGame m => m (Bool, Bool, Bool, Bool)
outfoxedState =
  (,,,)
    <$> inScenarioDeck InvisibleCombatDeck Stories.everywhereAndNowhere
    <*> inScenarioDeck InvisibleCombatDeck Stories.graspingAtStraws
    <*> inScenarioDeck InvisibleCombatDeck Stories.subtleSigns
    <*> inScenarioDeck InvisibleCombatDeck Stories.gotHisNumber

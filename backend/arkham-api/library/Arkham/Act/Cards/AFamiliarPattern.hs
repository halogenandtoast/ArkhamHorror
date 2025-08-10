module Arkham.Act.Cards.AFamiliarPattern (aFamiliarPattern) where

import Arkham.Ability
import Arkham.Card
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype AFamiliarPattern = AFamiliarPattern ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aFamiliarPattern :: ActCard AFamiliarPattern
aFamiliarPattern = act (1, A) AFamiliarPattern Cards.aFamiliarPattern Nothing

instance HasAbilities AFamiliarPattern where
  getAbilities = actAbilities1 \a ->
    mkAbility a 1
      $ Objective
      $ ForcedAbilityWithCost AnyWindow (GroupClueCost (PerPlayer 2) Anywhere)

instance RunMessage AFamiliarPattern where
  runMessage msg a@(AFamiliarPattern attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      theWingedSerpent <- genCard Enemies.theWingedSerpentTheFuryOfYig
      lead <- getLead
      drawCard lead theWingedSerpent
      advanceActDeck attrs
      pure a
    _ -> AFamiliarPattern <$> liftRunMessage msg attrs

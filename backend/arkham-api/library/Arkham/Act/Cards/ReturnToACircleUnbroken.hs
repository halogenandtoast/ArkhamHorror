module Arkham.Act.Cards.ReturnToACircleUnbroken (returnToACircleUnbroken) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Trait (Trait (Witch))

newtype ReturnToACircleUnbroken = ReturnToACircleUnbroken ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToACircleUnbroken :: ActCard ReturnToACircleUnbroken
returnToACircleUnbroken = act (4, A) ReturnToACircleUnbroken Cards.returnToACircleUnbroken Nothing

instance HasAbilities ReturnToACircleUnbroken where
  getAbilities = actAbilities1 \a ->
    restricted
      a
      1
      ( EnemyCount
          (GreaterThan (PerPlayer 1))
          (EnemyWithTrait Witch <> ExhaustedEnemy <> EnemyAt "Witches' Circle")
          <> DuringTurn Anyone
      )
      $ Objective
      $ FastAbility (GroupClueCost (PerPlayer 2) "Witches' Circle")

instance RunMessage ReturnToACircleUnbroken where
  runMessage msg a@(ReturnToACircleUnbroken attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithClues attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R5
      pure a
    _ -> ReturnToACircleUnbroken <$> liftRunMessage msg attrs

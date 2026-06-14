module Arkham.Act.Cards.AscendTheWall (ascendTheWall) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Matcher

newtype AscendTheWall = AscendTheWall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ascendTheWall :: ActCard AscendTheWall
ascendTheWall = act (1, A) AscendTheWall Cards.ascendTheWall Nothing

instance HasAbilities AscendTheWall where
  getAbilities (AscendTheWall a) =
    extend
      a
      [ restricted a 1 (youExist $ at_ FloodedLocation)
          $ FastAbility (GroupClueCost (PerPlayer 1) Anywhere)
      , restricted
          a
          2
          (EachUndefeatedInvestigator (at_ $ LocationWithTitle "Western Wall"))
          $ Objective
          $ forced (RoundEnds #when)
      ]

instance RunMessage AscendTheWall where
  runMessage msg a@(AscendTheWall attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withLocationOf iid \lid -> push $ DecreaseFloodLevel lid
      pure a
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide A attrs -> True) _ _ -> do
      push R2
      pure a
    _ -> AscendTheWall <$> liftRunMessage msg attrs

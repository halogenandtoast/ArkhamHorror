module Arkham.Act.Cards.TheReturnTrip (TheReturnTrip (..), theReturnTrip) where

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher

newtype TheReturnTrip = TheReturnTrip ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theReturnTrip :: ActCard TheReturnTrip
theReturnTrip =
  act (3, A) TheReturnTrip Cards.theReturnTrip
    $ Just (GroupClueCost (PerPlayer 2) $ LocationWithTitle "Templo Mayor")

instance HasAbilities TheReturnTrip where
  getAbilities (TheReturnTrip a) =
    extend a [mkAbility a 1 $ Objective $ forced $ ifEnemyDefeated Enemies.padmaAmrita | onSide A a]

instance RunMessage TheReturnTrip where
  runMessage msg a@(TheReturnTrip attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      advancedWithOther attrs
      pure a
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      push R1
      pure a
    _ -> TheReturnTrip <$> liftRunMessage msg attrs

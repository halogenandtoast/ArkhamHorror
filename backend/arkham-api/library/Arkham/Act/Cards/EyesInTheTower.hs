module Arkham.Act.Cards.EyesInTheTower (eyesInTheTower) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype EyesInTheTower = EyesInTheTower ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

eyesInTheTower :: ActCard EyesInTheTower
eyesInTheTower = act (3, A) EyesInTheTower Cards.eyesInTheTower Nothing

instance RunMessage EyesInTheTower where
  runMessage msg a@(EyesInTheTower attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> EyesInTheTower <$> liftRunMessage msg attrs

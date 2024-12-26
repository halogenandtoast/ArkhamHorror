module Arkham.Act.Cards.AscendTheMountain (ascendTheMountain) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype AscendTheMountain = AscendTheMountain ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ascendTheMountain :: ActCard AscendTheMountain
ascendTheMountain = act (1, A) AscendTheMountain Cards.ascendTheMountain Nothing

instance RunMessage AscendTheMountain where
  runMessage msg a@(AscendTheMountain attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> AscendTheMountain <$> liftRunMessage msg attrs

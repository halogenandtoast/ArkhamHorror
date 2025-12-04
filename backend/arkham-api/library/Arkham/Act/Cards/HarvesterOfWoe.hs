module Arkham.Act.Cards.HarvesterOfWoe (harvesterOfWoe) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype HarvesterOfWoe = HarvesterOfWoe ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

harvesterOfWoe :: ActCard HarvesterOfWoe
harvesterOfWoe = act (1, A) HarvesterOfWoe Cards.harvesterOfWoe Nothing

instance RunMessage HarvesterOfWoe where
  runMessage msg a@(HarvesterOfWoe attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> HarvesterOfWoe <$> liftRunMessage msg attrs

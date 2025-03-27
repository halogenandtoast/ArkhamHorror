module Arkham.Act.Cards.SprawlingCityV1 (sprawlingCityV1) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SprawlingCityV1 = SprawlingCityV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sprawlingCityV1 :: ActCard SprawlingCityV1
sprawlingCityV1 = act (1, A) SprawlingCityV1 Cards.sprawlingCityV1 Nothing

instance RunMessage SprawlingCityV1 where
  runMessage msg a@(SprawlingCityV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SprawlingCityV1 <$> liftRunMessage msg attrs

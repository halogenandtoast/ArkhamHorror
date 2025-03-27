module Arkham.Act.Cards.SprawlingCityV3 (sprawlingCityV3) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SprawlingCityV3 = SprawlingCityV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sprawlingCityV3 :: ActCard SprawlingCityV3
sprawlingCityV3 = act (1, A) SprawlingCityV3 Cards.sprawlingCityV3 Nothing

instance RunMessage SprawlingCityV3 where
  runMessage msg a@(SprawlingCityV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SprawlingCityV3 <$> liftRunMessage msg attrs

module Arkham.Act.Cards.SprawlingCityV2 (sprawlingCityV2) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype SprawlingCityV2 = SprawlingCityV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

sprawlingCityV2 :: ActCard SprawlingCityV2
sprawlingCityV2 = act (1, A) SprawlingCityV2 Cards.sprawlingCityV2 Nothing

instance RunMessage SprawlingCityV2 where
  runMessage msg a@(SprawlingCityV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> SprawlingCityV2 <$> liftRunMessage msg attrs

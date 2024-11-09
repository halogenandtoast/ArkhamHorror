module Arkham.Act.Cards.CityOfTheDeepV2
  ( CityOfTheDeepV2(..)
  , cityOfTheDeepV2
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CityOfTheDeepV2 = CityOfTheDeepV2 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cityOfTheDeepV2 :: ActCard CityOfTheDeepV2
cityOfTheDeepV2 = act (1, A) CityOfTheDeepV2 Cards.cityOfTheDeepV2 Nothing

instance RunMessage CityOfTheDeepV2 where
  runMessage msg a@(CityOfTheDeepV2 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CityOfTheDeepV2 <$> liftRunMessage msg attrs

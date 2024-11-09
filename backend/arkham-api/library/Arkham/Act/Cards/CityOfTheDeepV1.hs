module Arkham.Act.Cards.CityOfTheDeepV1
  ( CityOfTheDeepV1(..)
  , cityOfTheDeepV1
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CityOfTheDeepV1 = CityOfTheDeepV1 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cityOfTheDeepV1 :: ActCard CityOfTheDeepV1
cityOfTheDeepV1 = act (1, A) CityOfTheDeepV1 Cards.cityOfTheDeepV1 Nothing

instance RunMessage CityOfTheDeepV1 where
  runMessage msg a@(CityOfTheDeepV1 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CityOfTheDeepV1 <$> liftRunMessage msg attrs

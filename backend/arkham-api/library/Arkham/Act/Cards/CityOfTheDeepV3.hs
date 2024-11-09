module Arkham.Act.Cards.CityOfTheDeepV3
  ( CityOfTheDeepV3(..)
  , cityOfTheDeepV3
  ) where

import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted

newtype CityOfTheDeepV3 = CityOfTheDeepV3 ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cityOfTheDeepV3 :: ActCard CityOfTheDeepV3
cityOfTheDeepV3 = act (1, A) CityOfTheDeepV3 Cards.cityOfTheDeepV3 Nothing

instance RunMessage CityOfTheDeepV3 where
  runMessage msg a@(CityOfTheDeepV3 attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      advanceActDeck attrs
      pure a
    _ -> CityOfTheDeepV3 <$> liftRunMessage msg attrs

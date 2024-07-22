module Arkham.Event.Cards.BurningTheMidnightOil (burningTheMidnightOil, BurningTheMidnightOil (..)) where

import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate

newtype BurningTheMidnightOil = BurningTheMidnightOil EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

burningTheMidnightOil :: EventCard BurningTheMidnightOil
burningTheMidnightOil = event BurningTheMidnightOil Cards.burningTheMidnightOil

instance RunMessage BurningTheMidnightOil where
  runMessage msg e@(BurningTheMidnightOil attrs) = runQueueT $ case msg of
    PlayThisEvent iid (is attrs -> True) -> do
      gainResourcesIfCan iid attrs 2
      sid <- getRandom
      pushM $ mkInvestigate sid iid attrs
      pure e
    _ -> BurningTheMidnightOil <$> liftRunMessage msg attrs

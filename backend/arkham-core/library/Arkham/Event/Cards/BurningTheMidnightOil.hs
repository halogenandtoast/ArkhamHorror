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
      pushM $ mkInvestigate iid attrs
      pure e
    _ -> BurningTheMidnightOil <$> lift (runMessage msg attrs)

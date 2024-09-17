module Arkham.Location.Cards.InnsmouthHarbour
  ( innsmouthHarbour
  , InnsmouthHarbour(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype InnsmouthHarbour = InnsmouthHarbour LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

innsmouthHarbour :: LocationCard InnsmouthHarbour
innsmouthHarbour = location InnsmouthHarbour Cards.innsmouthHarbour 3 (PerPlayer 2)

instance HasAbilities InnsmouthHarbour where
  getAbilities (InnsmouthHarbour attrs) =
    extendRevealed attrs []

instance RunMessage InnsmouthHarbour where
  runMessage msg (InnsmouthHarbour attrs) = runQueueT $ case msg of
    _ -> InnsmouthHarbour <$> liftRunMessage msg attrs

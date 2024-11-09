module Arkham.Location.Cards.SunkenHalls
  ( sunkenHalls
  , SunkenHalls(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenHalls = SunkenHalls LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenHalls :: LocationCard SunkenHalls
sunkenHalls = location SunkenHalls Cards.sunkenHalls 2 (PerPlayer 1)

instance HasAbilities SunkenHalls where
  getAbilities (SunkenHalls attrs) =
    extendRevealed attrs []

instance RunMessage SunkenHalls where
  runMessage msg (SunkenHalls attrs) = runQueueT $ case msg of
    _ -> SunkenHalls <$> liftRunMessage msg attrs

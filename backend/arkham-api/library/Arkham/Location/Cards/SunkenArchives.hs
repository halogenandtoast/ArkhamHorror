module Arkham.Location.Cards.SunkenArchives
  ( sunkenArchives
  , SunkenArchives(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenArchives = SunkenArchives LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenArchives :: LocationCard SunkenArchives
sunkenArchives = location SunkenArchives Cards.sunkenArchives 0 (Static 0)

instance HasAbilities SunkenArchives where
  getAbilities (SunkenArchives attrs) =
    extendRevealed attrs []

instance RunMessage SunkenArchives where
  runMessage msg (SunkenArchives attrs) = runQueueT $ case msg of
    _ -> SunkenArchives <$> liftRunMessage msg attrs

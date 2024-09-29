module Arkham.Location.Cards.SaltMarshes
  ( saltMarshes
  , SaltMarshes(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SaltMarshes = SaltMarshes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

saltMarshes :: LocationCard SaltMarshes
saltMarshes = location SaltMarshes Cards.saltMarshes 4 (Static 0)

instance HasAbilities SaltMarshes where
  getAbilities (SaltMarshes attrs) =
    extendRevealed attrs []

instance RunMessage SaltMarshes where
  runMessage msg (SaltMarshes attrs) = runQueueT $ case msg of
    _ -> SaltMarshes <$> liftRunMessage msg attrs

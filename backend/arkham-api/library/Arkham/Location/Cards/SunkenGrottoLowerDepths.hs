module Arkham.Location.Cards.SunkenGrottoLowerDepths
  ( sunkenGrottoLowerDepths
  , SunkenGrottoLowerDepths(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenGrottoLowerDepths = SunkenGrottoLowerDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoLowerDepths :: LocationCard SunkenGrottoLowerDepths
sunkenGrottoLowerDepths = location SunkenGrottoLowerDepths Cards.sunkenGrottoLowerDepths 0 (Static 0)

instance HasAbilities SunkenGrottoLowerDepths where
  getAbilities (SunkenGrottoLowerDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoLowerDepths where
  runMessage msg (SunkenGrottoLowerDepths attrs) = runQueueT $ case msg of
    _ -> SunkenGrottoLowerDepths <$> liftRunMessage msg attrs

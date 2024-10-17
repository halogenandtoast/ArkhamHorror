module Arkham.Location.Cards.SunkenGrottoUpperDepths
  ( sunkenGrottoUpperDepths
  , SunkenGrottoUpperDepths(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenGrottoUpperDepths = SunkenGrottoUpperDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoUpperDepths :: LocationCard SunkenGrottoUpperDepths
sunkenGrottoUpperDepths = location SunkenGrottoUpperDepths Cards.sunkenGrottoUpperDepths 0 (Static 0)

instance HasAbilities SunkenGrottoUpperDepths where
  getAbilities (SunkenGrottoUpperDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoUpperDepths where
  runMessage msg (SunkenGrottoUpperDepths attrs) = runQueueT $ case msg of
    _ -> SunkenGrottoUpperDepths <$> liftRunMessage msg attrs

module Arkham.Location.Cards.SunkenGrottoFinalDepths
  ( sunkenGrottoFinalDepths
  , SunkenGrottoFinalDepths(..)
  )
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted

newtype SunkenGrottoFinalDepths = SunkenGrottoFinalDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoFinalDepths :: LocationCard SunkenGrottoFinalDepths
sunkenGrottoFinalDepths = location SunkenGrottoFinalDepths Cards.sunkenGrottoFinalDepths 0 (Static 0)

instance HasAbilities SunkenGrottoFinalDepths where
  getAbilities (SunkenGrottoFinalDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoFinalDepths where
  runMessage msg (SunkenGrottoFinalDepths attrs) = runQueueT $ case msg of
    _ -> SunkenGrottoFinalDepths <$> liftRunMessage msg attrs

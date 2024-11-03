module Arkham.Location.Cards.SunkenGrottoFinalDepths (
  sunkenGrottoFinalDepths,
  SunkenGrottoFinalDepths (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype SunkenGrottoFinalDepths = SunkenGrottoFinalDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoFinalDepths :: LocationCard SunkenGrottoFinalDepths
sunkenGrottoFinalDepths = location SunkenGrottoFinalDepths Cards.sunkenGrottoFinalDepths 0 (Static 0)

instance HasAbilities SunkenGrottoFinalDepths where
  getAbilities (SunkenGrottoFinalDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoFinalDepths where
  runMessage msg l@(SunkenGrottoFinalDepths attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> SunkenGrottoFinalDepths <$> liftRunMessage msg attrs

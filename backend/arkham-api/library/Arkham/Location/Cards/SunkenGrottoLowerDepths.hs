module Arkham.Location.Cards.SunkenGrottoLowerDepths (
  sunkenGrottoLowerDepths,
  SunkenGrottoLowerDepths (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype SunkenGrottoLowerDepths = SunkenGrottoLowerDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoLowerDepths :: LocationCard SunkenGrottoLowerDepths
sunkenGrottoLowerDepths = location SunkenGrottoLowerDepths Cards.sunkenGrottoLowerDepths 0 (Static 0)

instance HasAbilities SunkenGrottoLowerDepths where
  getAbilities (SunkenGrottoLowerDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoLowerDepths where
  runMessage msg l@(SunkenGrottoLowerDepths attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> SunkenGrottoLowerDepths <$> liftRunMessage msg attrs

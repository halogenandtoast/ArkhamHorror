module Arkham.Location.Cards.SunkenGrottoUpperDepths (
  sunkenGrottoUpperDepths,
  SunkenGrottoUpperDepths (..),
)
where

import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Grid
import Arkham.Location.Import.Lifted
import Arkham.Scenarios.ALightInTheFog.Helpers.Location

newtype SunkenGrottoUpperDepths = SunkenGrottoUpperDepths LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sunkenGrottoUpperDepths :: LocationCard SunkenGrottoUpperDepths
sunkenGrottoUpperDepths = location SunkenGrottoUpperDepths Cards.sunkenGrottoUpperDepths 0 (Static 0)

instance HasAbilities SunkenGrottoUpperDepths where
  getAbilities (SunkenGrottoUpperDepths attrs) =
    extendRevealed attrs []

instance RunMessage SunkenGrottoUpperDepths where
  runMessage msg l@(SunkenGrottoUpperDepths attrs) = runQueueT $ case msg of
    PlaceGrid (GridLocation pos lid) | lid == attrs.id -> setConnectedInRow pos l
    _ -> SunkenGrottoUpperDepths <$> liftRunMessage msg attrs

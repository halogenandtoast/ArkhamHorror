module Arkham.Location.Cards.FlightIntoOblivion
  ( flightIntoOblivion
  , FlightIntoOblivion(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype FlightIntoOblivion = FlightIntoOblivion LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flightIntoOblivion :: LocationCard FlightIntoOblivion
flightIntoOblivion = location FlightIntoOblivion Cards.flightIntoOblivion 2 (Static 1)

instance HasAbilities FlightIntoOblivion where
  getAbilities (FlightIntoOblivion attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage FlightIntoOblivion where
  runMessage msg (FlightIntoOblivion attrs) =
    FlightIntoOblivion <$> runMessage msg attrs

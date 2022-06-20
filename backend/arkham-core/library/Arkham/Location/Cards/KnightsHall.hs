module Arkham.Location.Cards.KnightsHall
  ( knightsHall
  , KnightsHall(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype KnightsHall = KnightsHall LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

knightsHall :: LocationCard KnightsHall
knightsHall = location KnightsHall Cards.knightsHall 2 (PerPlayer 1) Hourglass [Square, Heart]

instance HasAbilities KnightsHall where
  getAbilities (KnightsHall attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage KnightsHall where
  runMessage msg (KnightsHall attrs) =
    KnightsHall <$> runMessage msg attrs

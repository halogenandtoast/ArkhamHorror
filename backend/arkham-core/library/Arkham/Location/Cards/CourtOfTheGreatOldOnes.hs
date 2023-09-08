module Arkham.Location.Cards.CourtOfTheGreatOldOnes (
  courtOfTheGreatOldOnes,
  CourtOfTheGreatOldOnes (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  locationWith
    CourtOfTheGreatOldOnes
    Cards.courtOfTheGreatOldOnes
    4
    (Static 6)
    (connectsToL .~ adjacentLocations)

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes attrs) =
    getAbilities attrs

-- withRevealedAbilities attrs []

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg (CourtOfTheGreatOldOnes attrs) =
    CourtOfTheGreatOldOnes <$> runMessage msg attrs

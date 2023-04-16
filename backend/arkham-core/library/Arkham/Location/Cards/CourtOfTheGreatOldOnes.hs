module Arkham.Location.Cards.CourtOfTheGreatOldOnes
  ( courtOfTheGreatOldOnes
  , CourtOfTheGreatOldOnes(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype CourtOfTheGreatOldOnes = CourtOfTheGreatOldOnes LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courtOfTheGreatOldOnes :: LocationCard CourtOfTheGreatOldOnes
courtOfTheGreatOldOnes =
  location CourtOfTheGreatOldOnes Cards.courtOfTheGreatOldOnes 3 (PerPlayer 2)

instance HasAbilities CourtOfTheGreatOldOnes where
  getAbilities (CourtOfTheGreatOldOnes attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage CourtOfTheGreatOldOnes where
  runMessage msg (CourtOfTheGreatOldOnes attrs) =
    CourtOfTheGreatOldOnes <$> runMessage msg attrs

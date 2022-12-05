module Arkham.Location.Cards.Atlantis
  ( atlantis
  , Atlantis(..)
  ) where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Atlantis = Atlantis LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

atlantis :: LocationCard Atlantis
atlantis = location Atlantis Cards.atlantis 3 (Static 2)

instance HasAbilities Atlantis where
  getAbilities (Atlantis attrs) = getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage Atlantis where
  runMessage msg (Atlantis attrs) = Atlantis <$> runMessage msg attrs

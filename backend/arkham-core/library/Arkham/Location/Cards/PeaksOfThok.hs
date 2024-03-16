module Arkham.Location.Cards.PeaksOfThok
  ( peaksOfThok
  , PeaksOfThok(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype PeaksOfThok = PeaksOfThok LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peaksOfThok :: LocationCard PeaksOfThok
peaksOfThok = location PeaksOfThok Cards.peaksOfThok 3 (Static 0)

instance HasAbilities PeaksOfThok where
  getAbilities (PeaksOfThok attrs) =
    extendRevealed attrs []

instance RunMessage PeaksOfThok where
  runMessage msg (PeaksOfThok attrs) =
    PeaksOfThok <$> runMessage msg attrs

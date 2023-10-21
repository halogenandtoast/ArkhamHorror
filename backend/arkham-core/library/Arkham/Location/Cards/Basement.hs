module Arkham.Location.Cards.Basement
  ( basement
  , Basement(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Basement = Basement LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

basement :: LocationCard Basement
basement = location Basement Cards.basement 4 (PerPlayer 1)

instance HasAbilities Basement where
  getAbilities (Basement attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Basement where
  runMessage msg (Basement attrs) =
    Basement <$> runMessage msg attrs

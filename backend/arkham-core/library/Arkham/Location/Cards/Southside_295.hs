module Arkham.Location.Cards.Southside_295
  ( southside_295
  , Southside_295(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Southside_295 = Southside_295 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_295 :: LocationCard Southside_295
southside_295 = location Southside_295 Cards.southside_295 2 (Static 0)

instance HasAbilities Southside_295 where
  getAbilities (Southside_295 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Southside_295 where
  runMessage msg (Southside_295 attrs) =
    Southside_295 <$> runMessage msg attrs

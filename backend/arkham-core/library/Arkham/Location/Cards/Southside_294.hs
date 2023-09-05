module Arkham.Location.Cards.Southside_294
  ( southside_294
  , Southside_294(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype Southside_294 = Southside_294 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southside_294 :: LocationCard Southside_294
southside_294 = location Southside_294 Cards.southside_294 1 (Static 0)

instance HasAbilities Southside_294 where
  getAbilities (Southside_294 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage Southside_294 where
  runMessage msg (Southside_294 attrs) =
    Southside_294 <$> runMessage msg attrs

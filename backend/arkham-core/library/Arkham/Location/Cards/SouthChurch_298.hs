module Arkham.Location.Cards.SouthChurch_298
  ( southChurch_298
  , SouthChurch_298(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SouthChurch_298 = SouthChurch_298 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_298 :: LocationCard SouthChurch_298
southChurch_298 = location SouthChurch_298 Cards.southChurch_298 1 (Static 0)

instance HasAbilities SouthChurch_298 where
  getAbilities (SouthChurch_298 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SouthChurch_298 where
  runMessage msg (SouthChurch_298 attrs) =
    SouthChurch_298 <$> runMessage msg attrs

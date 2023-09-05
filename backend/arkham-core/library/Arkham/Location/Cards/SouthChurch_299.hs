module Arkham.Location.Cards.SouthChurch_299
  ( southChurch_299
  , SouthChurch_299(..)
  )
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner

newtype SouthChurch_299 = SouthChurch_299 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

southChurch_299 :: LocationCard SouthChurch_299
southChurch_299 = location SouthChurch_299 Cards.southChurch_299 2 (Static 0)

instance HasAbilities SouthChurch_299 where
  getAbilities (SouthChurch_299 attrs) =
    getAbilities attrs
    -- withRevealedAbilities attrs []

instance RunMessage SouthChurch_299 where
  runMessage msg (SouthChurch_299 attrs) =
    SouthChurch_299 <$> runMessage msg attrs

module Arkham.Location.Cards.UndergroundRuins
  ( undergroundRuins
  , UndergroundRuins(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.GameValue
import Arkham.Location.Runner

newtype UndergroundRuins = UndergroundRuins LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

undergroundRuins :: LocationCard UndergroundRuins
undergroundRuins = location UndergroundRuins Cards.undergroundRuins 2 (PerPlayer 1)

instance HasAbilities UndergroundRuins where
  getAbilities (UndergroundRuins attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage UndergroundRuins where
  runMessage msg (UndergroundRuins attrs) =
    UndergroundRuins <$> runMessage msg attrs

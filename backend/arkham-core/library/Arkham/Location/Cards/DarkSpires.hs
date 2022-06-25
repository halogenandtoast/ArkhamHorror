module Arkham.Location.Cards.DarkSpires
  ( darkSpires
  , DarkSpires(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Runner

newtype DarkSpires = DarkSpires LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkSpires :: LocationCard DarkSpires
darkSpires = location DarkSpires Cards.darkSpires 3 (PerPlayer 2) Moon [Moon, Equals]

instance HasAbilities DarkSpires where
  getAbilities (DarkSpires attrs) =
    getAbilities attrs
    -- withBaseAbilities attrs []

instance RunMessage DarkSpires where
  runMessage msg (DarkSpires attrs) =
    DarkSpires <$> runMessage msg attrs

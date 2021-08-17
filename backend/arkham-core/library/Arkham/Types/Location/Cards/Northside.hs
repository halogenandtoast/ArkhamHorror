module Arkham.Types.Location.Cards.Northside
  ( Northside(..)
  , northside
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (northside)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside =
  location Northside Cards.northside 3 (PerPlayer 2) T [Diamond, Triangle]

ability :: LocationAttrs -> Ability
ability attrs = base { abilityLimit = GroupLimit PerGame 1 }
 where
  base = mkAbility
    (toSource attrs)
    1
    (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 5])

instance ActionRunner env => HasAbilities env Northside where
  getAbilities iid window@(Window Timing.When NonFast) (Northside attrs@LocationAttrs {..})
    | locationRevealed
    = withBaseActions iid window attrs $ pure [locationAbility (ability attrs)]
  getAbilities iid window (Northside attrs) = getAbilities iid window attrs

instance (LocationRunner env) => RunMessage env Northside where
  runMessage msg l@(Northside attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (GainClues iid 2)
    _ -> Northside <$> runMessage msg attrs

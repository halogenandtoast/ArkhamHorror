module Arkham.Types.Location.Cards.Northside
  ( Northside(..)
  , northside
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (northside)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside =
  location Northside Cards.northside 3 (PerPlayer 2) T [Diamond, Triangle]

instance HasAbilities Northside where
  getAbilities (Northside x) | locationRevealed x =
    withBaseAbilities x $
      [ restrictedAbility
            x
            1
            Here
            (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 5])
          & (abilityLimitL .~ GroupLimit PerGame 1)
      ]
  getAbilities (Northside attrs) = getAbilities attrs

instance LocationRunner env => RunMessage env Northside where
  runMessage msg l@(Northside attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (GainClues iid 2)
    _ -> Northside <$> runMessage msg attrs

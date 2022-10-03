module Arkham.Location.Cards.Northside
  ( Northside(..)
  , northside
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( northside )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside = location Northside Cards.northside 3 (PerPlayer 2)

instance HasAbilities Northside where
  getAbilities (Northside x) | locationRevealed x =
    withBaseAbilities x
      $ [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility x 1 Here
          $ ActionAbility Nothing
          $ Costs [ActionCost 1, ResourceCost 5]
        ]
  getAbilities (Northside attrs) = getAbilities attrs

instance RunMessage Northside where
  runMessage msg l@(Northside attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source ->
      l <$ push (GainClues iid 2)
    _ -> Northside <$> runMessage msg attrs

module Arkham.Location.Cards.Northside (
  Northside (..),
  northside,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (northside)
import Arkham.Location.Runner

newtype Northside = Northside LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

northside :: LocationCard Northside
northside = location Northside Cards.northside 3 (PerPlayer 2)

instance HasAbilities Northside where
  getAbilities (Northside x) =
    withRevealedAbilities x
      $ [ limitedAbility (GroupLimit PerGame 1)
            $ restrictedAbility x 1 Here
            $ ActionAbility Nothing
            $ Costs [ActionCost 1, ResourceCost 5]
        ]

instance RunMessage Northside where
  runMessage msg l@(Northside attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ GainClues iid (toAbilitySource attrs 1) 2
      pure l
    _ -> Northside <$> runMessage msg attrs

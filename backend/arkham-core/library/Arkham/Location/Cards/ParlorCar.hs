module Arkham.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Location.Cards qualified as Cards (parlorCar)
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Id
import Arkham.Location.Runner
import Arkham.Location.Helpers
import Arkham.Message
import Arkham.Modifier
import Arkham.Query

newtype ParlorCar = ParlorCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: LocationCard ParlorCar
parlorCar = locationWith
  ParlorCar
  Cards.parlorCar
  3
  (PerPlayer 1)
  NoSymbol
  []
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasCount ClueCount env LocationId => HasModifiersFor ParlorCar where
  getModifiersFor _ target (ParlorCar l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          l
          (CannotInvestigate
          : [ Blocked | not locationRevealed && clueCount > 0 ]
          )
      Nothing -> pure $ toModifiers l [CannotInvestigate]
  getModifiersFor _ _ _ = pure []

instance HasAbilities ParlorCar where
  getAbilities (ParlorCar attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility attrs 1 Here $ ActionAbility Nothing $ Costs
          [ActionCost 1, ResourceCost 3]
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage ParlorCar where
  runMessage msg l@(ParlorCar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DiscoverCluesAtLocation iid locationId 1 Nothing)
    _ -> ParlorCar <$> runMessage msg attrs

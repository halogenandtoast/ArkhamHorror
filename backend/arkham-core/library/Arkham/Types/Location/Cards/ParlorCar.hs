module Arkham.Types.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (parlorCar)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query

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

instance HasCount ClueCount env LocationId => HasModifiersFor env ParlorCar where
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
      [ mkAbility attrs 1 $ ActionAbility Nothing $ Costs
          [ActionCost 1, ResourceCost 3]
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env ParlorCar where
  runMessage msg l@(ParlorCar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (DiscoverCluesAtLocation iid locationId 1 Nothing)
    _ -> ParlorCar <$> runMessage msg attrs

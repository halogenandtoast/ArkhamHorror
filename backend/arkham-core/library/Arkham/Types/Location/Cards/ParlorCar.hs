module Arkham.Types.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (parlorCar)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.Window

newtype ParlorCar = ParlorCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: LocationId -> ParlorCar
parlorCar =
  ParlorCar
    . (connectsToL .~ setFromList [LeftOf, RightOf])
    . baseAttrs
        Cards.parlorCar
        3
        (PerPlayer 1)
        NoSymbol
        []

instance HasCount ClueCount env LocationId => HasModifiersFor env ParlorCar where
  getModifiersFor _ target (ParlorCar location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          (CannotInvestigate
          : [ Blocked | not locationRevealed && clueCount > 0 ]
          )
      Nothing -> pure $ toModifiers location [CannotInvestigate]
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs = mkAbility
  (toSource attrs)
  1
  (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 3])

instance ActionRunner env => HasActions env ParlorCar where
  getActions iid NonFast (ParlorCar attrs) | locationRevealed attrs =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (ParlorCar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env ParlorCar where
  runMessage msg l@(ParlorCar attrs@LocationAttrs {..}) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessage (DiscoverCluesAtLocation iid locationId 1 Nothing)
    _ -> ParlorCar <$> runMessage msg attrs

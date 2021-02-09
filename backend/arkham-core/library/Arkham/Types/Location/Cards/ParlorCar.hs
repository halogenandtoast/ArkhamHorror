module Arkham.Types.Location.Cards.ParlorCar
  ( parlorCar
  , ParlorCar(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.Trait

newtype ParlorCar = ParlorCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

parlorCar :: ParlorCar
parlorCar = ParlorCar $ base
  { locationConnectsTo = setFromList [LeftOf, RightOf]
  , locationVictory = Just 1
  }
 where
  base = baseAttrs
    "02174"
    (Name "Parlor Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    3
    (PerPlayer 1)
    NoSymbol
    []
    (singleton Train)

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

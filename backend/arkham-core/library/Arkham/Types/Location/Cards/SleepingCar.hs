module Arkham.Types.Location.Cards.SleepingCar
  ( sleepingCar
  , SleepingCar(..)
  )
where


import qualified Arkham.Types.EncounterSet as EncounterSet
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Trait

newtype SleepingCar = SleepingCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepingCar :: SleepingCar
sleepingCar = SleepingCar
  $ base { locationConnectsTo = setFromList [LeftOf, RightOf] }
 where
  base = baseAttrs
    "02172"
    (Name "Sleeping Car" Nothing)
    EncounterSet.TheEssexCountyExpress
    4
    (Static 1)
    NoSymbol
    []
    (singleton Train)

instance HasCount ClueCount env LocationId => HasModifiersFor env SleepingCar where
  getModifiersFor _ target (SleepingCar location@LocationAttrs {..})
    | isTarget location target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers
          location
          [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = GroupLimit PerGame 1
    }

instance ActionRunner env => HasActions env SleepingCar where
  getActions iid NonFast (SleepingCar attrs) | locationRevealed attrs =
    withBaseActions iid NonFast attrs
      $ pure [ ActivateCardAbilityAction iid (ability attrs) | iid `on` attrs ]
  getActions iid window (SleepingCar attrs) = getActions iid window attrs

instance LocationRunner env => RunMessage env SleepingCar where
  runMessage msg l@(SleepingCar attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ unshiftMessages
        [TakeResources iid 3 False, Remember StolenAPassengersLuggage]
    _ -> SleepingCar <$> runMessage msg attrs

module Arkham.Types.Location.Cards.SleepingCar
  ( sleepingCar
  , SleepingCar(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (sleepingCar)
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
import Arkham.Types.ScenarioLogKey
import Arkham.Types.Window

newtype SleepingCar = SleepingCar LocationAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepingCar :: LocationId -> SleepingCar
sleepingCar =
  SleepingCar . (connectsToL .~ setFromList [LeftOf, RightOf]) . baseAttrs
    Cards.sleepingCar
    4
    (Static 1)
    NoSymbol
    []

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

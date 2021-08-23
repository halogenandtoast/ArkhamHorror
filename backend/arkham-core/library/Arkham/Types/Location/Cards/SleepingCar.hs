module Arkham.Types.Location.Cards.SleepingCar
  ( sleepingCar
  , SleepingCar(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Location.Cards as Cards (sleepingCar)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Location.Runner
import Arkham.Types.LocationSymbol
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.Query
import Arkham.Types.ScenarioLogKey
import qualified Arkham.Types.Timing as Timing
import Arkham.Types.Window

newtype SleepingCar = SleepingCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepingCar :: LocationCard SleepingCar
sleepingCar = locationWith
  SleepingCar
  Cards.sleepingCar
  4
  (Static 1)
  NoSymbol
  []
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasCount ClueCount env LocationId => HasModifiersFor env SleepingCar where
  getModifiersFor _ target (SleepingCar l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- unClueCount <$> getCount leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ _ = pure []

ability :: LocationAttrs -> Ability
ability attrs =
  (mkAbility (toSource attrs) 1 (ActionAbility Nothing $ ActionCost 1))
    { abilityLimit = GroupLimit PerGame 1
    }

instance HasAbilities env SleepingCar where
  getAbilities iid window@(Window Timing.When NonFast) (SleepingCar attrs)
    | locationRevealed attrs = withBaseAbilities iid window attrs
    $ pure [locationAbility (ability attrs)]
  getAbilities iid window (SleepingCar attrs) = getAbilities iid window attrs

instance LocationRunner env => RunMessage env SleepingCar where
  runMessage msg l@(SleepingCar attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll
        [TakeResources iid 3 False, Remember StolenAPassengersLuggage]
    _ -> SleepingCar <$> runMessage msg attrs

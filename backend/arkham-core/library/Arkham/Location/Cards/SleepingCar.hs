module Arkham.Location.Cards.SleepingCar
  ( sleepingCar
  , SleepingCar(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( sleepingCar )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Message
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype SleepingCar = SleepingCar LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleepingCar :: LocationCard SleepingCar
sleepingCar = locationWith
  SleepingCar
  Cards.sleepingCar
  4
  (Static 1)
  (connectsToL .~ setFromList [LeftOf, RightOf])

instance HasModifiersFor SleepingCar where
  getModifiersFor target (SleepingCar l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities SleepingCar where
  getAbilities (SleepingCar attrs) =
    withBaseAbilities attrs
      $ [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility attrs 1 Here
          $ ActionAbility Nothing
          $ ActionCost 1
        | locationRevealed attrs
        ]

instance RunMessage SleepingCar where
  runMessage msg l@(SleepingCar attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ pushAll
        [TakeResources iid 3 False, Remember StolenAPassengersLuggage]
    _ -> SleepingCar <$> runMessage msg attrs

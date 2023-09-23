module Arkham.Location.Cards.EngineCar_177 (
  engineCar_177,
  EngineCar_177 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (engineCar_177)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype EngineCar_177 = EngineCar_177 LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_177 :: LocationCard EngineCar_177
engineCar_177 =
  locationWith
    EngineCar_177
    Cards.engineCar_177
    1
    (PerPlayer 2)
    (connectsToL .~ singleton LeftOf)

instance HasModifiersFor EngineCar_177 where
  getModifiersFor target (EngineCar_177 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
        Just leftLocation -> do
          clueCount <- field LocationClues leftLocation
          pure $ toModifiers l [Blocked | not locationRevealed && clueCount > 0]
        Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities EngineCar_177 where
  getAbilities (EngineCar_177 x) =
    withBaseAbilities x
      $ [ restrictedAbility x 1 Here
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId x
        | locationRevealed x
        ]

instance RunMessage EngineCar_177 where
  runMessage msg l@(EngineCar_177 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ pushAll (replicate 3 $ InvestigatorDrawEncounterCard iid)
    _ -> EngineCar_177 <$> runMessage msg attrs

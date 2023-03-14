module Arkham.Location.Cards.EngineCar_176
  ( engineCar_176
  , EngineCar_176(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards ( engineCar_176 )
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype EngineCar_176 = EngineCar_176 LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

engineCar_176 :: LocationCard EngineCar_176
engineCar_176 = locationWith
  EngineCar_176
  Cards.engineCar_176
  2
  (PerPlayer 2)
  (connectsToL .~ singleton LeftOf)

instance HasModifiersFor EngineCar_176 where
  getModifiersFor target (EngineCar_176 l@LocationAttrs {..})
    | isTarget l target = case lookup LeftOf locationDirections of
      Just leftLocation -> do
        clueCount <- field LocationClues leftLocation
        pure $ toModifiers l [ Blocked | not locationRevealed && clueCount > 0 ]
      Nothing -> pure []
  getModifiersFor _ _ = pure []

instance HasAbilities EngineCar_176 where
  getAbilities (EngineCar_176 x) =
    withBaseAbilities x
      $ [ restrictedAbility x 1 Here
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId x
        | locationRevealed x
        ]

instance RunMessage EngineCar_176 where
  runMessage msg l@(EngineCar_176 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      l <$ push
        (FindAndDrawEncounterCard iid (CardWithTitle "Grappling Horror") True)
    _ -> EngineCar_176 <$> runMessage msg attrs

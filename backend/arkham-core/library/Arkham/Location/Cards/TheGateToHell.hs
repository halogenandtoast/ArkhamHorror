module Arkham.Location.Cards.TheGateToHell
  ( theGateToHell
  , TheGateToHell(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Direction
import Arkham.GameValue
import Arkham.Label (mkLabel, unLabel)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Message hiding ( RevealLocation )
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Timing qualified as Timing

newtype TheGateToHell = TheGateToHell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateToHell :: LocationCard TheGateToHell
theGateToHell =
  location TheGateToHell Cards.theGateToHell 0 (Static 0) NoSymbol []

instance HasAbilities TheGateToHell where
  getAbilities (TheGateToHell attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ mkAbility attrs 1
          $ ForcedAbility
          $ RevealLocation Timing.When Anyone
          $ LocationWithId
          $ toId attrs
        ]
      else []

instance LocationRunner env => RunMessage env TheGateToHell where
  runMessage msg l@(TheGateToHell attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) 2)
      pure l
    DrewFromScenarioDeck _ _ target [above, below] | isTarget attrs target -> do
      let
        (x, y) = posLabelToPosition (mkLabel $ locationLabel attrs)
        abovePosition = (x, y + 1)
        belowPosition = (x, y - 1)
      pushAll
        [ PlaceLocation above
        , SetLocationLabel (toLocationId above) (unLabel $ positionToLabel abovePosition)
        , PlacedLocationDirection (toLocationId above) Above (toId attrs)
        , PlaceLocation below
        , SetLocationLabel (toLocationId below) (unLabel $ positionToLabel belowPosition)
        , PlacedLocationDirection (toLocationId below) Below (toId attrs)
        ]
      pure l
    _ -> TheGateToHell <$> runMessage msg attrs

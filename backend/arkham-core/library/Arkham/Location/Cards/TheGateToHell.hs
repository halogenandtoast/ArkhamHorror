module Arkham.Location.Cards.TheGateToHell
  ( theGateToHell
  , TheGateToHell(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Criteria
import Arkham.Direction
import Arkham.GameValue
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
theGateToHell = locationWith
  TheGateToHell
  Cards.theGateToHell
  0
  (Static 0)
  NoSymbol
  []
  (connectsToL .~ adjacentLocations)

instance HasAbilities TheGateToHell where
  getAbilities (TheGateToHell attrs) =
    withBaseAbilities attrs $ if locationRevealed attrs
      then
        [ restrictedAbility
            attrs
            1
            (AnyCriterion
              [ Negate
                  (LocationExists
                  $ LocationInDirection dir (LocationWithId $ toId attrs)
                  )
              | dir <- [Above, Below]
              ]
            )
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
    DrewFromScenarioDeck iid _ target cards | isTarget attrs target -> do
      let
        placeAbove = placeAtDirection Above attrs
        placeBelow = placeAtDirection Below attrs
      case cards of
        [above, below] -> pushAll $ placeAbove above <> placeBelow below
        [aboveOrBelow] -> push $ chooseOne
          iid
          [ Label "Place above" (placeAbove aboveOrBelow)
          , Label "Place below" (placeBelow aboveOrBelow)
          ]
        [] -> pure ()
        _ -> error "wrong number of cards drawn"
      pure l
    _ -> TheGateToHell <$> runMessage msg attrs

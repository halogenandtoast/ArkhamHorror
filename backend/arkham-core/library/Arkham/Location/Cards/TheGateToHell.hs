module Arkham.Location.Cards.TheGateToHell
  ( theGateToHell
  , TheGateToHell(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Cost
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
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateToHell :: LocationCard TheGateToHell
theGateToHell = locationWith
  TheGateToHell
  Cards.theGateToHell
  1
  (PerPlayer 2)
  ((connectsToL .~ adjacentLocations)
  . (costToEnterUnrevealedL
    .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation]
    )
  )

instance HasAbilities TheGateToHell where
  getAbilities (TheGateToHell attrs) = withBaseAbilities
    attrs
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
    | locationRevealed attrs
    ]

instance RunMessage TheGateToHell where
  runMessage msg l@(TheGateToHell attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) _ 1 _ -> do
      n <- countM (directionEmpty attrs) [Above, Below]
      push (DrawFromScenarioDeck iid CatacombsDeck (toTarget attrs) n)
      pure l
    DrewFromScenarioDeck _ _ (isTarget attrs -> True) cards -> do
      placements <- mapMaybeM (toMaybePlacement attrs) [Above, Below]
      pushAll $ concat $ zipWith ($) placements cards
      pure l
    _ -> TheGateToHell <$> runMessage msg attrs

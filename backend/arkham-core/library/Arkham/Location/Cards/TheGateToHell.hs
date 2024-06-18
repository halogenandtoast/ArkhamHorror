module Arkham.Location.Cards.TheGateToHell (theGateToHell, TheGateToHell (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype TheGateToHell = TheGateToHell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateToHell :: LocationCard TheGateToHell
theGateToHell =
  locationWith TheGateToHell Cards.theGateToHell 1 (PerPlayer 2)
    $ (connectsToL .~ adjacentLocations)
    . (costToEnterUnrevealedL .~ Costs [ActionCost 1, GroupClueCost (PerPlayer 1) YourLocation])

instance HasAbilities TheGateToHell where
  getAbilities (TheGateToHell attrs) =
    extendRevealed
      attrs
      [ restrictedAbility
          attrs
          1
          (oneOf [notExists $ LocationInDirection dir (be attrs) | dir <- [Above, Below]])
          $ forced
          $ RevealLocation #when Anyone (be attrs)
      ]

instance RunMessage TheGateToHell where
  runMessage msg l@(TheGateToHell attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, Below]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, Below]
      pure l
    _ -> TheGateToHell <$> runMessage msg attrs

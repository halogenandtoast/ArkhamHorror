module Arkham.Location.Cards.TheGateToHell (theGateToHell) where

import Arkham.Ability
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Helpers
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype TheGateToHell = TheGateToHell LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theGateToHell :: LocationCard TheGateToHell
theGateToHell =
  locationWith TheGateToHell Cards.theGateToHell 1 (PerPlayer 2)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities TheGateToHell where
  getAbilities (TheGateToHell a) =
    extendRevealed1 a
      $ restricted a 1 (oneOf [notExists $ LocationInDirection dir (be a) | dir <- [Above, Below]])
      $ forced
      $ RevealLocation #when Anyone (be a)

instance RunMessage TheGateToHell where
  runMessage msg l@(TheGateToHell attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      n <- countM (directionEmpty attrs) [Above, Below]
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck n
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [Above, Below]
      pure l
    _ -> TheGateToHell <$> liftRunMessage msg attrs

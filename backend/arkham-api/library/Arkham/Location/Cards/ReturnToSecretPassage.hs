module Arkham.Location.Cards.ReturnToSecretPassage (returnToSecretPassage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenario.Deck
import Arkham.Scenarios.ThePallidMask.Helpers

newtype ReturnToSecretPassage = ReturnToSecretPassage LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

returnToSecretPassage :: LocationCard ReturnToSecretPassage
returnToSecretPassage =
  locationWith ReturnToSecretPassage Cards.returnToSecretPassage 5 (PerPlayer 1)
    $ connectsToAdjacent
    . (costToEnterUnrevealedL .~ GroupClueCost (PerPlayer 1) YourLocation)

instance HasAbilities ReturnToSecretPassage where
  getAbilities (ReturnToSecretPassage a) =
    extendRevealed
      a
      [ restricted
          a
          1
          ( youExist (ControlsAsset $ assetIs Assets.claspOfBlackOnyx)
              <> exists (LocationInDirection RightOf (be a) <> UnrevealedLocation)
          )
          actionAbility
      , restricted a 2 (notExists $ LocationInDirection RightOf (be a))
          $ forced
          $ RevealLocation #when Anyone (be a)
      ]

instance RunMessage ReturnToSecretPassage where
  runMessage msg l@(ReturnToSecretPassage attrs) = runQueueT $ case msg of
    UseThisAbility _iid (isSource attrs -> True) 1 -> do
      selectEach (LocationInDirection RightOf (be attrs)) reveal
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      push $ DrawCards iid $ targetCardDraw attrs CatacombsDeck 1
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placeDrawnLocations attrs drewCards.cards [RightOf]
      pure l
    _ -> ReturnToSecretPassage <$> liftRunMessage msg attrs

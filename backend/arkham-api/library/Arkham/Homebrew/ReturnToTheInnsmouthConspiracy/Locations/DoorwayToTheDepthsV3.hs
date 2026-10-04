module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.DoorwayToTheDepthsV3 (
  doorwayToTheDepthsV3,
) where

import Arkham.Ability
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Key
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.ScenarioLogKey

newtype DoorwayToTheDepthsV3 = DoorwayToTheDepthsV3 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheDepthsV3 :: LocationCard DoorwayToTheDepthsV3
doorwayToTheDepthsV3 =
  location DoorwayToTheDepthsV3 Cards.doorwayToTheDepthsV3 5 (PerPlayer 1)

instance HasAbilities DoorwayToTheDepthsV3 where
  getAbilities (DoorwayToTheDepthsV3 a) =
    extendRevealed
      a
      [ mkAbility a 1 $ forced $ RevealLocation #after Anyone (be a)
      , groupLimit PerGame
          $ restricted a 2 Here
          $ ActionAbility mempty Nothing
          $ ActionCost 1
          <> SpendKeyCost GreenKey
          <> GroupClueCost (PerPlayer 3) Anywhere
      ]

-- | "Secret Passage": the green key is simply here for the taking.
instance RunMessage DoorwayToTheDepthsV3 where
  runMessage msg l@(DoorwayToTheDepthsV3 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      placeKey attrs GreenKey
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember UnlockedTheFinalDepths
      pure l
    _ -> DoorwayToTheDepthsV3 <$> liftRunMessage msg attrs

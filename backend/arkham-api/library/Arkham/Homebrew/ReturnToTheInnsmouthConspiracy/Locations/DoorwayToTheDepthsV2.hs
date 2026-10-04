module Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.Locations.DoorwayToTheDepthsV2 (
  doorwayToTheDepthsV2,
) where

import Arkham.Ability
import Arkham.Enemy.CardDefs.TheInnsmouthConspiracy.TheLairOfDagon qualified as Enemies
import Arkham.Helpers.Query (getSetAsideCardsMatching)
import Arkham.Homebrew.ReturnToTheInnsmouthConspiracy.CardDefs.Locations qualified as Cards
import Arkham.Key
import Arkham.Location.Import.Lifted
import Arkham.Location.Types (Field (LocationShroud))
import Arkham.Matcher
import Arkham.Message.Lifted.Log
import Arkham.Projection
import Arkham.ScenarioLogKey

newtype DoorwayToTheDepthsV2 = DoorwayToTheDepthsV2 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doorwayToTheDepthsV2 :: LocationCard DoorwayToTheDepthsV2
doorwayToTheDepthsV2 = location DoorwayToTheDepthsV2 Cards.doorwayToTheDepthsV2 5 (Static 1)

instance HasAbilities DoorwayToTheDepthsV2 where
  getAbilities (DoorwayToTheDepthsV2 a) =
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

{- | "True Believer's Den": unlike v3 the green key arrives on the Apostle of Dagon
rather than on the location, so it has to be fought for.
-}
instance RunMessage DoorwayToTheDepthsV2 where
  runMessage msg l@(DoorwayToTheDepthsV2 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- "Search the set-aside cards and victory display for Apostle of Dagon and spawn
      -- it at the revealed location with the lowest shroud." No matcher ranks locations
      -- by shroud, so the lowest is picked here.
      selectOne (enemyIs Enemies.apostleOfDagon) >>= \case
        Just eid -> placeKey eid GreenKey
        Nothing -> do
          revealed <- select RevealedLocation
          shrouds <- for revealed \lid -> (lid,) <$> fieldWithDefault 0 LocationShroud lid
          for_ (listToMaybe $ sortOn snd shrouds) \(lid, _) -> do
            cards <- getSetAsideCardsMatching (cardIs Enemies.apostleOfDagon)
            for_ (listToMaybe cards) \card -> do
              eid <- createEnemyAt card lid
              placeKey eid GreenKey
      pure l
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      remember UnlockedTheFinalDepths
      pure l
    _ -> DoorwayToTheDepthsV2 <$> liftRunMessage msg attrs

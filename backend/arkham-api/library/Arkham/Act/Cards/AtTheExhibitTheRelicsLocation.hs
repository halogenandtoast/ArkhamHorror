module Arkham.Act.Cards.AtTheExhibitTheRelicsLocation (atTheExhibitTheRelicsLocation) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheExhibitTheRelicsLocation = AtTheExhibitTheRelicsLocation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheExhibitTheRelicsLocation :: ActCard AtTheExhibitTheRelicsLocation
atTheExhibitTheRelicsLocation =
  act (2, A) AtTheExhibitTheRelicsLocation Cards.atTheExhibitTheRelicsLocation
    $ Just
    $ GroupClueCost (PerPlayer 2) "Eztli Exhibit"

instance RunMessage AtTheExhibitTheRelicsLocation where
  runMessage msg a@(AtTheExhibitTheRelicsLocation attrs) = runQueueT $ case msg of
    AdvanceAct (isSide B attrs -> True) _ _ -> do
      deckCount <- getActDecksInPlayCount
      n <- perPlayer 1
      townHallId <-
        selectOne (LocationWithTitle "Town Hall") >>= \case
          Just townHall -> do
            placeClues attrs townHall $ n + if deckCount <= 2 then n else 0
            pure townHall
          Nothing -> do
            locationId <- placeLocation =<< getSetAsideCard Locations.townHall
            when (deckCount <= 2) $ placeClues attrs locationId n
            pure locationId

      createAssetAt_ Assets.relicOfAgesADeviceOfSomeSort (AttachedToLocation townHallId)
      advanceToAct attrs Acts.findTheRelic A
      pure a
    _ -> AtTheExhibitTheRelicsLocation <$> liftRunMessage msg attrs

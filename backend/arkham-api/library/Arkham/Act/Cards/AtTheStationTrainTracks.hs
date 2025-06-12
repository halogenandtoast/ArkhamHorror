module Arkham.Act.Cards.AtTheStationTrainTracks (atTheStationTrainTracks) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Import.Lifted
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Placement

newtype AtTheStationTrainTracks = AtTheStationTrainTracks ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheStationTrainTracks :: ActCard AtTheStationTrainTracks
atTheStationTrainTracks =
  act (2, C) AtTheStationTrainTracks Cards.atTheStationTrainTracks
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Arkham Police Station"

instance RunMessage AtTheStationTrainTracks where
  runMessage msg a@(AtTheStationTrainTracks attrs) = runQueueT $ case msg of
    AdvanceAct (isSide D attrs -> True) _ _ -> do
      locationId <- placeLocation =<< genCard Locations.trainTracks
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      createAssetAt_ alejandroVela (AttachedToLocation locationId)
      advanceToAct attrs Acts.alejandrosPrison C
      pure a
    _ -> AtTheStationTrainTracks <$> liftRunMessage msg attrs

module Arkham.Act.Cards.AtTheStationTrainTracks (
  AtTheStationTrainTracks (..),
  atTheStationTrainTracks,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Card
import Arkham.Classes
import Arkham.Helpers.Query
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
  runMessage msg a@(AtTheStationTrainTracks attrs) = case msg of
    AdvanceAct aid _ _ | aid == toId attrs && onSide D attrs -> do
      trainTracks <- genCard Locations.trainTracks
      (locationId, placeTrainTracks) <- placeLocation trainTracks
      alejandroVela <- getSetAsideCard Assets.alejandroVela
      assetId <- getRandom
      pushAll
        [ placeTrainTracks
        , CreateAssetAt assetId alejandroVela (AttachedToLocation locationId)
        , AdvanceToAct (actDeckId attrs) Acts.alejandrosPrison C (toSource attrs)
        ]
      pure a
    _ -> AtTheStationTrainTracks <$> runMessage msg attrs

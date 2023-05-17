module Arkham.Act.Cards.AtTheExhibitTheRelicsLocation
  ( AtTheExhibitTheRelicsLocation(..)
  , atTheExhibitTheRelicsLocation
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Placement
import Arkham.Scenarios.ThreadsOfFate.Helpers

newtype AtTheExhibitTheRelicsLocation = AtTheExhibitTheRelicsLocation ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

atTheExhibitTheRelicsLocation :: ActCard AtTheExhibitTheRelicsLocation
atTheExhibitTheRelicsLocation =
  act (2, A) AtTheExhibitTheRelicsLocation Cards.atTheExhibitTheRelicsLocation
    $ Just
    $ GroupClueCost (PerPlayer 2)
    $ LocationWithTitle "Eztli Exhibit"

instance RunMessage AtTheExhibitTheRelicsLocation where
  runMessage msg a@(AtTheExhibitTheRelicsLocation attrs) = case msg of
    AdvanceAct aid _ _ | aid == actId attrs && onSide B attrs -> do
      mTownHall <- selectOne $ LocationWithTitle "Town Hall"
      deckCount <- getActDecksInPlayCount
      relicOfAges <- getSetAsideCard Assets.relicOfAgesADeviceOfSomeSort
      n <- perPlayer 1
      (townHallId, msgs) <- case mTownHall of
        Just townHall -> do
          pure
            ( townHall
            , [ PlaceClues
                  (LocationTarget townHall)
                  (n + if deckCount <= 2 then n else 0)
              ]
            )
        Nothing -> do
          townHall <- getSetAsideCard Locations.townHall
          locationId <- getRandom
          pure
            ( locationId
            , PlaceLocation locationId townHall
              : [ PlaceClues (LocationTarget locationId) n | deckCount <= 2 ]
            )

      assetId <- getRandom
      pushAll
        $ msgs
        <> [ CreateAssetAt assetId relicOfAges (AttachedToLocation townHallId)
           , AdvanceToAct (actDeckId attrs) Acts.findTheRelic A (toSource attrs)
           ]
      pure a
    _ -> AtTheExhibitTheRelicsLocation <$> runMessage msg attrs

module Arkham.Scenarios.HorrorInHighGear.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, whenNone)
import Arkham.Direction
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Label
import Arkham.Layout
import Arkham.Location.Cards qualified as Location
import Arkham.Matcher
import Arkham.Message (Message (PlacedLocationDirection, SetLayout, SetLocationLabel))
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenario.Deck
import Data.Text qualified as T
import GHC.Records
import Text.Read

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "horrorInHighGear" a

road :: (ReverseQueue m, HasField "label" a Text, HasField "id" a LocationId) => Int -> a -> m ()
road n attrs = do
  let
    x =
      case unsnoc attrs.label of
        Just (lbl, _) -> read @Int $ drop 4 $ unpack lbl
        Nothing -> error "impossible"
  let prefix = "road" <> tshow (x + 1)
  whenNone (LocationWithLabel $ Label $ prefix <> "a") do
    roadCard <- take 1 <$> getScenarioDeck RoadDeck
    longWays <- take (n - 1) <$> getSetAsideCardsMatching (cardIs Location.longWayAround)
    cards <- shuffleM $ roadCard <> longWays

    for_ (zip cards "abc") $ \(lid, c) -> do
      location <- placeLocation lid
      push $ SetLocationLabel location (prefix <> singleton c)
      push $ PlacedLocationDirection location RightOf attrs.id
    layout <- getLayout
    let
      go =
        case length cards of
          1 -> \y -> if y `elem` [3, 4] then (" " <> prefix <> "a") else " ."
          2 -> \case
            1 -> " ."
            2 -> " " <> prefix <> "a"
            3 -> " " <> prefix <> "a"
            4 -> " " <> prefix <> "b"
            5 -> " " <> prefix <> "b"
            6 -> " ."
            _ -> error "impossible"
          3 -> \case
            1 -> " " <> prefix <> "a"
            2 -> " " <> prefix <> "a"
            3 -> " " <> prefix <> "b"
            4 -> " " <> prefix <> "b"
            5 -> " " <> prefix <> "c"
            6 -> " " <> prefix <> "c"
            _ -> error "impossible"
          _ -> error "impossible"
      layout' = map (\(idx, GridTemplateRow row) -> GridTemplateRow $ row <> go idx) (withIndex1 layout)
    push $ SetLayout layout'

getRear :: HasGame m => m [LocationId]
getRear = do
  let
    getRearLabel (GridTemplateRow txt) =
      case (T.words txt) of
        [] -> Nothing
        ("." : _) -> Nothing
        (x : _) -> Just x
  labels <- mapMaybe getRearLabel <$> getLayout
  select $ mapOneOf (LocationWithLabel . Label) labels

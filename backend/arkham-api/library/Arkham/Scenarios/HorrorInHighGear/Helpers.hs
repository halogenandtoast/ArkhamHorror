module Arkham.Scenarios.HorrorInHighGear.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue (push)
import Arkham.Classes.Query (select, selectAny, selectField, selectWithField, whenNone)
import Arkham.Direction
import Arkham.Helpers.Cost
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.I18n
import Arkham.Id
import Arkham.Label
import Arkham.Layout
import Arkham.Location.Cards qualified as Location
import Arkham.Location.Types (Field (..), LocationAttrs)
import Arkham.Matcher
import Arkham.Message (
  Message (PlacedLocationDirection, SetLayout, SetLocationLabel, SpendClues),
 )
import Arkham.Message.Lifted
import Arkham.Prelude
import Arkham.Scenario.Deck
import Arkham.Trait (Trait (Vehicle))
import Arkham.Window
import Arkham.Window qualified as Window
import Data.Aeson qualified as Aeson
import Data.Foldable (foldl)
import Data.Text qualified as T
import GHC.Records
import Text.Read

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "horrorInHighGear" a

getRoadDeck :: HasGame m => m [Card]
getRoadDeck = getScenarioDeck RoadDeck

getLabelPosition :: Text -> Int
getLabelPosition label' = case unsnoc label' of
  Just (lbl, _) -> read @Int $ drop 4 $ unpack lbl
  Nothing -> error "impossible"

road :: (ReverseQueue m, HasField "label" a Text, HasField "id" a LocationId) => Int -> a -> m ()
road n attrs = do
  let x = getLabelPosition attrs.label
  let prefix = "road" <> tshow (x + 1)
  whenNone (LocationWithLabel $ Label $ prefix <> "a") do
    roadCard <- take 1 <$> getRoadDeck
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
  labels <- getRearLabels <$> getLayout
  select $ mapOneOf (LocationWithLabel . Label) labels

getRearLabels :: [GridTemplateRow] -> [Text]
getRearLabels layout =
  let
    getRearLabel (GridTemplateRow txt) =
      case (T.words txt) of
        [] -> Nothing
        ("." : _) -> Nothing
        (x : _) -> Just x
   in
    mapMaybe getRearLabel layout

getRearmostInvestigator :: HasGame m => m [InvestigatorId]
getRearmostInvestigator = do
  locations <- selectWithField LocationLabel $ LocationWithInvestigator Anyone
  case nonEmpty locations of
    Nothing -> pure []
    Just (x :| xs) -> do
      let chooseRearmost z@(_, l1) y@(_, l2) = if getLabelPosition l1 > getLabelPosition l2 then y else z
      let (rearmost, _) = foldl chooseRearmost x xs
      select $ investigatorAt rearmost

advanceRoad :: ReverseQueue m => m ()
advanceRoad = do
  mStopAt <-
    getRearmostInvestigator >>= \case
      [] -> pure Nothing
      (x : _) ->
        fmap (subtract 1 . getLabelPosition)
          . listToMaybe
          <$> selectField LocationLabel (locationWithInvestigator x)
  push . SetLayout =<< go mStopAt =<< getLayout
 where
  go mStopAt layout = do
    case getRearLabels layout of
      [] -> error "impossible"
      xs@(x : _) -> do
        shouldStop <-
          orM
            [ pure $ maybe False (== getLabelPosition x) mStopAt
            , selectAny $ AssetWithTrait Vehicle <> AssetAt (mapOneOf (LocationWithLabel . Label) xs)
            , selectAny $ EnemyAt (mapOneOf (LocationWithLabel . Label) xs)
            ]
        if shouldStop
          then pure layout
          else do
            selectEach (mapOneOf (LocationWithLabel . Label) xs) removeLocation
            go mStopAt
              $ map (\(GridTemplateRow row) -> GridTemplateRow . T.unwords . drop 1 $ T.words row) layout

getLeavingVehicle :: [Window] -> AssetId
getLeavingVehicle = \case
  [] -> error "impossible"
  ((windowType -> Window.VehicleLeaves aid _) : _) -> aid
  (_ : rest) -> getLeavingVehicle rest

getEnteringVehicle :: [Window] -> AssetId
getEnteringVehicle = \case
  [] -> error "impossible"
  ((windowType -> Window.VehicleEnters aid _) : _) -> aid
  (_ : rest) -> getEnteringVehicle rest

handleVehicleLeaves :: ReverseQueue m => AssetId -> LocationId -> Int -> m Int
handleVehicleLeaves vehicle here per = do
  investigators <- select $ investigatorAt here
  n <- getSpendableClueCount investigators
  inVehicle <- select $ InVehicleMatching (AssetWithId vehicle)
  let total = length inVehicle * per
  when (n > 0) $ push $ SpendClues (min n total) investigators
  pure $ max 0 (total - n)

notSeenVehicle :: LocationAttrs -> AssetMatcher
notSeenVehicle attrs = case (maybeResult =<< attrs.global "seenVehicles") of
  Nothing -> AnyAsset
  Just xs -> not_ (mapOneOf AssetWithId xs)

sawVehicle :: AssetId -> Map Aeson.Key Value -> Map Aeson.Key Value
sawVehicle aid kmap = insertMap "seenVehicles" (toJSON (aid : current)) kmap
 where
  current = maybe [] (toResultDefault []) $ lookup "seenVehicles" kmap

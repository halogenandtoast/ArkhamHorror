module Arkham.Scenarios.WrittenInRock.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Card.CardDef
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Classes.Query
import Arkham.Direction
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..))
import Arkham.Matcher.Location
import Arkham.Message
import Arkham.Message.Lifted.Queue
import Arkham.Modifier
import Arkham.Prelude
import Arkham.Projection
import Arkham.Tracing

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "writtenInRock" a

pattern CannotBeSlidOrSwapped :: ModifierType
pattern CannotBeSlidOrSwapped = ScenarioModifier "cannotBeSlidOrSwapped"

pattern CanBeSlid :: ModifierType
pattern CanBeSlid = ScenarioModifier "canBeSlid"

pattern LocationCanBeSwapped :: LocationMatcher
pattern LocationCanBeSwapped = LocationWithoutModifier CannotBeSlidOrSwapped

pattern LocationCanBeSlid :: LocationMatcher
pattern LocationCanBeSlid = LocationWithModifier CanBeSlid

swapLocations :: ReverseQueue m => LocationId -> LocationId -> m ()
swapLocations lid1 lid2 = do
  pos1 <- fieldJust LocationPosition lid1
  pos2 <- fieldJust LocationPosition lid2
  push $ PlaceGrid (GridLocation pos2 lid1)
  push $ PlaceGrid (GridLocation pos1 lid2)

clampPositions :: [Pos] -> [Pos]
clampPositions = filter (\(Pos x y) -> x >= 1 && x <= 5 && y >= 1 && y <= 4)

getEmptyPositions :: (Tracing m, HasGame m) => LocationId -> m [Pos]
getEmptyPositions lid = do
  pos <- fieldJust LocationPosition lid
  let candidates = clampPositions $ adjacentPositions pos
  filterM (selectNone . LocationInPosition) candidates

getRails :: (Tracing m, HasGame m) => LocationId -> m [GridDirection]
getRails lid = do
  def <- field LocationCardDef lid
  pure $ maybe [] (toResultDefault []) $ lookup "rails" (cdMeta def)

forEachRail :: (Tracing m, HasGame m) => LocationId -> (GridDirection -> m ()) -> m ()
forEachRail lid f = do
  rails <- getRails lid
  for_ rails f

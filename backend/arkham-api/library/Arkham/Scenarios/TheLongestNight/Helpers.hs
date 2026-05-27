module Arkham.Scenarios.TheLongestNight.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.Direction
import Arkham.I18n
import Arkham.Id
import Arkham.Location.Grid (Pos (..))
import Arkham.Message.Lifted.Queue
import Arkham.Message.Lifted.Scenario
import Arkham.Modifier
import Arkham.Prelude
import Arkham.SortedPair
import Arkham.Source
import Data.Map.Strict qualified as Map

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theLongestNight" a

data Meta = Meta
  { barriers :: Map (SortedPair LocationId) Int
  , discardNextEnemyDraw :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON Meta where
  parseJSON = withObject "Meta" $ \o -> do
    barriers <- o .: "barriers"
    discardNextEnemyDraw <- o .:? "discardNextEnemyDraw" .!= False
    pure Meta {..}

defaultMeta :: Meta
defaultMeta = Meta mempty False

pattern CannotHaveDecoys :: ModifierType
pattern CannotHaveDecoys = ScenarioModifier "cannotHaveDecoys"

pattern CannotHaveTraps :: ModifierType
pattern CannotHaveTraps = ScenarioModifier "cannotHaveTraps"

pattern IgnoreBarriers :: ModifierType
pattern IgnoreBarriers = ScenarioModifier "ignoreBarriers"

pattern IgnoreDecoys :: ModifierType
pattern IgnoreDecoys = ScenarioModifier "ignoreDecoys"

pattern IgnoreTraps :: ModifierType
pattern IgnoreTraps = ScenarioModifier "ignoreTraps"

pattern IgnoreFireDamage :: ModifierType
pattern IgnoreFireDamage = ScenarioModifier "ignoreFireDamage"

incrementBarriers :: Int -> LocationId -> LocationId -> Meta -> Meta
incrementBarriers n a b meta =
  meta {barriers = Map.insertWith (+) (sortedPair a b) n meta.barriers}

decrementBarriers :: Int -> LocationId -> LocationId -> Meta -> Meta
decrementBarriers n a b meta =
  meta {barriers = Map.insertWith ((max 0 .) . subtract) (sortedPair a b) n meta.barriers}

directionBetween :: Pos -> Pos -> Maybe GridDirection
directionBetween (Pos x1 y1) (Pos x2 y2) = case (x2 - x1, y2 - y1) of
  (1, 0) -> Just East
  (-1, 0) -> Just West
  (0, 1) -> Just North
  (0, -1) -> Just South
  _ -> Nothing

barrierModifier :: GridDirection -> Text
barrierModifier = \case
  North -> "barrierNorth"
  South -> "barrierSouth"
  East -> "barrierEast"
  West -> "barrierWest"

placeTrap :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
placeTrap source lid = scenarioSpecific "placeTrap" (toSource source, lid)

placeDecoy :: (ReverseQueue m, Sourceable source) => source -> LocationId -> m ()
placeDecoy source lid = scenarioSpecific "placeDecoy" (toSource source, lid)

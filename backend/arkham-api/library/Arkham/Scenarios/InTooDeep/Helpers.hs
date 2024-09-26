module Arkham.Scenarios.InTooDeep.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.I18n
import Arkham.Id
import Arkham.Prelude
import Arkham.SortedPair
import Data.Map.Strict qualified as Map

newtype Meta = Meta {barriers :: Map (SortedPair LocationId) Int}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

insertBarrier :: LocationId -> LocationId -> Meta -> Meta
insertBarrier a b (Meta barriers) =
  Meta $ Map.insertWith (+) (sortedPair a b) 1 barriers

removeBarrier :: LocationId -> LocationId -> Meta -> Meta
removeBarrier a b (Meta barriers) =
  Meta $ Map.insertWith (+) (sortedPair a b) (-1) barriers

setBarriers :: LocationId -> LocationId -> Int -> Meta -> Meta
setBarriers a b n (Meta barriers) =
  Meta $ Map.insert (sortedPair a b) n barriers

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "inTooDeep" a

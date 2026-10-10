{- | Campaign-specific helpers for Ages Unwound.

Besides the campaign's i18n scopes, this is the single source of truth for
"recording the time": the @(agenda number, doom on agenda)@ pair that Scenarios
III and VI write and compare. Nothing calls 'recordTheTime' / 'isAtOrPast' yet
-- Phase B's scenario work does.
-}
module Arkham.Homebrew.AgesUnwound.Helpers where

import Arkham.Action.Additional (isStandardAdditionalAction)
import Arkham.Agenda.Types (Field (AgendaDoom))
import Arkham.Classes.HasGame
import Arkham.Classes.Query (selectOne)
import Arkham.Helpers.Agenda (getAgendaStep)
import Arkham.Helpers.Log (getSomeRecordSetJSON)
import Arkham.Homebrew.AgesUnwound.Key
import Arkham.I18n
import Arkham.Id (InvestigatorId)
import Arkham.Investigator.Types (
  Field (InvestigatorAdditionalActions, InvestigatorRemainingActions),
 )
import Arkham.Matcher
import Arkham.Message.Lifted.Log (recordSetInsert)
import Arkham.Message.Lifted.Queue (ReverseQueue)
import Arkham.Prelude
import Arkham.Projection

campaignI18n :: (HasI18n => a) -> a
campaignI18n a = withI18n $ scope "agesUnwound" a

scenarioI18n :: Scope -> (HasI18n => a) -> a
scenarioI18n scenarioScope a = campaignI18n $ scope scenarioScope a

{- | A campaign-log entry written "with a time", e.g.
@the boundary is broken (1,5)@.

One record set with a structured entry rather than two count keys, so several
timed entries can coexist and still be compared pairwise. 'Recordable' already
covers 'Data.Aeson.Value', which is what 'recordSetInsert' stores and
'getSomeRecordSetJSON' reads back.
-}
data RecordedTime = RecordedTime
  { event :: Text
  , agenda :: Int
  , doom :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | The current @(agenda, doom)@ pair, or @(3, 4)@ when no agenda is in play --
the fallback the guide prints for Scenario III ending with an empty agenda deck.
-}
getTheTime :: HasGame m => m (Int, Int)
getTheTime =
  selectOne AnyAgenda >>= \case
    Nothing -> pure (3, 4)
    Just aid -> (,) <$> getAgendaStep aid <*> field AgendaDoom aid

-- | Record a campaign-log key together with the time it happened at.
recordTheTime :: ReverseQueue m => Text -> m ()
recordTheTime e = do
  (a, d) <- getTheTime
  recordSetInsert RecordedTimes [toJSON (RecordedTime e a d)]

{- | "the current agenda number is greater than the recorded number, or equal to
it with at least as much doom" -- which is exactly @>=@ on the pair.
-}
isAtOrPast :: HasGame m => Text -> m Bool
isAtOrPast e = do
  recorded <- getSomeRecordSetJSON @RecordedTime RecordedTimes
  now <- getTheTime
  pure $ any (\(RecordedTime e' a d) -> e' == e && now >= (a, d)) recorded

{- | 'recordTheTime', keyed off the campaign-log key itself so every caller
agrees on the tag. Use this rather than passing a hand-written 'Text'.
-}
recordTheTimeFor :: ReverseQueue m => AgesUnwoundKey -> m ()
recordTheTimeFor = recordTheTime . tshow

-- | 'isAtOrPast' for a campaign-log key recorded with 'recordTheTimeFor'.
isAtOrPastFor :: HasGame m => AgesUnwoundKey -> m Bool
isAtOrPastFor = isAtOrPast . tshow

{- | The campaign's /standard actions/: an investigator's remaining actions plus
every additional action without a limitation on its use.

> A standard action is any action that does not have a limitation on its use,
> regardless of its source.

The read-side counterpart of 'Arkham.Message.LoseStandardActions', which is how
this campaign's cards take one away. Several cards in the @night_of_the_ritual@
set count them ("for each standard action you have remaining", "where X is the
number of standard actions you have").
-}
getStandardActions :: HasGame m => InvestigatorId -> m Int
getStandardActions iid = do
  remaining <- field InvestigatorRemainingActions iid
  additional <- count isStandardAdditionalAction <$> field InvestigatorAdditionalActions iid
  pure $ remaining + additional

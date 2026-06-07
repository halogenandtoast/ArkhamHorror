{-# OPTIONS_GHC -Wno-unused-imports -Wno-orphans #-}


module Arkham.Message.Lifted.Base where


import Arkham.Helpers.FetchCard as X

import Arkham.Ability
import Arkham.Act.Sequence qualified as Act
import Arkham.Act.Types (ActAttrs (actDeckId))
import Arkham.Action (Action)
import Arkham.Agenda.Sequence qualified as Agenda
import Arkham.Agenda.Types (AgendaAttrs (agendaDeckId))
import Arkham.Aspect (IsAspect (..))
import Arkham.Aspect qualified as Msg
import Arkham.Asset.Types (AssetAttrs)
import Arkham.Asset.Types qualified as Field
import Arkham.Asset.Uses (UseType)
import Arkham.Attack
import Arkham.Calculation
import Arkham.CampaignStep hiding (continue)
import Arkham.CampaignStep qualified as CS
import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Capability
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.Classes.GameLogger
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue hiding (insertAfterMatching)
import Arkham.Classes.HasQueue as X (runQueueT)
import Arkham.Classes.Query
import Arkham.DamageEffect
import Arkham.Deck (IsDeck)
import Arkham.Discover as X (IsInvestigate (..))
import Arkham.Discover qualified as Msg
import Arkham.Draw.Types
import Arkham.Effect.Builder
import Arkham.Effect.Types (EffectBuilder (effectBuilderEffectId), Field (..))
import Arkham.Effect.Window
import Arkham.EffectMetadata (EffectMetadata)
import Arkham.EncounterSet
import Arkham.Enemy.Creation
import Arkham.Enemy.Helpers qualified as Msg
import Arkham.Enemy.Types (Field (..))
import Arkham.Evade
import Arkham.Evade qualified as Evade
import Arkham.Exhaust qualified as Exhaust
import Arkham.Fight
import Arkham.Fight qualified as Fight
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers
import Arkham.Helpers.Act
import Arkham.Helpers.Agenda
import Arkham.Helpers.Campaign
import Arkham.Helpers.Campaign qualified as Msg
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Helpers.ChaosToken qualified as Msg
import Arkham.Helpers.Effect qualified as Msg
import Arkham.Helpers.Enemy qualified as Msg
import Arkham.Helpers.GameValue (getGameValue)
import Arkham.Helpers.Investigator (
  canHaveDamageHealed,
  canHaveHorrorHealed,
  getCanDiscoverClues,
 )
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.Message qualified as Msg
import Arkham.Helpers.Message.Discard qualified as HandDiscard
import Arkham.Helpers.Modifiers qualified as Msg
import Arkham.Helpers.Query
import Arkham.Helpers.Ref (sourceToTarget)
import Arkham.Helpers.Scenario (getEncounterDeckKey, getInResolution, getIsStandalone)
import Arkham.Helpers.Shuffle
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Helpers.UI qualified as Msg
import Arkham.Helpers.Window qualified as Msg
import Arkham.Helpers.Xp
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigate
import Arkham.Investigate qualified as Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Key
import Arkham.Layout
import Arkham.Location.Grid
import Arkham.Location.Types (Field (..), Location)
import Arkham.Matcher hiding (PerformAction)
import Arkham.Message hiding (story)
import Arkham.Message as X (AndThen (..), getChoiceAmount, optionWhenExists, preOriginalOption)
import Arkham.Message.Lifted.Queue as X
import Arkham.Modifier
import Arkham.Name
import Arkham.Phase (Phase)
import Arkham.Placement
import Arkham.Prelude hiding (pred)
import Arkham.Projection
import Arkham.Query
import Arkham.Queue
import Arkham.RequestedChaosTokenStrategy
import Arkham.Scenario.Deck
import Arkham.Search qualified as Search
import Arkham.SkillTest.Step
import Arkham.SkillType
import Arkham.SkillType qualified as SkillType
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Token
import Arkham.Tracing
import Arkham.Trait (Trait)
import Arkham.Window (Window (..), WindowType, defaultWindows)
import Arkham.Window qualified as Window
import Arkham.Xp
import Control.Monad.State.Strict (MonadState, StateT, execStateT, get, put)
import Control.Monad.Trans.Class
import Data.Aeson.Key qualified as Aeson
import Data.Typeable

capture :: MonadIO m => QueueT msg m a -> m [msg]
capture = evalQueueT

matchingDon't :: (MonadTrans t, HasQueue Message m) => (Message -> Bool) -> t m ()
matchingDon't f = lift $ popMessageMatching_ f

withTimings :: ReverseQueue m => WindowType -> m () -> m ()
withTimings w body = do
  let (before, atIf, after) = Msg.timings w
  checkWindows [before]
  checkWindows [atIf]
  body
  checkWindows [after]

withBatchedTimings :: ReverseQueue m => WindowType -> QueueT Message m () -> m ()
withBatchedTimings w body = do
  batched \batchId -> do
    let (before, atIf, after) = Msg.batchedTimings batchId w
    checkWindows [before]
    checkWindows [atIf]
    body
    checkWindows [after]

selectEach :: (Query a, HasGame m, Tracing m) => a -> (QueryElement a -> m ()) -> m ()
selectEach matcher f = select matcher >>= traverse_ f

checkWindows :: ReverseQueue m => [Window] -> m ()
checkWindows [] = pure ()
checkWindows ws = Msg.pushM $ Msg.checkWindows ws

checkAfter :: ReverseQueue m => WindowType -> m ()
checkAfter = Msg.pushM . Msg.checkAfter

checkWhen :: ReverseQueue m => WindowType -> m ()
checkWhen = Msg.pushM . Msg.checkWhen

batched :: ReverseQueue m => (BatchId -> QueueT Message m ()) -> m ()
batched f = do
  batchId <- getId
  withBatched batchId f

batchedOrCurrent :: ReverseQueue m => (BatchId -> QueueT Message m ()) -> m ()
batchedOrCurrent f =
  getCurrentBatchId >>= \case
    Just batchId -> withBatched batchId f
    Nothing -> batched f

withBatched :: ReverseQueue m => BatchId -> (BatchId -> QueueT Message m ()) -> m ()
withBatched batchId f = do
  msgs <- capture (f batchId)
  push $ Would batchId $ map updateBatch msgs
 where
  -- Sets the batch id for any top level window calls
  updateBatch = \case
    CheckWindows ws -> CheckWindows $ map (\w -> w {windowBatchId = Just batchId}) ws
    other -> other

payBatchCost :: ReverseQueue m => BatchId -> InvestigatorId -> Cost -> m ()
payBatchCost batchId iid cost = push $ PayAdditionalCost iid batchId cost

withCost :: ReverseQueue m => InvestigatorId -> Cost -> QueueT Message m () -> m ()
withCost iid cost f = batched \batchId -> payBatchCost batchId iid cost >> f

setupModifier
  :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> ModifierType -> m ()
setupModifier source target modifier = Msg.pushM $ Msg.setupModifier source target modifier

shuffleCardsIntoDeck
  :: ( ReverseQueue m
     , IsDeck deck
     , MonoFoldable cards
     , Element cards ~ card
     , IsCard card
     , CanShuffleIn cards
     )
  => deck
  -> cards
  -> m ()
shuffleCardsIntoDeck deck cards = whenCanShuffleIn deck cards do
  push $ Msg.shuffleCardsIntoDeck deck cards

chooseAndDiscardAssetMatching
  :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> AssetMatcher -> m ()
chooseAndDiscardAssetMatching iid source matcher = push $ ChooseAndDiscardAsset iid (toSource source) matcher

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Arkham.Message.Lifted.Prompt where


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
import Arkham.Deck (IsDeck (..))
import Arkham.Deck qualified as Deck
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
import Data.Map.Strict qualified as Map
import Data.Typeable

chooseOrRunOne :: (ReverseQueue m, HasCallStack) => InvestigatorId -> [UI Message] -> m ()
chooseOrRunOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunOne player msgs

continue :: ReverseQueue m => InvestigatorId -> QueueT Message m () -> m ()
continue iid = prompt iid "$label.continue"

continue_ :: ReverseQueue m => InvestigatorId -> m ()
continue_ iid = continue iid (pure ())

prompt :: ReverseQueue m => InvestigatorId -> Text -> QueueT Message m () -> m ()
prompt iid lbl body = do
  msgs <- capture body
  Arkham.Message.Lifted.chooseOne iid [Label lbl msgs]

promptI :: ReverseQueue m => InvestigatorId -> Text -> QueueT Message m () -> m ()
promptI iid lbl body = do
  msgs <- capture body
  Arkham.Message.Lifted.chooseOne iid [Label (withI18n $ "$" <> labelKey lbl) msgs]

prompt_ :: (HasI18n, ReverseQueue m) => InvestigatorId -> Text -> m ()
prompt_ iid lbl = Arkham.Message.Lifted.chooseOne iid [Label ("$" <> labelKey lbl) []]

promptI_ :: ReverseQueue m => InvestigatorId -> Text -> m ()
promptI_ iid lbl = withI18n $ prompt_ iid lbl

choose :: ReverseQueue m => InvestigatorId -> UI Message -> m ()
choose iid msg = Arkham.Message.Lifted.chooseOne iid [msg]

questionLabel :: ReverseQueue m => Text -> InvestigatorId -> Question Message -> m ()
questionLabel lbl iid q = do
  pid <- getPlayer iid
  push $ Ask pid (QuestionLabel lbl Nothing q)

questionLabel' :: ReverseQueue m => Text -> InvestigatorId -> Question Message -> m ()
questionLabel' lbl = Arkham.Message.Lifted.questionLabel ("$" <> lbl)

chooseOne :: (HasCallStack, ReverseQueue m) => InvestigatorId -> [UI Message] -> m ()
chooseOne iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOne player msgs

chooseOneFromEach :: (HasCallStack, ReverseQueue m) => InvestigatorId -> [[UI Message]] -> m ()
chooseOneFromEach iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneFromEach player msgs

chooseSome :: ReverseQueue m => InvestigatorId -> Text -> [UI Message] -> m ()
chooseSome iid done msgs = do
  player <- getPlayer iid
  push $ Msg.chooseSome player done msgs

chooseSome1 :: ReverseQueue m => InvestigatorId -> Text -> [UI Message] -> m ()
chooseSome1 iid done msgs = do
  player <- getPlayer iid
  push $ Msg.chooseSome1 player done msgs

chooseUpToN :: ReverseQueue m => InvestigatorId -> Int -> Text -> [UI Message] -> m ()
chooseUpToN iid n label msgs = do
  player <- getPlayer iid
  push $ Msg.chooseUpToN player n label msgs

chooseOneAtATime :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOneAtATime iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneAtATime player msgs

chooseOrRunOneAtATime :: ReverseQueue m => InvestigatorId -> [UI Message] -> m ()
chooseOrRunOneAtATime iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunOneAtATime player msgs

chooseOneDropDown :: ReverseQueue m => InvestigatorId -> [(Text, Message)] -> m ()
chooseOneDropDown iid msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOneDropDown player msgs

chooseN :: ReverseQueue m => InvestigatorId -> Int -> [UI Message] -> m ()
chooseN iid n msgs = do
  player <- getPlayer iid
  push $ Msg.chooseN player n msgs

chooseOrRunN :: ReverseQueue m => InvestigatorId -> Int -> [UI Message] -> m ()
chooseOrRunN iid n msgs = do
  player <- getPlayer iid
  push $ Msg.chooseOrRunN player n msgs

chooseUpgradeDecks :: ReverseQueue m => m ()
chooseUpgradeDecks = push . Msg.chooseUpgradeDecks =<< allPlayers

chooseAndDiscardAsset :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> m ()
chooseAndDiscardAsset iid source = chooseAndDiscardAssetMatching iid source AnyAsset

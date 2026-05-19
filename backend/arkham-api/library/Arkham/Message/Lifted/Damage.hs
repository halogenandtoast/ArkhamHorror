{-# OPTIONS_GHC -Wno-unused-imports #-}

module Arkham.Message.Lifted.Damage where


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
import Arkham.Message.Lifted.Base
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

dealAssetDamage :: (ReverseQueue m, Sourceable source) => AssetId -> source -> Int -> m ()
dealAssetDamage aid source damage = push $ Msg.DealAssetDamageWithCheck aid (toSource source) damage 0 True

dealAssetHorror :: (ReverseQueue m, Sourceable source) => AssetId -> source -> Int -> m ()
dealAssetHorror aid source horror = push $ Msg.DealAssetDamageWithCheck aid (toSource source) 0 horror True

directDamage :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
directDamage iid source = push . Msg.directDamage iid source

directHorror :: (ReverseQueue m, Sourceable source) => InvestigatorId -> source -> Int -> m ()
directHorror iid source = push . Msg.directHorror iid source

gameOverIf :: ReverseQueue m => Bool -> m ()
gameOverIf t = when t gameOver

gameOver :: ReverseQueue m => m ()
gameOver = push GameOver

kill :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
kill (toSource -> source) iid = do
  push $ InvestigatorKilled source iid
  push CheckForRemainingInvestigators

defeat :: (Sourceable source, ReverseQueue m) => source -> InvestigatorId -> m ()
defeat (toSource -> source) iid = do
  push $ InvestigatorWhenDefeated source iid
  push CheckForRemainingInvestigators

drivenInsane :: ReverseQueue m => InvestigatorId -> m ()
drivenInsane iid = do
  push $ DrivenInsane iid
  push CheckForRemainingInvestigators

assignEnemyDamage :: ReverseQueue m => DamageAssignment -> EnemyId -> m ()
assignEnemyDamage assignment = push . Msg.assignEnemyDamage assignment

attackEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
attackEnemyDamage source damage enemy = do
  whenM (enemy <=~> EnemyCanBeDamagedBySource (toSource source)) do
    push $ Msg.EnemyDamage enemy (attack source damage)

storyEnemyDamage :: (ReverseQueue m, Sourceable a) => a -> Int -> EnemyId -> m ()
storyEnemyDamage source damage enemy = push $ Msg.EnemyDamage enemy (storyDamage source damage)

applyHealing :: (ReverseQueue m, Sourceable source) => source -> m ()
applyHealing source = push $ Msg.ApplyHealing (toSource source)

checkDefeated :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
checkDefeated source target = push $ Msg.checkDefeated source target

addCurseTokens :: ReverseQueue m => Maybe InvestigatorId -> Int -> m ()
addCurseTokens mWho n = do
  batchId <- getId
  would <-
    Msg.checkWindows
      [ (Window.mkWhen $ Window.WouldAddChaosTokensToChaosBag mWho $ replicate n #curse)
          { Window.windowBatchId = Just batchId
          }
      ]
  Msg.push $ Would batchId $ would : replicate n (Msg.AddChaosToken #curse)

assetDefeated :: (ReverseQueue m, ToId asset AssetId, Sourceable source) => source -> asset -> m ()
assetDefeated source asset = push $ Msg.AssetDefeated (toSource source) (asId asset)

healAllDamage :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> m ()
healAllDamage source target = push $ Msg.HealAllDamage (toTarget target) (toSource source)

investigatorDefeated :: (ReverseQueue m, Sourceable source) => source -> InvestigatorId -> m ()
investigatorDefeated source iid = push $ Msg.InvestigatorDefeated (toSource source) iid

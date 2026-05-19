{-# OPTIONS_GHC -Wno-unused-imports #-}

module Arkham.Message.Lifted.Scenario where


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

storyI :: (HasI18n, ReverseQueue m) => Text -> m ()
storyI flavor = do
  players <- allPlayers
  push $ Msg.story players (i18n flavor)

story :: ReverseQueue m => FlavorText -> m ()
story flavor = do
  players <- allPlayers
  push $ Msg.story players flavor

storyWhen :: ReverseQueue m => Bool -> FlavorText -> m ()
storyWhen cond flavor = when cond do
  players <- allPlayers
  push $ Msg.story players flavor

storyWithCard :: ReverseQueue m => CardDef -> FlavorText -> m ()
storyWithCard cardDef flavor = do
  players <- allPlayers
  push $ Msg.storyWithCards [cardDef] players flavor

storyWithCards :: ReverseQueue m => [CardDef] -> FlavorText -> m ()
storyWithCards cardDefs flavor = do
  players <- allPlayers
  push $ Msg.storyWithCards cardDefs players flavor

storyOnly :: ReverseQueue m => [InvestigatorId] -> FlavorText -> m ()
storyOnly [] _ = pure ()
storyOnly iids flavor = do
  players <- traverse getPlayer iids
  push $ Msg.story players flavor

storyOnly' :: (HasI18n, ReverseQueue m) => [InvestigatorId] -> Scope -> m ()
storyOnly' [] _ = pure ()
storyOnly' iids lbl = do
  players <- traverse getPlayer iids
  push $ Msg.story players (i18n lbl)

storyWithChooseOne :: ReverseQueue m => FlavorText -> [UI Message] -> m ()
storyWithChooseOne flavor choices = do
  players <- allPlayers
  lead <- getLeadPlayer
  push $ Msg.storyWithChooseOne lead players flavor choices

playerStoryWithChooseOne :: ReverseQueue m => PlayerId -> FlavorText -> [UI Message] -> m ()
playerStoryWithChooseOne pid flavor choices = do
  players <- allPlayers
  push $ Msg.storyWithChooseOne pid players flavor choices

storyWithChooseN :: ReverseQueue m => Int -> FlavorText -> [UI Message] -> m ()
storyWithChooseN n flavor choices = do
  players <- allPlayers
  lead <- getLeadPlayer
  push $ Msg.storyWithChooseN lead players n flavor choices

storyWithChooseUpToN :: ReverseQueue m => Int -> FlavorText -> [UI Message] -> m ()
storyWithChooseUpToN n flavor choices = do
  players <- allPlayers
  lead <- getLeadPlayer
  pushAll
    [ SetActivePlayer lead
    , Msg.storyWithChooseUpToN lead players n flavor choices
    ]

sufferTrauma :: ReverseQueue m => InvestigatorId -> Int -> Int -> m ()
sufferTrauma iid physical mental = push $ SufferTrauma iid physical mental

sufferMentalTrauma :: ReverseQueue m => InvestigatorId -> Int -> m ()
sufferMentalTrauma iid mental = sufferTrauma iid 0 mental

sufferPhysicalTrauma :: ReverseQueue m => InvestigatorId -> Int -> m ()
sufferPhysicalTrauma iid physical = sufferTrauma iid physical 0

allGainXpWithBonus :: (ReverseQueue m, Sourceable source) => source -> XpBonus -> m ()
allGainXpWithBonus source xp = do
  push . ReportXp =<< generateXpReport xp
  pushAll =<< toGainXp source (getXpWithBonus xp.value)

allGainXpWithBonus' :: (ReverseQueue m, Sourceable source) => source -> XpBonus -> m Int
allGainXpWithBonus' source xp = do
  push . ReportXp =<< generateXpReport xp
  (initial, details) <- getXpWithBonus' xp.value
  pushAll =<< toGainXp source (pure details)
  pure initial

allGainXp' :: (ReverseQueue m, Sourceable source) => source -> m Int
allGainXp' source = do
  (initial, details) <- getXp'
  push . ReportXp =<< generateXpReport NoBonus
  pushAll =<< toGainXp source (pure details)
  pure initial

allGainXp :: (ReverseQueue m, Sourceable source) => source -> m ()
allGainXp = void . allGainXp'

interludeXpAll :: ReverseQueue m => XpBonus -> m ()
interludeXpAll xp = do
  investigatorIds <- allInvestigators
  push
    $ ReportXp
    $ XpBreakdown
      [ InvestigatorGainXp iid $ XpDetail XpBonus txt n
      | WithBonus txt n <- xp.flatten
      , iid <- investigatorIds
      ]
  for_ investigatorIds \iid -> do
    push $ GainXP iid CampaignSource xp.value

interludeXp :: ReverseQueue m => InvestigatorId -> XpBonus -> m ()
interludeXp iid xp = do
  pushAll
    [ ReportXp
        $ XpBreakdown
          [InvestigatorGainXp iid $ XpDetail XpBonus txt n | WithBonus txt n <- xp.flatten]
    , GainXP iid CampaignSource xp.value
    ]

endOfScenario :: ReverseQueue m => m ()
endOfScenario = push $ EndOfGame Nothing

endOfScenarioThen :: ReverseQueue m => CampaignStep -> m ()
endOfScenarioThen = push . EndOfGame . CS.continue

addCampaignCardToDeckChoice_ :: (FetchCard card, ReverseQueue m) => card -> m ()
addCampaignCardToDeckChoice_ card = do
  choices <- allInvestigators
  lead <- getLeadPlayer
  card' <- fetchCard card
  push $ Msg.addCampaignCardToDeckChoice lead choices DoNotShuffleIn card'

removeCampaignCard :: (HasCardDef a, ReverseQueue m) => a -> m ()
removeCampaignCard (toCardDef -> def) = do
  mOwner <- getOwner def
  when def.unique do
    case def.kind of
      AssetType -> selectOne (assetIs def) >>= traverse_ removeFromGame
      _ -> pure ()
  for_ mOwner \owner -> do
    findCard ((== def) . toCardDef) >>= traverse_ \card -> do
      deck <- field InvestigatorDeck owner
      obtainCard card
      when (any ((== card.id) . toCardId) deck.cards) $ shuffleDeck owner
    removeCampaignCardFromDeck owner def

oncePerCampaign :: ReverseQueue m => Text -> m () -> m ()
oncePerCampaign k body = unlessM getIsStandalone do
  stored @Bool k >>= \case
    Nothing -> do
      push $ SetGlobal CampaignTarget (Aeson.fromText k) (toJSON True)
      body
    Just _ -> pure ()

resign :: ReverseQueue m => InvestigatorId -> m ()
resign iid = push $ Resign iid

campaignSpecific :: (ToJSON a, ReverseQueue m) => Text -> a -> m ()
campaignSpecific key value = push $ CampaignSpecific key (toJSON value)

campaignSpecific_ :: ReverseQueue m => Text -> m ()
campaignSpecific_ key = push $ CampaignSpecific key Null

investigatorSpecific :: (ToJSON a, ReverseQueue m) => InvestigatorId -> Text -> a -> m ()
investigatorSpecific iid key value = push $ InvestigatorSpecific iid key (toJSON value)

investigatorSpecific_ :: ReverseQueue m => InvestigatorId -> Text -> m ()
investigatorSpecific_ iid key = push $ InvestigatorSpecific iid key Null

scenarioSpecific :: (ToJSON a, ReverseQueue m) => Text -> a -> m ()
scenarioSpecific key value = push $ ScenarioSpecific key (toJSON value)

scenarioSpecific_ :: ReverseQueue m => Text -> m ()
scenarioSpecific_ key = push $ ScenarioSpecific key Null

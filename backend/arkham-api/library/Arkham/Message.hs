{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -O0 -fomit-interface-pragmas -fno-specialise #-}

module Arkham.Message (module Arkham.Message, module X) where

import Arkham.Message.ChaosBag as X
import Arkham.Message.Clue as X
import Arkham.Message.Damage as X
import Arkham.Message.Defeat as X
import Arkham.Message.Doom as X
import Arkham.Message.Engage as X
import Arkham.Message.EnemyAttack as X
import Arkham.Message.Evade as X
import Arkham.Message.Exhaust as X
import Arkham.Message.Fight as X
import Arkham.Message.Horror as X
import Arkham.Message.Hunt as X
import Arkham.Message.Investigator as X
import Arkham.Message.Seal as X
import Arkham.Message.Search as X
import Arkham.Message.SkillTest as X
import Arkham.Message.Spawn as X
import Arkham.Message.Story as X
import Arkham.Message.Token as X
import Arkham.Message.Type as X
import Arkham.Question as X
import Arkham.SkillTest.Option as X
import Arkham.Strategy as X
import Arkham.Text as X

import Arkham.Ability.Types
import Arkham.Act.Sequence
import Arkham.Action hiding (Explore)
import Arkham.Action qualified as Action
import Arkham.Action.Additional
import Arkham.Agenda.Sequence
import Arkham.Asset.Uses
import Arkham.Attack.Types
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.EdgeOfTheEarth.Seal
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Campaigns.TheScarletKeys.Concealed.Types
import Arkham.Campaigns.TheScarletKeys.Key.Id
import Arkham.Card
import Arkham.Card.Settings
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken.Types
import Arkham.Choose
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.Customization
import Arkham.DamageEffect
import Arkham.Deck
import Arkham.DeckBuilding.Adjustment
import Arkham.Decklist.Type
import Arkham.Direction
import Arkham.Discard
import Arkham.Discover
import Arkham.Draw.Types
import {-# SOURCE #-} Arkham.Effect.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard.Source
import Arkham.Enemy.Creation
import Arkham.Exhaust
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Evade.Types
import Arkham.Exception
import Arkham.Field
import Arkham.Fight.Types
import Arkham.Game.State
import Arkham.Helpers
import Arkham.History
import Arkham.I18n
import Arkham.Id
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Key
import Arkham.Layout
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import {-# SOURCE #-} Arkham.Location.Types
import Arkham.Matcher hiding (AssetDefeated, DealtDamage, EnemyAttacks, EnemyEvaded, InvestigatorDefeated, RevealChaosToken)
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.Prelude
import Arkham.RequestedChaosTokenStrategy
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Options
import Arkham.ScenarioLogKey
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos.Types
import Arkham.Search
import {-# SOURCE #-} Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult qualified as SkillTest
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Spawn
import Arkham.Target
import Arkham.Tarot
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (Window, WindowType)
import Arkham.Xp
import Control.Monad.Fail
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.UUID (fromWords64, nil)
import Data.UUID qualified as UUID
import GHC.OverloadedLabels

messageType :: Message -> Maybe MessageType
messageType (PerformEnemyAttack _) = Just AttackMessage
messageType (After (PerformEnemyAttack _)) = Just AttackMessage
messageType Revelation {} = Just RevelationMessage
messageType DrawChaosToken {} = Just DrawChaosTokenMessage
messageType ResolveChaosToken {} = Just ResolveChaosTokenMessage
messageType EnemySpawn {} = Just EnemySpawnMessage
messageType EnemySpawnAtLocationMatching {} = Just EnemySpawnMessage
messageType InvestigatorDrawEnemy {} = Just DrawEnemyMessage
messageType Arkham.Message.Defeated {} = Just EnemyDefeatedMessage
messageType Arkham.Message.EnemyLocationDefeated {} = Just EnemyDefeatedMessage
messageType (Discard _ GameSource (EnemyTarget _)) = Just EnemyDefeatedMessage
messageType RevealChaosToken {} = Just RevealChaosTokenMessage
messageType InvestigatorDamage {} = Just DealDamageMessage
messageType InvestigatorDoAssignDamage {} = Just DealDamageMessage
messageType InvestigatorDrewEncounterCard {} = Just DrawEncounterCardMessage
messageType InvestigatorDefeated {} = Just InvestigatorDefeatedMessage
messageType InvestigatorIsDefeated {} = Just InvestigatorDefeatedMessage
messageType CheckWindows {} = Just CheckWindowMessage
messageType Explore {} = Just ExploreMessage
messageType DealAssetDamageWithCheck {} = Just AssetDamageMessage
messageType DealAssetDirectDamage {} = Just AssetDamageMessage
messageType AssignAssetDamageWithCheck {} = Just AssetDamageMessage
messageType (MoveWithSkillTest msg) = messageType msg
messageType (MovedWithSkillTest _ msg) = messageType msg
messageType (Do msg) = messageType msg
messageType (When msg) = messageType msg
messageType (After msg) = messageType msg
messageType _ = Nothing

isBlanked :: Message -> Bool
isBlanked Blanked {} = True
isBlanked _ = False

resolve :: Message -> [Message]
resolve msg = [When msg, msg, After msg]

story :: [PlayerId] -> FlavorText -> Message
story pids flavor =
  AskMap
    (mapFromList [(pid, Read flavor (BasicReadChoices [Label "$continue" []]) Nothing) | pid <- pids])

storyWithCards :: [CardDef] -> [PlayerId] -> FlavorText -> Message
storyWithCards cardDefs pids flavor =
  AskMap
    $ mapFromList
      [ (pid, Read flavor (BasicReadChoices [Label "$continue" []]) (Just $ map (.cardCode) cardDefs))
      | pid <- pids
      ]

storyWithChooseOne :: PlayerId -> [PlayerId] -> FlavorText -> [UI Message] -> Message
storyWithChooseOne lead pids flavor choices =
  AskMap
    ( mapFromList
        [(pid, Read flavor (BasicReadChoices $ if pid == lead then choices else []) Nothing) | pid <- pids]
    )

storyWithChooseN :: PlayerId -> [PlayerId] -> Int -> FlavorText -> [UI Message] -> Message
storyWithChooseN lead pids n flavor choices =
  AskMap
    ( mapFromList
        [ (pid, Read flavor (BasicReadChoicesN n $ if pid == lead then choices else []) Nothing) | pid <- pids
        ]
    )

storyWithChooseUpToN :: PlayerId -> [PlayerId] -> Int -> FlavorText -> [UI Message] -> Message
storyWithChooseUpToN lead pids n flavor choices =
  AskMap
    ( mapFromList
        [ (pid, Read flavor (BasicReadChoicesUpToN n $ if pid == lead then choices else []) Nothing)
        | pid <- pids
        ]
    )

data AdvancementMethod = AdvancedWithClues | AdvancedWithOther
  deriving stock (Generic, Ord, Eq, Show, Data)
  deriving anyclass (FromJSON, ToJSON)

instance IsLabel "clues" AdvancementMethod where
  fromLabel = AdvancedWithClues

instance IsLabel "other" AdvancementMethod where
  fromLabel = AdvancedWithOther

data AgendaAdvancementMethod = AgendaAdvancedWithDoom | AgendaAdvancedWithOther
  deriving stock (Generic, Ord, Eq, Show, Data)
  deriving anyclass (FromJSON, ToJSON)

instance IsLabel "doom" AgendaAdvancementMethod where
  fromLabel = AgendaAdvancedWithDoom

instance IsLabel "other" AgendaAdvancementMethod where
  fromLabel = AgendaAdvancedWithOther

class Is a b where
  is :: a -> b -> Bool

instance Sourceable a => Is a Source where
  is = isSource

instance Targetable a => Is a Target where
  is = isTarget

pattern AfterSkillTest :: Message -> Message
pattern AfterSkillTest msg <- Do (After msg)
  where
    AfterSkillTest msg = Do (After msg)

pattern LoseAll :: InvestigatorId -> Source -> Token -> Message
pattern LoseAll iid source token <- LoseTokens iid source token AllLost
  where
    LoseAll iid source token = LoseTokens iid source token AllLost

pattern LoseAllResources :: InvestigatorId -> Source -> Message
pattern LoseAllResources iid source <- LoseAll iid source Token.Resource
  where
    LoseAllResources iid source = LoseAll iid source Token.Resource

pattern MovedDamage :: Source -> Source -> Target -> Int -> Message
pattern MovedDamage source source' target n <- MoveTokens source source' target Token.Damage n
  where
    MovedDamage source source' target n = MoveTokens source source' target Token.Damage n

pattern MovedHorror :: Source -> Source -> Target -> Int -> Message
pattern MovedHorror source source' target n <- MoveTokens source source' target Token.Horror n
  where
    MovedHorror source source' target n = MoveTokens source source' target Token.Horror n

pattern MovedClues :: Source -> Source -> Target -> Int -> Message
pattern MovedClues source source' target n <- MoveTokens source source' target Token.Clue n
  where
    MovedClues source source' target n = MoveTokens source source' target Token.Clue n

pattern MoveUses :: Source -> Source -> Target -> UseType -> Int -> Message
pattern MoveUses source source' target useType' n <- MoveTokens source source' target useType' n
  where
    MoveUses source source' target useType' n = MoveTokens source source' target useType' n

pattern AdvanceAgenda :: AgendaId -> Message
pattern AdvanceAgenda aid <- AdvanceAgendaBy aid _

pattern UseThisAbility :: InvestigatorId -> Source -> Int -> Message
pattern UseThisAbility iid source n <- UseCardAbility iid source n _ _

pattern PassedSkillTestWithToken :: InvestigatorId -> ChaosTokenFace -> Message
pattern PassedSkillTestWithToken iid face <-
  PassedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> face)) _ _

pattern PassedThisSkillTest :: InvestigatorId -> Source -> Message
pattern PassedThisSkillTest iid source <-
  PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ _

pattern PassedThisSkillTestBy :: InvestigatorId -> Source -> Int -> Message
pattern PassedThisSkillTestBy iid source n <-
  PassedSkillTest iid _ source SkillTestInitiatorTarget {} _ n

pattern FailedThisSkillTest :: InvestigatorId -> Source -> Message
pattern FailedThisSkillTest iid source <-
  FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ _

pattern FailedThisSkillTestBy :: InvestigatorId -> Source -> Int -> Message
pattern FailedThisSkillTestBy iid source n <-
  FailedSkillTest iid _ source SkillTestInitiatorTarget {} _ n

pattern FailedSkillTestWithToken :: InvestigatorId -> ChaosTokenFace -> Message
pattern FailedSkillTestWithToken iid face <-
  FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> face)) _ _

pattern ElderSignEffect :: InvestigatorId -> Message
pattern ElderSignEffect iid <- ResolveChaosToken _ ElderSign iid

pattern PlaceClues :: Source -> Target -> Int -> Message
pattern PlaceClues source target n = PlaceTokens source target Clue n

pattern PlaceDoom :: Source -> Target -> Int -> Message
pattern PlaceDoom source target n = PlaceTokens source target Doom n

pattern PlaceResources :: Source -> Target -> Int -> Message
pattern PlaceResources source target n = PlaceTokens source target Token.Resource n

pattern PlaceDamage :: Source -> Target -> Int -> Message
pattern PlaceDamage source target n = PlaceTokens source target Token.Damage n

pattern RemoveDamage :: Source -> Target -> Int -> Message
pattern RemoveDamage source target n = RemoveTokens source target Token.Damage n

pattern PlaceHorror :: Source -> Target -> Int -> Message
pattern PlaceHorror source target n = PlaceTokens source target Horror n

pattern RemoveHorror :: Source -> Target -> Int -> Message
pattern RemoveHorror source target n = RemoveTokens source target Horror n

pattern RemoveClues :: Source -> Target -> Int -> Message
pattern RemoveClues source target n = RemoveTokens source target Clue n

pattern RemoveDoom :: Source -> Target -> Int -> Message
pattern RemoveDoom source target n = RemoveTokens source target Doom n

pattern RemoveResources :: Source -> Target -> Int -> Message
pattern RemoveResources source target n = RemoveTokens source target Token.Resource n

pattern CancelNext :: Source -> MessageType -> Message
pattern CancelNext source msgType = CancelEachNext Nothing source [msgType]

pattern CancelRevelation :: CardId -> Source -> Message
pattern CancelRevelation cid source = CancelEachNext (Just cid) source [RevelationMessage]

pattern PlayThisEvent :: InvestigatorId -> EventId -> Message
pattern PlayThisEvent iid eid <- InvestigatorPlayEvent iid eid _ _ _

getChoiceAmount :: Text -> [(NamedUUID, Int)] -> Int
getChoiceAmount key choices =
  let choicesMap = mapFromList @(Map Text Int) $ map (first nuName) choices
   in findWithDefault 0 key choicesMap

class IsMessage msg where
  toMessage :: msg -> Message

instance IsMessage StoryMessage where
  toMessage = StoryMessage
  {-# INLINE toMessage #-}

instance IsMessage Message where
  toMessage = id
  {-# INLINE toMessage #-}

instance IsMessage Movement where
  toMessage = Arkham.Message.Move
  {-# INLINE toMessage #-}

instance IsMessage EnemyAttackDetails where
  toMessage = InitiateEnemyAttack
  {-# INLINE toMessage #-}

instance IsMessage Investigate where
  toMessage = Arkham.Message.Investigate
  {-# INLINE toMessage #-}

instance IsMessage ChooseFight where
  toMessage = Arkham.Message.ChooseFightEnemy
  {-# INLINE toMessage #-}

instance IsMessage ChooseEvade where
  toMessage = Arkham.Message.ChooseEvadeEnemy
  {-# INLINE toMessage #-}

instance IsMessage (HandDiscard Message) where
  toMessage = DiscardFromHand
  {-# INLINE toMessage #-}

instance IsMessage (EnemyCreation Message) where
  toMessage = CreateEnemy
  {-# INLINE toMessage #-}

data ReplaceStrategy = DefaultReplace | Swap
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data IncludeDiscard = IncludeDiscard | ExcludeDiscard
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- FromSkillType / ToSkillType now live in "Arkham.Message.SkillTest" (re-exported above).

pattern FlipThis :: Target -> Message
pattern FlipThis target <- Flip _ _ target

pattern SuccessfulInvestigationWith :: InvestigatorId -> Target -> Message
pattern SuccessfulInvestigationWith iid target <- Successful (Action.Investigate, _) iid _ target _

pattern BeginSkillTest :: SkillTest -> Message
pattern BeginSkillTest skillTest <- BeginSkillTestWithPreMessages' [] skillTest
  where
    BeginSkillTest skillTest = BeginSkillTestWithPreMessages' [] skillTest

pattern DealAssetDamage :: AssetId -> Source -> Int -> Int -> Message
pattern DealAssetDamage aid source damage horror <- DealAssetDamageWithCheck aid source damage horror True
  where
    DealAssetDamage aid source damage horror = DealAssetDamageWithCheck aid source damage horror True

type IsSameAction = Bool

data CanAdvance = CanAdvance | CanNotAdvance
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

class AndThen a where
  andThen :: a -> Message -> a

instance AndThen (CardDraw Message) where
  andThen cd msg = cd {cardDrawAndThen = Just msg}

instance AndThen EnemyAttackDetails where
  andThen cd msg = cd {attackAfter = [msg]}

data ShuffleIn = ShuffleIn | DoNotShuffleIn
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data GroupKey = HunterGroup | FailSkillTestGroup
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass ToJSON

instance FromJSON GroupKey where
  parseJSON (Array _) = pure HunterGroup
  parseJSON v = flip (withText "GroupKey") v \case
    "HunterGroup" -> pure HunterGroup
    "FailSkillTestGroup" -> pure FailSkillTestGroup
    _ -> fail "Invalid GroupKey"

data AutoStatus = Auto | Manual | NoAutoStatus
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup AutoStatus where
  NoAutoStatus <> x = x
  x <> NoAutoStatus = x
  Auto <> _ = Auto
  _ <> Auto = Auto
  Manual <> Manual = Manual

data TokenLoss = AllLost | AllLostBut Int | Lose Int
  deriving stock (Show, Ord, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

-- SkillTestOption / SkillTestOptionKind / helpers now live in
-- "Arkham.SkillTest.Option" (re-exported above).

data Message
  = UseAbility InvestigatorId Ability [Window]
  | ResolvedAbility Ability -- INTERNAL, See Arbiter of Fates
  | SkillTestMessage SkillTestMessage
  | ChaosBagMessage ChaosBagMessage
  | SearchMessage SearchMessage
  | InvestigatorMessage InvestigatorMessage
  | ClearAbilityUse AbilityRef
  | UpdateGlobalSetting InvestigatorId SetGlobalSetting
  | UpdateCardSetting InvestigatorId CardCode SetCardSetting
  | SetAsIfAtIgnored InvestigatorId Bool
  | SetGameRunWindows Bool
  | SetGameState GameState
  | SetGlobal Target Aeson.Key Value
  | MoveWithSkillTest Message
  | MovedWithSkillTest SkillTestId Message
  | ClearInvestigators
  | SetInvestigator PlayerId Investigator
  | SetDriver AssetId InvestigatorId
  | IncreaseFloodLevel LocationId
  | DecreaseFloodLevel LocationId
  | SetFloodLevel LocationId FloodLevel
  | -- Skill Test Specific
    AddSubscriber Target
  | StoryMessage StoryMessage
  | -- Handle discard costs
    DiscardedCost Target
  | -- Act Deck Messages
    SetActDeck
  | SetActDeckCards Int [Card]
  | AddAct Int Card
  | AdvanceAct ActId Source AdvancementMethod
  | NextAdvanceActStep ActId Int
  | ReplaceAct ActId Card
  | RevertAct ActId
  | ResetActDeckToStage Int
  | AdvanceActDeck Int Source
  | AdvanceToAct Int CardDef ActSide Source
  | SetCurrentActDeck Int [Card]
  | -- Agenda Deck Messages
    SetAgendaDeck
  | SetAgendaDeckCards Int [Card]
  | AddAgenda Int Card
  | SetCurrentAgendaDeck Int [Card]
  | AdvanceAgendaBy AgendaId AgendaAdvancementMethod
  | AdvanceToAgenda Int CardDef AgendaSide Source
  | NextAdvanceAgendaStep AgendaId Int
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgendaDeck Int Source
  | AdvanceCurrentAgenda
  | ReplaceLocation LocationId Card ReplaceStrategy
  | ReplaceEnemy EnemyId Card ReplaceStrategy
  | ReplaceAgenda AgendaId Card
  | RevertAgenda AgendaId
  | ResetAgendaDeckToStage Int
  | -- No Remaining Investigator Messages
    SetNoRemainingInvestigatorsHandler Target
  | HandleNoRemainingInvestigators Target
  | CheckForRemainingInvestigators
  | AddDirectConnection LocationId LocationId
  | SetConnections LocationId [LocationMatcher]
  | SetFlippable LocationId Bool
  | AddCampaignCardToDeck InvestigatorId ShuffleIn Card
  | RemoveCardFromDeckForCampaign InvestigatorId CardId
  | -- Adding Cards to Hand
    AddFocusedToHand InvestigatorId Target Zone CardId
  | AddToHand InvestigatorId [Card]
  | DrawFocusedToHand InvestigatorId Target Zone CardId
  | DrawToHand InvestigatorId [Card]
  | DrawToHandFrom InvestigatorId DeckSignifier [Card]
  | ReturnToHand InvestigatorId Target
  | -- Adding Cards to Deck
    AddFocusedToTopOfDeck InvestigatorId Target CardId
  | -- Adding Cards to Player Discard
    AddToDiscard InvestigatorId PlayerCard
  | AddToEncounterDiscard EncounterCard
  | AddToSpecificEncounterDiscard ScenarioEncounterDeckKey EncounterCard
  | -- Slot Messages
    AddSlot InvestigatorId SlotType Slot
  | RemoveSlot InvestigatorId SlotType
  | RemoveSlotFrom InvestigatorId Source SlotType
  | -- Scenario Deck Messages
    AddToScenarioDeck ScenarioDeckKey Target
  | AddCardToScenarioDeck ScenarioDeckKey Card
  | ShuffleScenarioDeckIntoEncounterDeck ScenarioDeckKey
  | DrawStartingHand InvestigatorId
  | DrawCards InvestigatorId (CardDraw Message)
  | DoDrawCards InvestigatorId
  | DrawEnded CardDrawId InvestigatorId
  | Instead Message Message
  | ReplaceCurrentCardDraw InvestigatorId (CardDraw Message)
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
  | DrewCards InvestigatorId CardDrew
  | ChooseFrom InvestigatorId Choose
  | ChoseCards InvestigatorId Chosen
  | SetScenarioDeck ScenarioDeckKey [Card]
  | RemoveCardFromScenarioDeck ScenarioDeckKey Card
  | SwapPlaces (Target, LocationId) (Target, LocationId) -- we include the placement so it is up to date
  | -- Victory
    AddToVictory (Maybe InvestigatorId) Target
  | -- Tokens
    AddChaosToken ChaosTokenFace
  | -- Asset Uses
    AddUses Source AssetId UseType Int
  | -- Asks
    AskPlayer Message
  | Ask PlayerId (Question Message)
  | WindowAsk [Window] PlayerId (Question Message)
  | AskMap (Map PlayerId (Question Message))
  | After Message
  | EvadeMessage EvadeMessage
  | AfterRevelation InvestigatorId TreacheryId
  | AllCheckHandSize
  | AllDrawCardAndResource
  | AllDrawEncounterCard
  | AllInvestigatorsResigned
  | AllRandomDiscard Source CardMatcher
  | DealAssetDamageWithCheck AssetId Source Int Int Bool
  | DealAssetDirectDamage AssetId Source Int Int
  | AssignAssetDamageWithCheck AssetId Source Int Int Bool
  | DamageMessage DamageMessage
  | DefeatMessage DefeatMessage
  | ExhaustMessage ExhaustMessage
  | -- Attach
    AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AttachStoryTreacheryTo TreacheryId Card Target
  | FightMessage FightMessage
  | -- Game State Control
    BeginGame
  | Begin Phase
  | SetPhase Phase
  | Again Message -- if we repeat the investigation phase we need to reset actions
  | PhaseStep PhaseStep [Message]
  | BeginRound
  | BeginTrade InvestigatorId Source Target [InvestigatorId]
  | BeginTurn InvestigatorId
  | Blanked Message
  | HandleOption CampaignOption
  | RemoveOption CampaignOption
  | CampaignStep CampaignStep
  | ScenarioCampaignStep CampaignStep
  | CancelEachNext (Maybe CardId) Source [MessageType]
  | CancelSkillEffects -- used by scenarios to cancel skill cards
  | CancelDamage InvestigatorId Int
  | CancelAssetDamage AssetId Source Int
  | CheckAttackOfOpportunity InvestigatorId Bool (Maybe EnemyMatcher)
  | AssignDamage Target
  | CancelAssignedDamage Target Int Int
  | AssignedDamage Target Int Int
  | AssignedHealing Target
  | CheckHandSize InvestigatorId
  | CheckWindows [Window]
  | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
  | ChooseAndDiscardAsset InvestigatorId Source AssetMatcher
  | DiscardFromHand (HandDiscard Message)
  | DoneDiscarding InvestigatorId
  | DiscardCard InvestigatorId Source CardId
  | ChooseEndTurn InvestigatorId
  | EngageMessage EngageMessage
  | SpawnMessage SpawnMessage
  | HuntMessage HuntMessage
  | ClueMessage ClueMessage
  | DoomMessage DoomMessage
  | TokenMessage TokenMessage
  | SealMessage SealMessage
  | HorrorMessage HorrorMessage
  | ChooseLeadInvestigator
  | PreScenarioSetup
  | StandaloneSetup
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | SetPlayerOrder
  | ChoosePlayerOrder InvestigatorId [InvestigatorId] [InvestigatorId]
  | ChooseRandomLocation Target [LocationId]
  | ChosenRandomLocation Target LocationId
  | Continue Text
  | CreateEffect EffectBuilder
  | ObtainCard CardId
  | RemoveCard CardId
  | CreateEnemy (EnemyCreation Message)
  | CreateSkill SkillId Card InvestigatorId Placement
  | CreatedEnemyAt EnemyId LocationId Target
  | -- new payment bs
    PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | CancelCost ActiveCostId
  | SetCost ActiveCostId Cost
  | SetActiveCostChosenAction ActiveCostId Action
  | PaySealCost InvestigatorId CardId Cost
  | PayAdditionalCost InvestigatorId BatchId Cost
  | CheckAdditionalCosts ActiveCostId
  | PayCosts ActiveCostId
  | PayCost ActiveCostId InvestigatorId Bool Cost
  | PaidInitialCostForAbility ActiveCostId InvestigatorId AbilityRef Payment
  | PayCostFinished ActiveCostId
  | PaidCost ActiveCostId InvestigatorId (Maybe Action) Payment
  | PaidAllCosts
  | -- end  new payment bs
    CreateWindowModifierEffect EffectWindow (EffectMetadata Message) Source Target
  | CreateChaosTokenEffect (EffectMetadata Message) Source ChaosToken
  | CreateOnRevealChaosTokenEffect SkillTestId ChaosTokenMatcher Source Target [Message]
  | CreateOnSucceedByEffect SkillTestId ValueMatcher Source Target [Message]
  | CreateOnFailedByEffect SkillTestId ValueMatcher Source Target [Message]
  | CreateOnNextTurnEffect Source InvestigatorId [Message]
  | CreateEndOfTurnEffect Source InvestigatorId [Message]
  | CreateEndOfRoundEffect Source [Message]
  | CreateAssetAt AssetId Card Placement
  | CreateScarletKeyAt Card Placement
  | CreateEventAt InvestigatorId Card Placement
  | CreateTreacheryAt TreacheryId Card Placement
  | PlaceAsset AssetId Placement
  | PlaceScarletKey ScarletKeyId Placement
  | RemoveScarletKey ScarletKeyId
  | PlaceEvent EventId Placement
  | PlaceTreachery TreacheryId Placement
  | PlaceSkill SkillId Placement
  | PlaceKey Target ArkhamKey
  | PlaceSeal Target Seal
  | ActivateSeal SealKind
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateChaosTokenValueEffect SkillTestId Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Message)) Source Target
  | UpdateEffectMeta EffectId (EffectMetadata Message)
  | CrossOutRecord CampaignLogKey
  | SetCampaignLog CampaignLog
  | DeckHasNoCards InvestigatorId (Maybe Target)
  | DisableEffect EffectId
  | Discard (Maybe InvestigatorId) Source Target
  | DiscardHand InvestigatorId Source
  | RevealUntilFirst InvestigatorId Source DeckSignifier ExtendedCardMatcher
  | RevealedCards InvestigatorId Source DeckSignifier (Maybe Card) [Card]
  | DiscardUntilFirst InvestigatorId Source DeckSignifier ExtendedCardMatcher
  | DiscardUntilN Int InvestigatorId Source Target DeckSignifier ExtendedCardMatcher
  | DiscardTopOfDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeckWithDiscardedCards InvestigatorId Int Source (Maybe Target) [EncounterCard]
  | Discarded Target Source Card
  | DiscardedCards InvestigatorId Source Target [Card]
  | DiscardedCard CardId
  | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Source Target
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Source Target
  | DiscoverClues InvestigatorId Discover
  | DrawAnotherChaosToken InvestigatorId
  | RequestAnotherChaosToken InvestigatorId Source
  | DrewPlayerEnemy InvestigatorId Card
  | DrewTreachery InvestigatorId (Maybe DeckSignifier) Card
  | ResolveTreachery InvestigatorId TreacheryId
  | DrivenInsane InvestigatorId
  | EmptyDeck InvestigatorId (Maybe Message)
  | EndPhase
  | EndCheckWindow
  | EndEnemy
  | EndInvestigation
  | EndMythos
  | EndOfGame (Maybe CampaignStep)
  | Exile Target
  | Exiled Target Card
  | ScenarioResolution Resolution
  | ScenarioResolutionStep Int Resolution
  | EndOfScenario (Maybe CampaignStep)
  | EndRound
  | BeginRoundWindow
  | EndRoundWindow
  | EndSearch InvestigatorId Source Target [(Zone, ZoneReturnStrategy)]
  | EndTurn InvestigatorId
  | EndUpkeep
  | EnemyAttackMessage EnemyAttackMessage
  | PerformAction InvestigatorId Source Action
  | SetBearer Target InvestigatorId
  | SpawnEnemyAt Card LocationId
  | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
  | ChoseEnemy SkillTestId InvestigatorId Source EnemyId
  | FindAndDrawEncounterCard InvestigatorId CardMatcher IncludeDiscard
  | FindAndDrawEncounterCardWithDeckKey
      InvestigatorId
      CardMatcher
      IncludeDiscard
      ScenarioEncounterDeckKey
  | FindEncounterCard InvestigatorId Target [ScenarioZone] CardMatcher FindEncounterCardStrategy
  | FinishedWithMulligan InvestigatorId
  | FocusCards [Card]
  | FocusChaosTokens [ChaosToken]
  | Force Message
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | FoundEncounterCard InvestigatorId Target EncounterCard
  | FoundEncounterCardFrom InvestigatorId Target EncounterCardSource EncounterCard
  | FoundEnemyInOutOfPlay OutOfPlayZone InvestigatorId Target EnemyId
  | GainActions InvestigatorId Source Int
  | LoseAdditionalAction InvestigatorId AdditionalAction
  | UseEffectAction InvestigatorId EffectId [Window]
  | GainClues InvestigatorId Source Int
  | IgnoreGainXP CampaignStep
  | GainXP InvestigatorId Source Int
  | SetXP InvestigatorId Int
  | SpendXP InvestigatorId Int
  | GameOver
  | HandlePointOfFailure InvestigatorId Target Int -- Really do x n times, does not have to be failure
  | ApplyHealing Source
  | ReportXp XpBreakdown
  | HealAllDamageAndHorror Target Source
  | ExcessHealDamage InvestigatorId Source Int
  | HealDamageDelayed Target Source Int
  | HealHorrorDelayed Target Source Int
  | ReassignDamage Source Target Int
  | ReassignHorror Source Target Int
  | HealHorrorWithAdditional Target Source Int
  | AdditionalHealHorror Target Source Int
  | HealDamageDirectly Target Source Int
  | HealHorrorDirectly Target Source Int
  | InDiscard InvestigatorId Message -- Nothing uses this yet
  | InSearch Message
  | InHand InvestigatorId Message
  | InitDeck InvestigatorId (Maybe Text) (Deck PlayerCard) -- used to initialize the deck for the campaign
  | LoadSideDeck InvestigatorId [PlayerCard] -- used to initialize the side deck for the campaign
  | LoadDecklist PlayerId ArkhamDBDecklist
  | ReplaceInvestigator InvestigatorId ArkhamDBDecklist
  | UpgradeDeck InvestigatorId (Maybe Text) (Deck PlayerCard) -- used to upgrade deck during campaign
  | UpgradeDecklist InvestigatorId ArkhamDBDecklist
  | FinishedUpgradingDecks
  | Flip InvestigatorId Source Target
  | Flipped Source Card
  | InitiatePlayCardAsChoose
      InvestigatorId
      Card
      [Card]
      [Message]
      ChosenCardStrategy
      Payment
      [Window]
      Bool
  | InitiatePlayCardAs InvestigatorId Card Card [Message] ChosenCardStrategy Payment [Window] Bool
  | InitiatePlayCard InvestigatorId Card (Maybe Target) Payment [Window] Bool
  | InitiatePlayCardWithWindows InvestigatorId Card (Maybe Target) Payment [Window] Bool
  | -- | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
    CheckAdditionalActionCosts InvestigatorId Target Action [Message]
  | CheckAllAdditionalCommitCosts
  | CheckAdditionalCommitCosts InvestigatorId [Card]
  | -- Maybe Target is handler for success
    Investigate Investigate
  | UpdateEventMeta EventId Value
  | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
  | LookAtRevealed InvestigatorId Source Target
  | LookAtTopOfDeck InvestigatorId Target Int
  | LoseActions InvestigatorId Source Int
  | LoseResources InvestigatorId Source Int
  | LoseTokens InvestigatorId Source Token TokenLoss
  | SpendActions InvestigatorId Source [Action] Int
  | -- | Handles complex movement for a target, triggers Moves windows, and uses MoveFrom, MoveTo messages
    Move Movement
  | {- | When bool is True, This triggers the windows for PerformAction, as
    well as BeginAction and Finish action brackets.
    When bool is false it only triggers the after move action window, but
    also the (When, After) messages
    Eventually calls the Move message with a movement it built
    -}
    MoveAction InvestigatorId LocationId Cost Bool
  | -- | Only calls MoveTo for all investigators
    MoveAllTo Source LocationId
  | {- | Pretty useless, simply triggers Will/After variants that just push
    windows, possibly we could inline this, the only benefit seems to be the
    ability to cancel the batch easily
    -}
    MoveFrom Source InvestigatorId LocationId
  | -- | Actual movement, will add MovedBy, MovedBut, and after Entering windows
    MoveTo Movement
  | ResolveMovement InvestigatorId
  | SetMovement InvestigatorId Movement
  | ResolvedMovement InvestigatorId MovementId
  | WhenCanMove InvestigatorId [Message]
  | MoveIgnored InvestigatorId
  | MoveTopOfDeckToBottom Source DeckSignifier Int
  | NextCampaignStep (Maybe CampaignStep)
  | NextScenarioCampaignStep (Maybe CampaignStep)
  | Noop
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId Card [Window]
  | PaidForCardCost InvestigatorId Card Payment
  | PayForCardAbility InvestigatorId Source [Window] Int Payment
  | PlaceDoomOnAgenda Int CanAdvance
  | PlaceEnemyOutOfPlay OutOfPlayZone EnemyId
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Card
  | PlaceLocationWith LocationId Card (Update Location)
  | PlaceLocationMatching CardMatcher
  | PlaceEnemyLocation LocationId Card
  | FlipToEnemyLocation LocationId Card
  | FlipToLocation LocationId Card
  | RemoveEnemyLocation LocationId
  | PlaceUnderneath Target [Card]
  | PlacedUnderneath Target Card
  | PlaceNextTo Target [Card]
  | PlacedLocation Name CardCode LocationId
  | PlacedLocationDirection LocationId Direction LocationId
  | LocationMoved LocationId
  | PlayCard InvestigatorId Card (Maybe Target) Payment [Window] Bool
  | CardEnteredPlay InvestigatorId Card
  | CardIsEnteringPlay InvestigatorId Card
  | ResolvedCard InvestigatorId Card
  | ResolvedPlayCard InvestigatorId Card
  | PlayerWindow InvestigatorId [UI Message] Bool Bool
  | PutCampaignCardIntoPlay InvestigatorId CardDef
  | PutCardIntoPlayById InvestigatorId CardId (Maybe Target) Payment [Window]
  | PutCardIntoPlay InvestigatorId Card (Maybe Target) Payment [Window]
  | PutCardIntoPlayWithAdditionalCosts InvestigatorId Card (Maybe Target) Payment [Window]
  | PutCardOnTopOfDeck InvestigatorId DeckSignifier Card
  | PutOnTopOfDeck InvestigatorId DeckSignifier Target
  | PutCardOnBottomOfDeck InvestigatorId DeckSignifier Card
  | PutOnBottomOfDeck InvestigatorId DeckSignifier Target
  | Record CampaignLogKey
  | RecordForInvestigator InvestigatorId CampaignLogKey
  | RecordCount CampaignLogKey Int
  | IncrementRecordCount CampaignLogKey Int
  | DecrementRecordCount CampaignLogKey Int
  | RecordSetInsert CampaignLogKey [SomeRecorded]
  | RecordSetReplace CampaignLogKey SomeRecorded SomeRecorded
  | CrossOutRecordSetEntries CampaignLogKey [SomeRecorded]
  | RefillSlots InvestigatorId [AssetId]
  | Remember ScenarioLogKey
  | Forget ScenarioLogKey
  | ScenarioCountSet ScenarioCountKey Int
  | ScenarioCountIncrementBy ScenarioCountKey Int
  | ScenarioCountDecrementBy ScenarioCountKey Int
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemoveAllCopiesOfEncounterCardFromGame CardMatcher
  | RemovePlayerCardFromGame Bool Card
  | RemoveAllAttachments Source Target
  | RemoveCampaignCardFromDeck InvestigatorId CardDef
  | RemoveCardFromHand InvestigatorId CardId
  | RemoveCardFromSearch InvestigatorId CardId
  | RemoveDiscardFromGame InvestigatorId
  | -- | Remove the entity identified by Target from play. The six entity-specific
    -- back-compat pattern synonyms (RemoveAsset, RemoveEnemy, RemoveEvent,
    -- RemoveSkill, RemoveTreachery, RemoveLocation) wrap this with the right
    -- per-entity Target constructor.
    Remove Target
  | RemoveFromDiscard InvestigatorId CardId
  | RemoveFromEncounterDiscard EncounterCard
  | RemoveFromEncounterDeck EncounterCard
  | RemoveFromGame Target
  | QuietlyRemoveFromGame Target
  | RemoveCompletedActFromGame Int ActId
  | RemovedFromGame Card
  | RemovedLocation LocationId
  | SetCardAside Card
  | PlaceInvestigator InvestigatorId Placement
  | PlaceInBonded InvestigatorId Card
  | CreateInBonded InvestigatorId CardCode
  | RemoveFromPlay Source
  | RemovedFromPlay Source
  | SlotSourceRemovedFromPlay Source
  | RequestedEncounterCard Source (Maybe InvestigatorId) (Maybe EncounterCard)
  | RequestedEncounterCards Target [EncounterCard]
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard) [PlayerCard]
  | RequestedSetAsideCard Source Card
  | ResetInvestigators
  | ResetGame
  | ReloadDecks
  | Reset Target
  | ResolveAmounts InvestigatorId [(NamedUUID, Int)] Target
  | ResolveEvent InvestigatorId EventId (Maybe Target) [Window]
  | ResolveEventChoice InvestigatorId EventId Int (Maybe Target) [Window]
  | ResolveSkill SkillId
  | RevealCard CardId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | UnrevealLocation LocationId
  | Revelation InvestigatorId Source
  | RevelationSkipped InvestigatorId Source
  | RevelationChoice InvestigatorId Source Int
  | ResolveRevelation InvestigatorId Card
  | Run [Message]
  | RemoveFromBearersDeckOrDiscard PlayerCard
  | SetActions InvestigatorId Source Int
  | SetEncounterDeck (Deck EncounterCard)
  | SetLayout [GridTemplateRow]
  | SetDecksLayout [GridTemplateRow]
  | SetLocationLabel LocationId Text
  | SetActiveInvestigator InvestigatorId
  | SetActivePlayer PlayerId
  | Setup
  | EndSetup
  | SetupInvestigators
  | SetupInvestigator InvestigatorId
  | SetupStep Target Int
  | ShuffleAllFocusedIntoDeck InvestigatorId Target
  | PutAllFocusedIntoDiscard InvestigatorId Target
  | ShuffleAllInEncounterDiscardBackIn CardCode
  | ShuffleBackIntoEncounterDeck Source Target
  | ShuffleCardsIntoDeck DeckSignifier [Card]
  | ShuffleDiscardBackIn InvestigatorId
  | ShuffleEncounterDiscardBackIn
  | ShuffleEncounterDiscardBackInByKey ScenarioEncounterDeckKey
  | ShuffleDeck DeckSignifier
  | ShuffleIntoDeck DeckSignifier Target
  | ShuffleCardsIntoTopOfDeck DeckSignifier Int [Card]
  | SpendClues Int [InvestigatorId]
  | SpendResources InvestigatorId Int
  | SpendUses Source Target UseType Int
  | SpentAllUses Target
  | StartCampaign
  | StartScenario ScenarioId (Maybe ScenarioOptions)
  | LoadScenario ScenarioOptions
  | RestartScenario
  | SkippedWindow InvestigatorId
  | SufferTrauma InvestigatorId Int Int
  | SetTrauma InvestigatorId Int Int
  | CheckTrauma InvestigatorId
  | HealTrauma InvestigatorId Int Int
  | GainSurge Source Target
  | CancelSurge Source
  | Surge InvestigatorId Source
  | TakeActions InvestigatorId [Action] Cost
  | TakeControlOfAsset InvestigatorId AssetId
  | LoseControlOfAsset AssetId
  | ReplaceInvestigatorAsset InvestigatorId AssetId Card
  | ReplaceAsset AssetId CardDef
  | ReplacedInvestigatorAsset InvestigatorId AssetId
  | TakeControlOfSetAsideAsset InvestigatorId Card
  | SetAsideCards [Card]
  | TakeResources InvestigatorId Int Source Bool
  | DrawStartingHands
  | TakeStartingResources InvestigatorId
  | TakenActions InvestigatorId [Action]
  | PerformedActions InvestigatorId [Action]
  | UnfocusCards
  | UnfocusChaosTokens
  | SetActiveCard Card
  | UnsetActiveCard
  | AddCardEntity UUID Card
  | RemoveCardEntity UUID Card
  | UseCardAbility InvestigatorId Source Int [Window] Payment
  | UseCardAbilityStep InvestigatorId Source Int [Window] Payment Int -- todo eliminated in favor of DoStep
  | UseCardAbilityChoice InvestigatorId Source Int AbilityMetadata [Window] Payment
  | UseCardAbilityChoiceTarget InvestigatorId Source Int Target [Window] Payment
  | HandleTargetChoice InvestigatorId Source Target
  | HandleAbilityOption InvestigatorId Source Int
  | ResetMetadata Target
  | DoNotCountUseTowardsAbilityLimit InvestigatorId Ability
  | When Message
  | Would BatchId [Message]
  | CancelBatch BatchId
  | IgnoreBatch BatchId
  | WhenWillEnterLocation InvestigatorId LocationId
  | EnterLocation InvestigatorId LocationId
  | Will Message
  | -- must be called on instance directly
    SetOriginalCardCode CardCode
  | -- Time Warp
    ActionCannotBeUndone
  | UndoAction
  | BeginAction
  | FinishAction
  | ReplaceCard CardId Card
  | UpdateHistory InvestigatorId HistoryItem
  | -- The Forgotten Age
    PickSupply InvestigatorId Supply
  | UseSupply InvestigatorId Supply
  | Explore InvestigatorId Source CardMatcher
  | BecomeYithian InvestigatorId
  | BecomeHomunculus InvestigatorId
  | SetScenarioMeta Value
  | ScenarioSpecific Text Value
  | CampaignSpecific Text Value
  | SetCampaignMeta Value
  | DoStep Int Message
  | ForInvestigator InvestigatorId Message
  | ForInvestigators [InvestigatorId] Message
  | ForTrait Trait Message
  | ForAction Action Message
  | ForActions [Action] Message
  | ForTarget Target Message
  | ForTargets [Target] Message
  | ForPlayer PlayerId Message
  | ForChoice Int Message
  | ForSkillType SkillType Message
  | -- The Circle Undone
    PutLocationInFrontOf InvestigatorId LocationId
  | PutLocationInCenter LocationId
  | PlaceBreaches Target Int
  | RemoveBreaches Target Int
  | RunCosmos InvestigatorId LocationId [Message]
  | PlaceCosmos InvestigatorId LocationId (CosmosLocation Card LocationId)
  | PlaceGrid GridLocation
  | LoadTarotDeck
  | PerformTarotReading
  | SetPerformTarotReadings Bool
  | PerformReading TarotReading
  | DrawAndChooseTarot InvestigatorId TarotCardFacing Int
  | PlaceTarot InvestigatorId TarotCard
  | FocusTarotCards [TarotCard]
  | UnfocusTarotCards
  | RotateTarot TarotCard
  | SetDestiny (Map Scope TarotCard)
  | CheckDestiny
  | RunDestiny
  | ResolveDestiny TarotCard
  | Incursion LocationId
  | SetInvestigatorForm InvestigatorId InvestigatorForm
  | PlaceReferenceCard Target CardCode
  | -- The Dream Eaters
    PlaceSwarmCards InvestigatorId EnemyId Int
  | PlacedSwarmCard EnemyId Card
  | UpdateLocation LocationId (Update Location)
  | UpdateEnemy EnemyId (Update Enemy)
  | If WindowType [Message]
  | SendMessage Target Message
  | IfEnemyExists EnemyMatcher [Message]
  | IfCardExists ExtendedCardMatcher [Message]
  | ExcessDamage EnemyId [Message]
  | AddDeckBuildingAdjustment InvestigatorId DeckBuildingAdjustment
  | IncreaseCustomization InvestigatorId CardCode Customization [CustomizationChoice]
  | ChoosingDecks
  | UpgradingDecks
  | DoneChoosingDecks
  | DoneUpgradingDecks
  | SetPartnerStatus CardCode PartnerStatus
  | HandleGroupTarget GroupKey Target [Message]
  | HandleGroupTargets AutoStatus GroupKey (Map Target [Message])
  | KonamiCode PlayerId
  | CreateConcealedCard ConcealedCard
  | PlaceConcealedCards InvestigatorId [ConcealedCardId] [LocationId]
  | PlaceConcealedCard InvestigatorId ConcealedCardId Placement
  | RemoveAllConcealed
  | SetLocationOutOfGame LocationId
  | ReturnLocationToGame LocationId
  | -- Commit
    Do Message
  | DoBatch BatchId Message
  | -- UI
    ClearUI
  | Priority Message
  | Simultaneously [Message]
  | -- Debug
    ClearQueue
  | DebugAddToHand InvestigatorId CardId
  | SetCampaignStep CampaignStep
  | CreateCard CardId CardCode
  deriving stock (Show, Eq, Ord, Data)

-- | The fight cluster routes by 'Target' internally. Two public forms exist
-- per constructor: a generic @*Target@ form that matches any 'Target', and an
-- @*Enemy@ back-compat form that matches only @EnemyTarget eid@. Old call
-- sites that say @FightEnemy eid cf@ continue to compile unchanged.
pattern FightTarget :: Target -> ChooseFight -> Message
pattern FightTarget tgt cf = FightMessage (FightTarget_ tgt cf)

pattern AttackTarget :: Target -> ChooseFight -> Message
pattern AttackTarget tgt cf = FightMessage (AttackTarget_ tgt cf)

pattern FailedAttackTarget :: InvestigatorId -> Target -> Message
pattern FailedAttackTarget iid tgt = FightMessage (FailedAttackTarget_ iid tgt)

pattern FightEnemy :: EnemyId -> ChooseFight -> Message
pattern FightEnemy eid cf = FightMessage (FightTarget_ (EnemyTarget eid) cf)

pattern AttackEnemy :: EnemyId -> ChooseFight -> Message
pattern AttackEnemy eid cf = FightMessage (AttackTarget_ (EnemyTarget eid) cf)

pattern FailedAttackEnemy :: InvestigatorId -> EnemyId -> Message
pattern FailedAttackEnemy iid eid = FightMessage (FailedAttackTarget_ iid (EnemyTarget eid))

pattern ChooseFightEnemy :: ChooseFight -> Message
pattern ChooseFightEnemy cf = FightMessage (ChooseFightEnemy_ cf)

-- | The evade cluster routes by 'Target' internally. Two public forms exist
-- per constructor: a generic @*Target@ form that matches any 'Target', and an
-- @*Enemy@/@Enemy*@ back-compat form that matches only @EnemyTarget eid@.
pattern ChooseEvadeEnemy :: ChooseEvade -> Message
pattern ChooseEvadeEnemy ce = EvadeMessage (ChooseEvadeEnemy_ ce)

pattern TryEvadeTarget :: SkillTestId -> InvestigatorId -> Target -> Source -> Maybe Target -> SkillType -> Message
pattern TryEvadeTarget sid iid tgt src mt st = EvadeMessage (TryEvadeTarget_ sid iid tgt src mt st)

pattern EvadeTarget :: SkillTestId -> InvestigatorId -> Target -> Source -> Maybe Target -> SkillType -> Bool -> Message
pattern EvadeTarget sid iid tgt src mt st b = EvadeMessage (EvadeTarget_ sid iid tgt src mt st b)

pattern EvadedTarget :: InvestigatorId -> Target -> Message
pattern EvadedTarget iid tgt = EvadeMessage (EvadedTarget_ iid tgt)

pattern ChosenEvadeTarget :: SkillTestId -> Source -> Target -> Message
pattern ChosenEvadeTarget sid src tgt = EvadeMessage (ChosenEvadeTarget_ sid src tgt)

pattern AfterEvadeTarget :: InvestigatorId -> Target -> Message
pattern AfterEvadeTarget iid tgt = EvadeMessage (AfterEvadeTarget_ iid tgt)

pattern TryEvadeEnemy :: SkillTestId -> InvestigatorId -> EnemyId -> Source -> Maybe Target -> SkillType -> Message
pattern TryEvadeEnemy sid iid eid src mt st = EvadeMessage (TryEvadeTarget_ sid iid (EnemyTarget eid) src mt st)

pattern EvadeEnemy :: SkillTestId -> InvestigatorId -> EnemyId -> Source -> Maybe Target -> SkillType -> Bool -> Message
pattern EvadeEnemy sid iid eid src mt st b = EvadeMessage (EvadeTarget_ sid iid (EnemyTarget eid) src mt st b)

pattern EnemyEvaded :: InvestigatorId -> EnemyId -> Message
pattern EnemyEvaded iid eid = EvadeMessage (EvadedTarget_ iid (EnemyTarget eid))

pattern ChosenEvadeEnemy :: SkillTestId -> Source -> EnemyId -> Message
pattern ChosenEvadeEnemy sid src eid = EvadeMessage (ChosenEvadeTarget_ sid src (EnemyTarget eid))

pattern AfterEvadeEnemy :: InvestigatorId -> EnemyId -> Message
pattern AfterEvadeEnemy iid eid = EvadeMessage (AfterEvadeTarget_ iid (EnemyTarget eid))

-- Bidirectional pattern synonyms preserving the public API for SkillTestMessage.
pattern AbilityIsSkillTest :: AbilityRef -> Message
pattern AbilityIsSkillTest a = SkillTestMessage (AbilityIsSkillTest_ a)

pattern CollectSkillTestOptions :: Message
pattern CollectSkillTestOptions = SkillTestMessage CollectSkillTestOptions_

pattern NextSkillTest :: SkillTestId -> Message
pattern NextSkillTest sid = SkillTestMessage (NextSkillTest_ sid)

pattern BeforeSkillTest :: SkillTestId -> Message
pattern BeforeSkillTest sid = SkillTestMessage (BeforeSkillTest_ sid)

pattern ChangeSkillTestType :: SkillTestType -> SkillTestBaseValue -> Message
pattern ChangeSkillTestType t b = SkillTestMessage (ChangeSkillTestType_ t b)

pattern IncreaseSkillTestDifficulty :: Int -> Message
pattern IncreaseSkillTestDifficulty n = SkillTestMessage (IncreaseSkillTestDifficulty_ n)

pattern ReplaceSkillTestSkill :: FromSkillType -> ToSkillType -> Message
pattern ReplaceSkillTestSkill f t = SkillTestMessage (ReplaceSkillTestSkill_ f t)

pattern RepeatSkillTest :: SkillTestId -> SkillTestId -> Message
pattern RepeatSkillTest a b = SkillTestMessage (RepeatSkillTest_ a b)

pattern SetSkillTestTarget :: Target -> Message
pattern SetSkillTestTarget tgt = SkillTestMessage (SetSkillTestTarget_ tgt)

pattern SetSkillTestResolveFailureInvestigator :: InvestigatorId -> Message
pattern SetSkillTestResolveFailureInvestigator iid = SkillTestMessage (SetSkillTestResolveFailureInvestigator_ iid)

pattern BeginSkillTestWithPreMessages :: Bool -> [Message] -> SkillTest -> Message
pattern BeginSkillTestWithPreMessages b ms st = SkillTestMessage (BeginSkillTestWithPreMessages_ b ms st)

pattern BeginSkillTestWithPreMessages' :: [Message] -> SkillTest -> Message
pattern BeginSkillTestWithPreMessages' ms st = SkillTestMessage (BeginSkillTestWithPreMessages'_ ms st)

pattern BeginSkillTestAfterFast :: Message
pattern BeginSkillTestAfterFast = SkillTestMessage BeginSkillTestAfterFast_

pattern CommitCard :: InvestigatorId -> Card -> Message
pattern CommitCard iid c = SkillTestMessage (CommitCard_ iid c)

pattern CommitToSkillTest :: SkillTestId -> UI Message -> Message
pattern CommitToSkillTest sid ui = SkillTestMessage (CommitToSkillTest_ sid ui)

pattern FailSkillTest :: Message
pattern FailSkillTest = SkillTestMessage FailSkillTest_

pattern FailedSkillTest :: InvestigatorId -> Maybe Action -> Source -> Target -> SkillTestType -> Int -> Message
pattern FailedSkillTest iid ma src tgt stt n = SkillTestMessage (FailedSkillTest_ iid ma src tgt stt n)

pattern PassSkillTest :: Message
pattern PassSkillTest = SkillTestMessage PassSkillTest_

pattern PassSkillTestBy :: Int -> Message
pattern PassSkillTestBy n = SkillTestMessage (PassSkillTestBy_ n)

pattern PassedSkillTest :: InvestigatorId -> Maybe Action -> Source -> Target -> SkillTestType -> Int -> Message
pattern PassedSkillTest iid ma src tgt stt n = SkillTestMessage (PassedSkillTest_ iid ma src tgt stt n)

pattern RerunSkillTest :: Message
pattern RerunSkillTest = SkillTestMessage RerunSkillTest_

pattern RevelationSkillTest :: SkillTestId -> InvestigatorId -> Source -> SkillType -> SkillTestDifficulty -> Message
pattern RevelationSkillTest sid iid src st diff = SkillTestMessage (RevelationSkillTest_ sid iid src st diff)

pattern RunSkillTest :: InvestigatorId -> Message
pattern RunSkillTest iid = SkillTestMessage (RunSkillTest_ iid)

pattern AfterThisTestResolves :: SkillTestId -> [Message] -> Message
pattern AfterThisTestResolves sid ms = SkillTestMessage (AfterThisTestResolves_ sid ms)

pattern StartSkillTest :: InvestigatorId -> Message
pattern StartSkillTest iid = SkillTestMessage (StartSkillTest_ iid)

pattern Successful :: (Action, Target) -> InvestigatorId -> Source -> Target -> Int -> Message
pattern Successful at iid src tgt n = SkillTestMessage (Successful_ at iid src tgt n)

pattern Failed :: (Action, Target) -> InvestigatorId -> Source -> Target -> Int -> Message
pattern Failed at iid src tgt n = SkillTestMessage (Failed_ at iid src tgt n)

pattern TriggerSkillTest :: InvestigatorId -> Message
pattern TriggerSkillTest iid = SkillTestMessage (TriggerSkillTest_ iid)

pattern SkillTestResultOption :: SkillTestOption -> Message
pattern SkillTestResultOption sto = SkillTestMessage (SkillTestResultOption_ sto)

pattern SkillTestResultOptions :: [SkillTestOption] -> Message
pattern SkillTestResultOptions stos = SkillTestMessage (SkillTestResultOptions_ stos)

pattern RecalculateSkillTestResults :: Message
pattern RecalculateSkillTestResults = SkillTestMessage RecalculateSkillTestResults_

pattern RecalculateSkillTestResultsCanChangeAutomatic :: Bool -> Message
pattern RecalculateSkillTestResultsCanChangeAutomatic b = SkillTestMessage (RecalculateSkillTestResultsCanChangeAutomatic_ b)

pattern SkillTestApplyResults :: Message
pattern SkillTestApplyResults = SkillTestMessage SkillTestApplyResults_

pattern SkillTestApplyResultsAfter :: Message
pattern SkillTestApplyResultsAfter = SkillTestMessage SkillTestApplyResultsAfter_

pattern SkillTestAsk :: Message -> Message
pattern SkillTestAsk m = SkillTestMessage (SkillTestAsk_ m)

pattern SkillTestCommitCard :: InvestigatorId -> Card -> Message
pattern SkillTestCommitCard iid c = SkillTestMessage (SkillTestCommitCard_ iid c)

pattern SkillTestEnds :: SkillTestId -> InvestigatorId -> Source -> Message
pattern SkillTestEnds sid iid src = SkillTestMessage (SkillTestEnds_ sid iid src)

pattern SkillTestEnded :: SkillTestId -> Message
pattern SkillTestEnded sid = SkillTestMessage (SkillTestEnded_ sid)

pattern AfterSkillTestEnds :: Source -> Target -> SkillTest.SkillTestResult -> Message
pattern AfterSkillTestEnds src tgt r = SkillTestMessage (AfterSkillTestEnds_ src tgt r)

pattern SkillTestResults :: SkillTestResultsData -> Message
pattern SkillTestResults d = SkillTestMessage (SkillTestResults_ d)

pattern SkillTestUncommitCard :: InvestigatorId -> Card -> Message
pattern SkillTestUncommitCard iid c = SkillTestMessage (SkillTestUncommitCard_ iid c)

pattern AfterSkillTestQuiet :: [Message] -> Message
pattern AfterSkillTestQuiet ms = SkillTestMessage (AfterSkillTestQuiet_ ms)

pattern AfterSkillTestOption :: InvestigatorId -> Text -> [Message] -> Message
pattern AfterSkillTestOption iid t ms = SkillTestMessage (AfterSkillTestOption_ iid t ms)

pattern EndSkillTestWindow :: Message
pattern EndSkillTestWindow = SkillTestMessage EndSkillTestWindow_

pattern ReturnSkillTestRevealedChaosTokens :: Message
pattern ReturnSkillTestRevealedChaosTokens = SkillTestMessage ReturnSkillTestRevealedChaosTokens_

pattern RevealSkillTestChaosTokens :: InvestigatorId -> Message
pattern RevealSkillTestChaosTokens iid = SkillTestMessage (RevealSkillTestChaosTokens_ iid)

pattern RevealSkillTestChaosTokensAgain :: InvestigatorId -> Message
pattern RevealSkillTestChaosTokensAgain iid = SkillTestMessage (RevealSkillTestChaosTokensAgain_ iid)

-- Bidirectional pattern synonyms preserving the public API for ChaosBagMessage.
pattern DrawChaosToken :: InvestigatorId -> ChaosToken -> Message
pattern DrawChaosToken iid t = ChaosBagMessage (DrawChaosToken_ iid t)

pattern ResolveChaosToken :: ChaosToken -> ChaosTokenFace -> InvestigatorId -> Message
pattern ResolveChaosToken t f iid = ChaosBagMessage (ResolveChaosToken_ t f iid)

pattern TargetResolveChaosToken :: Target -> ChaosToken -> ChaosTokenFace -> InvestigatorId -> Message
pattern TargetResolveChaosToken tgt t f iid = ChaosBagMessage (TargetResolveChaosToken_ tgt t f iid)

pattern RevealChaosToken :: Source -> InvestigatorId -> ChaosToken -> Message
pattern RevealChaosToken src iid t = ChaosBagMessage (RevealChaosToken_ src iid t)

pattern SilentRevealChaosToken :: Source -> InvestigatorId -> ChaosToken -> Message
pattern SilentRevealChaosToken src iid t = ChaosBagMessage (SilentRevealChaosToken_ src iid t)

pattern SwapChaosToken :: ChaosTokenFace -> ChaosTokenFace -> Message
pattern SwapChaosToken a b = ChaosBagMessage (SwapChaosToken_ a b)

pattern RemoveChaosToken :: ChaosTokenFace -> Message
pattern RemoveChaosToken f = ChaosBagMessage (RemoveChaosToken_ f)

pattern ResetTokenPool :: Message
pattern ResetTokenPool = ChaosBagMessage ResetTokenPool_

pattern RequestChaosTokens :: Source -> Maybe InvestigatorId -> RevealStrategy -> RequestedChaosTokenStrategy -> Message
pattern RequestChaosTokens src miid rs rts = ChaosBagMessage (RequestChaosTokens_ src miid rs rts)

pattern RequestedChaosTokens :: Source -> Maybe InvestigatorId -> [ChaosToken] -> Message
pattern RequestedChaosTokens src miid ts = ChaosBagMessage (RequestedChaosTokens_ src miid ts)

pattern ReturnChaosTokens :: [ChaosToken] -> Message
pattern ReturnChaosTokens ts = ChaosBagMessage (ReturnChaosTokens_ ts)

pattern ReturnChaosTokensToPool :: [ChaosToken] -> Message
pattern ReturnChaosTokensToPool ts = ChaosBagMessage (ReturnChaosTokensToPool_ ts)

pattern RunBag :: Source -> Maybe InvestigatorId -> RequestedChaosTokenStrategy -> Message
pattern RunBag src miid rts = ChaosBagMessage (RunBag_ src miid rts)

pattern RunDrawFromBag :: Source -> Maybe InvestigatorId -> RequestedChaosTokenStrategy -> Message
pattern RunDrawFromBag src miid rts = ChaosBagMessage (RunDrawFromBag_ src miid rts)

pattern FinalizeRequestedChaosTokens :: Source -> Maybe InvestigatorId -> Message
pattern FinalizeRequestedChaosTokens src miid = ChaosBagMessage (FinalizeRequestedChaosTokens_ src miid)

pattern NextChaosBagStep :: Source -> Maybe InvestigatorId -> RequestedChaosTokenStrategy -> Message
pattern NextChaosBagStep src miid rts = ChaosBagMessage (NextChaosBagStep_ src miid rts)

pattern ChooseChaosTokenGroups :: Source -> InvestigatorId -> ChaosBagStep -> Message
pattern ChooseChaosTokenGroups src iid step = ChaosBagMessage (ChooseChaosTokenGroups_ src iid step)

pattern BeforeRevealChaosTokens :: Message
pattern BeforeRevealChaosTokens = ChaosBagMessage BeforeRevealChaosTokens_

pattern AfterRevealChaosTokens :: Message
pattern AfterRevealChaosTokens = ChaosBagMessage AfterRevealChaosTokens_

pattern ReplaceCurrentDraw :: Source -> InvestigatorId -> ChaosBagStep -> Message
pattern ReplaceCurrentDraw src iid step = ChaosBagMessage (ReplaceCurrentDraw_ src iid step)

pattern ReplaceEntireDraw :: Source -> InvestigatorId -> ChaosBagStep -> Message
pattern ReplaceEntireDraw src iid step = ChaosBagMessage (ReplaceEntireDraw_ src iid step)

pattern SetChaosBagChoice :: Source -> InvestigatorId -> ChaosBagStep -> Message
pattern SetChaosBagChoice src iid step = ChaosBagMessage (SetChaosBagChoice_ src iid step)

pattern ResetChaosTokens :: Source -> Message
pattern ResetChaosTokens src = ChaosBagMessage (ResetChaosTokens_ src)

pattern ChaosTokenSelected :: InvestigatorId -> Source -> ChaosToken -> Message
pattern ChaosTokenSelected iid src t = ChaosBagMessage (ChaosTokenSelected_ iid src t)

pattern ChaosTokenIgnored :: InvestigatorId -> Source -> ChaosToken -> Message
pattern ChaosTokenIgnored iid src t = ChaosBagMessage (ChaosTokenIgnored_ iid src t)

pattern ChaosTokenCanceled :: InvestigatorId -> Source -> ChaosToken -> Message
pattern ChaosTokenCanceled iid src t = ChaosBagMessage (ChaosTokenCanceled_ iid src t)

pattern ForceChaosTokenDraw :: ChaosTokenFace -> Message
pattern ForceChaosTokenDraw f = ChaosBagMessage (ForceChaosTokenDraw_ f)

pattern ForceChaosTokenDrawToken :: ChaosToken -> Message
pattern ForceChaosTokenDrawToken t = ChaosBagMessage (ForceChaosTokenDrawToken_ t)

pattern SetChaosTokens :: [ChaosTokenFace] -> Message
pattern SetChaosTokens fs = ChaosBagMessage (SetChaosTokens_ fs)

pattern SetChaosTokensForScenario :: Message
pattern SetChaosTokensForScenario = ChaosBagMessage SetChaosTokensForScenario_

pattern ObtainChaosToken :: ChaosToken -> Message
pattern ObtainChaosToken t = ChaosBagMessage (ObtainChaosToken_ t)

-- Bidirectional pattern synonyms preserving the public API for SearchMessage.
pattern Search :: Search -> Message
pattern Search s = SearchMessage (Search_ s)

pattern ResolveSearch :: Target -> Message
pattern ResolveSearch tgt = SearchMessage (ResolveSearch_ tgt)

pattern FinishedSearch :: Message
pattern FinishedSearch = SearchMessage FinishedSearch_

pattern PreSearchFound :: InvestigatorId -> Maybe Target -> DeckSignifier -> [Card] -> Message
pattern PreSearchFound iid mt d cs = SearchMessage (PreSearchFound_ iid mt d cs)

pattern SearchFound :: InvestigatorId -> Target -> DeckSignifier -> [Card] -> Message
pattern SearchFound iid t d cs = SearchMessage (SearchFound_ iid t d cs)

pattern FoundCards :: Map Zone [Card] -> Message
pattern FoundCards m = SearchMessage (FoundCards_ m)

pattern SearchNoneFound :: InvestigatorId -> Target -> Message
pattern SearchNoneFound iid t = SearchMessage (SearchNoneFound_ iid t)

pattern UpdateSearchReturnStrategy :: InvestigatorId -> Zone -> ZoneReturnStrategy -> Message
pattern UpdateSearchReturnStrategy iid z s = SearchMessage (UpdateSearchReturnStrategy_ iid z s)

pattern SearchCollectionForRandom :: InvestigatorId -> Source -> CardMatcher -> Message
pattern SearchCollectionForRandom iid src m = SearchMessage (SearchCollectionForRandom_ iid src m)

pattern SearchEnded :: Target -> Message
pattern SearchEnded tgt = SearchMessage (SearchEnded_ tgt)

pattern CancelSearch :: Target -> Message
pattern CancelSearch tgt = SearchMessage (CancelSearch_ tgt)

pattern ClearFound :: Zone -> Message
pattern ClearFound z = SearchMessage (ClearFound_ z)

-- Bidirectional pattern synonyms preserving the public API for InvestigatorMessage.
pattern InvestigatorAssignDamage :: InvestigatorId -> Source -> DamageStrategy -> Int -> Int -> Message
pattern InvestigatorAssignDamage iid src ds d h = InvestigatorMessage (InvestigatorAssignDamage_ iid src ds d h)

pattern InvestigatorCommittedCard :: InvestigatorId -> Card -> Message
pattern InvestigatorCommittedCard iid c = InvestigatorMessage (InvestigatorCommittedCard_ iid c)

pattern InvestigatorCommittedSkill :: InvestigatorId -> SkillId -> Message
pattern InvestigatorCommittedSkill iid sid = InvestigatorMessage (InvestigatorCommittedSkill_ iid sid)

pattern InvestigatorDamage :: InvestigatorId -> Source -> Int -> Int -> Message
pattern InvestigatorDamage iid src d h = InvestigatorMessage (InvestigatorDamage_ iid src d h)

pattern InvestigatorDamageEnemy :: InvestigatorId -> EnemyId -> Source -> Message
pattern InvestigatorDamageEnemy iid eid src = InvestigatorMessage (InvestigatorDamageEnemy_ iid eid src)

pattern InvestigatorDamageInvestigator :: InvestigatorId -> InvestigatorId -> Message
pattern InvestigatorDamageInvestigator a b = InvestigatorMessage (InvestigatorDamageInvestigator_ a b)

pattern InvestigatorDefeated :: Source -> InvestigatorId -> Message
pattern InvestigatorDefeated src iid = InvestigatorMessage (InvestigatorDefeated_ src iid)

pattern InvestigatorIsDefeated :: Source -> InvestigatorId -> Message
pattern InvestigatorIsDefeated src iid = InvestigatorMessage (InvestigatorIsDefeated_ src iid)

pattern InvestigatorDirectDamage :: InvestigatorId -> Source -> Int -> Int -> Message
pattern InvestigatorDirectDamage iid src d h = InvestigatorMessage (InvestigatorDirectDamage_ iid src d h)

pattern InvestigatorDiscardAllClues :: Source -> InvestigatorId -> Message
pattern InvestigatorDiscardAllClues src iid = InvestigatorMessage (InvestigatorDiscardAllClues_ src iid)

pattern InvestigatorDoAssignDamage :: InvestigatorId -> Source -> DamageStrategy -> AssetMatcher -> Int -> Int -> [Target] -> [Target] -> Message
pattern InvestigatorDoAssignDamage iid src ds m d h dts hts = InvestigatorMessage (InvestigatorDoAssignDamage_ iid src ds m d h dts hts)

pattern InvestigatorDrawEnemy :: InvestigatorId -> EnemyId -> Message
pattern InvestigatorDrawEnemy iid eid = InvestigatorMessage (InvestigatorDrawEnemy_ iid eid)

pattern InvestigatorDrewEncounterCard :: InvestigatorId -> EncounterCard -> Message
pattern InvestigatorDrewEncounterCard iid c = InvestigatorMessage (InvestigatorDrewEncounterCard_ iid c)

pattern InvestigatorDrewEncounterCardFrom :: InvestigatorId -> EncounterCard -> Maybe DeckSignifier -> Message
pattern InvestigatorDrewEncounterCardFrom iid c mds = InvestigatorMessage (InvestigatorDrewEncounterCardFrom_ iid c mds)

pattern InvestigatorDrewPlayerCardFrom :: InvestigatorId -> PlayerCard -> Maybe DeckSignifier -> Message
pattern InvestigatorDrewPlayerCardFrom iid c mds = InvestigatorMessage (InvestigatorDrewPlayerCardFrom_ iid c mds)

pattern InvestigatorEliminated :: InvestigatorId -> Message
pattern InvestigatorEliminated iid = InvestigatorMessage (InvestigatorEliminated_ iid)

pattern InvestigatorKilled :: Source -> InvestigatorId -> Message
pattern InvestigatorKilled src iid = InvestigatorMessage (InvestigatorKilled_ src iid)

pattern InvestigatorMulligan :: InvestigatorId -> Message
pattern InvestigatorMulligan iid = InvestigatorMessage (InvestigatorMulligan_ iid)

pattern InvestigatorsMulligan :: Message
pattern InvestigatorsMulligan = InvestigatorMessage InvestigatorsMulligan_

pattern InvestigatorPlaceAllCluesOnLocation :: InvestigatorId -> Source -> Message
pattern InvestigatorPlaceAllCluesOnLocation iid src = InvestigatorMessage (InvestigatorPlaceAllCluesOnLocation_ iid src)

pattern InvestigatorPlaceCluesOnLocation :: InvestigatorId -> Source -> Int -> Message
pattern InvestigatorPlaceCluesOnLocation iid src n = InvestigatorMessage (InvestigatorPlaceCluesOnLocation_ iid src n)

pattern InvestigatorPlayAsset :: InvestigatorId -> AssetId -> Message
pattern InvestigatorPlayAsset iid aid = InvestigatorMessage (InvestigatorPlayAsset_ iid aid)

pattern InvestigatorClearUnusedAssetSlots :: InvestigatorId -> [AssetId] -> Message
pattern InvestigatorClearUnusedAssetSlots iid aids = InvestigatorMessage (InvestigatorClearUnusedAssetSlots_ iid aids)

pattern InvestigatorAdjustAssetSlots :: InvestigatorId -> AssetId -> Message
pattern InvestigatorAdjustAssetSlots iid aid = InvestigatorMessage (InvestigatorAdjustAssetSlots_ iid aid)

pattern InvestigatorAdjustSlot :: InvestigatorId -> Slot -> SlotType -> SlotType -> Message
pattern InvestigatorAdjustSlot iid s a b = InvestigatorMessage (InvestigatorAdjustSlot_ iid s a b)

pattern InvestigatorPlayedAsset :: InvestigatorId -> AssetId -> Message
pattern InvestigatorPlayedAsset iid aid = InvestigatorMessage (InvestigatorPlayedAsset_ iid aid)

pattern InvestigatorPlayEvent :: InvestigatorId -> EventId -> Maybe Target -> [Window] -> Zone -> Message
pattern InvestigatorPlayEvent iid eid mt ws z = InvestigatorMessage (InvestigatorPlayEvent_ iid eid mt ws z)

pattern InvestigatorResigned :: InvestigatorId -> Message
pattern InvestigatorResigned iid = InvestigatorMessage (InvestigatorResigned_ iid)

pattern InvestigatorSpendClues :: InvestigatorId -> Int -> Message
pattern InvestigatorSpendClues iid n = InvestigatorMessage (InvestigatorSpendClues_ iid n)

pattern InvestigatorWhenDefeated :: Source -> InvestigatorId -> Message
pattern InvestigatorWhenDefeated src iid = InvestigatorMessage (InvestigatorWhenDefeated_ src iid)

pattern InvestigatorWhenEliminated :: Source -> InvestigatorId -> Maybe Message -> Message
pattern InvestigatorWhenEliminated src iid mm = InvestigatorMessage (InvestigatorWhenEliminated_ src iid mm)

pattern InvestigatorSpecific :: InvestigatorId -> Text -> Value -> Message
pattern InvestigatorSpecific iid t v = InvestigatorMessage (InvestigatorSpecific_ iid t v)

pattern HandleKilledOrInsaneInvestigators :: Message
pattern HandleKilledOrInsaneInvestigators = InvestigatorMessage HandleKilledOrInsaneInvestigators_

pattern Resign :: InvestigatorId -> Message
pattern Resign iid = InvestigatorMessage (Resign_ iid)

pattern ResignWith :: Target -> Message
pattern ResignWith tgt = InvestigatorMessage (ResignWith_ tgt)

pattern Devour :: InvestigatorId -> Message
pattern Devour iid = InvestigatorMessage (Devour_ iid)

pattern Devoured :: InvestigatorId -> Card -> Message
pattern Devoured iid c = InvestigatorMessage (Devoured_ iid c)

pattern BeforePlayEvent :: InvestigatorId -> EventId -> ActiveCostId -> Message
pattern BeforePlayEvent iid eid cid = InvestigatorMessage (BeforePlayEvent_ iid eid cid)

pattern CreatePendingEvent :: Card -> InvestigatorId -> EventId -> Message
pattern CreatePendingEvent c iid eid = InvestigatorMessage (CreatePendingEvent_ c iid eid)

pattern BeforeCardCost :: InvestigatorId -> ActionStatus -> [Window] -> CardId -> Message
pattern BeforeCardCost iid st ws cid = InvestigatorMessage (BeforeCardCost_ iid st ws cid)

pattern FinishedEvent :: EventId -> Message
pattern FinishedEvent eid = InvestigatorMessage (FinishedEvent_ eid)

-- Back-compat pattern synonyms for the previously-entity-specific Remove* messages.
pattern RemoveAsset :: AssetId -> Message
pattern RemoveAsset aid = Remove (AssetTarget aid)

pattern RemoveEnemy :: EnemyId -> Message
pattern RemoveEnemy eid = Remove (EnemyTarget eid)

pattern RemoveEvent :: EventId -> Message
pattern RemoveEvent eid = Remove (EventTarget eid)

pattern RemoveSkill :: SkillId -> Message
pattern RemoveSkill sid = Remove (SkillTarget sid)

pattern RemoveTreachery :: TreacheryId -> Message
pattern RemoveTreachery tid = Remove (TreacheryTarget tid)

pattern RemoveLocation :: LocationId -> Message
pattern RemoveLocation lid = Remove (LocationTarget lid)

-- Bidirectional pattern synonyms preserving the public API for EnemyAttackMessage.
pattern EnemiesAttack :: Message
pattern EnemiesAttack = EnemyAttackMessage EnemiesAttack_

pattern EnemyWillAttack :: EnemyAttackDetails -> Message
pattern EnemyWillAttack d = EnemyAttackMessage (EnemyWillAttack_ d)

pattern EnemyAttack :: EnemyAttackDetails -> Message
pattern EnemyAttack d = EnemyAttackMessage (EnemyAttack_ d)

pattern InitiateEnemyAttack :: EnemyAttackDetails -> Message
pattern InitiateEnemyAttack d = EnemyAttackMessage (InitiateEnemyAttack_ d)

pattern PerformEnemyAttack :: EnemyId -> Message
pattern PerformEnemyAttack eid = EnemyAttackMessage (PerformEnemyAttack_ eid)

pattern AfterEnemyAttack :: EnemyId -> [Message] -> Message
pattern AfterEnemyAttack eid msgs = EnemyAttackMessage (AfterEnemyAttack_ eid msgs)

pattern EnemyAttackFromDiscard :: InvestigatorId -> Source -> Card -> Message
pattern EnemyAttackFromDiscard iid src c = EnemyAttackMessage (EnemyAttackFromDiscard_ iid src c)

pattern EnemyAttackIfEngaged :: EnemyId -> Maybe InvestigatorId -> Message
pattern EnemyAttackIfEngaged eid miid = EnemyAttackMessage (EnemyAttackIfEngaged_ eid miid)

pattern EnemyAttacks :: [Message] -> Message
pattern EnemyAttacks msgs = EnemyAttackMessage (EnemyAttacks_ msgs)

pattern ChangeEnemyAttackTarget :: EnemyId -> Target -> Message
pattern ChangeEnemyAttackTarget eid t = EnemyAttackMessage (ChangeEnemyAttackTarget_ eid t)

pattern ChangeEnemyAttackDetails :: EnemyId -> EnemyAttackDetails -> Message
pattern ChangeEnemyAttackDetails eid d = EnemyAttackMessage (ChangeEnemyAttackDetails_ eid d)

-- Bidirectional pattern synonyms preserving the public API for DamageMessage.
pattern DealDamage :: Target -> DamageAssignment -> Message
pattern DealDamage tgt da = DamageMessage (DealDamage_ tgt da)

-- Named 'Damaged' (not 'DealtDamage') to avoid clashing with the matcher
-- 'Arkham.Matcher.Window.DealtDamage' that cards use in trigger conditions.
pattern Damaged :: Target -> DamageAssignment -> Message
pattern Damaged tgt da = DamageMessage (Damaged_ tgt da)

pattern HealDamage :: Target -> Source -> Int -> Message
pattern HealDamage tgt src n = DamageMessage (HealDamage_ tgt src n)

pattern HealAllDamage :: Target -> Source -> Message
pattern HealAllDamage tgt src = DamageMessage (HealAllDamage_ tgt src)

pattern PlaceAdditionalDamage :: Target -> Source -> Int -> Int -> Message
pattern PlaceAdditionalDamage tgt src h s = DamageMessage (PlaceAdditionalDamage_ tgt src h s)

-- Bidirectional pattern synonyms preserving the public API for DefeatMessage.
pattern AssetDefeated :: Source -> AssetId -> Message
pattern AssetDefeated src aid = DefeatMessage (AssetDefeated_ src aid)

pattern CheckDefeated :: Source -> Target -> Message
pattern CheckDefeated src tgt = DefeatMessage (CheckDefeated_ src tgt)

pattern DefeatEnemy :: EnemyId -> InvestigatorId -> Source -> Message
pattern DefeatEnemy eid iid src = DefeatMessage (DefeatEnemy_ eid iid src)

pattern Defeated :: Target -> CardId -> Source -> [Trait] -> Message
pattern Defeated tgt cid src ts = DefeatMessage (Defeated_ tgt cid src ts)

pattern EnemyLocationDefeated :: LocationId -> CardId -> Source -> [Trait] -> Message
pattern EnemyLocationDefeated lid cid src ts = DefeatMessage (EnemyLocationDefeated_ lid cid src ts)

-- Bidirectional pattern synonyms preserving the public API for ExhaustMessage.
pattern Exhaust :: Exhaustion Message -> Message
pattern Exhaust e = ExhaustMessage (Exhaust_ e)

pattern Ready :: Target -> Message
pattern Ready tgt = ExhaustMessage (Ready_ tgt)

pattern ReadyAlternative :: Source -> Target -> Message
pattern ReadyAlternative src tgt = ExhaustMessage (ReadyAlternative_ src tgt)

pattern ReadyExhausted :: Message
pattern ReadyExhausted = ExhaustMessage ReadyExhausted_

-- Bidirectional pattern synonyms preserving the public API for EngageMessage.
pattern EngageEnemy :: InvestigatorId -> EnemyId -> Maybe Target -> Bool -> Message
pattern EngageEnemy iid eid mt b = EngageMessage (EngageEnemy_ iid eid mt b)

pattern DisengageEnemy :: InvestigatorId -> EnemyId -> Message
pattern DisengageEnemy iid eid = EngageMessage (DisengageEnemy_ iid eid)

pattern DisengageEnemyFromAll :: EnemyId -> Message
pattern DisengageEnemyFromAll eid = EngageMessage (DisengageEnemyFromAll_ eid)

pattern ChooseEngageEnemy :: InvestigatorId -> Source -> Maybe Target -> EnemyMatcher -> Bool -> Message
pattern ChooseEngageEnemy iid src mt m b = EngageMessage (ChooseEngageEnemy_ iid src mt m b)

pattern CheckEnemyEngagement :: InvestigatorId -> Message
pattern CheckEnemyEngagement iid = EngageMessage (CheckEnemyEngagement_ iid)

pattern EnemyCheckEngagement :: EnemyId -> Message
pattern EnemyCheckEngagement eid = EngageMessage (EnemyCheckEngagement_ eid)

pattern EnemyEngageInvestigator :: EnemyId -> InvestigatorId -> Message
pattern EnemyEngageInvestigator eid iid = EngageMessage (EnemyEngageInvestigator_ eid iid)

-- Bidirectional pattern synonyms preserving the public API for SpawnMessage.
pattern EnemySpawn :: SpawnDetails -> Message
pattern EnemySpawn d = SpawnMessage (EnemySpawn_ d)

pattern EnemySpawned :: SpawnDetails -> Message
pattern EnemySpawned d = SpawnMessage (EnemySpawned_ d)

pattern EnemySpawnAtLocationMatching :: Maybe InvestigatorId -> LocationMatcher -> EnemyId -> Message
pattern EnemySpawnAtLocationMatching miid m eid = SpawnMessage (EnemySpawnAtLocationMatching_ miid m eid)

pattern EnemySpawnFromOutOfPlay :: OutOfPlayZone -> Maybe InvestigatorId -> LocationId -> EnemyId -> Message
pattern EnemySpawnFromOutOfPlay z miid lid eid = SpawnMessage (EnemySpawnFromOutOfPlay_ z miid lid eid)

pattern EnemySpawnEngagedWithPrey :: EnemyId -> Message
pattern EnemySpawnEngagedWithPrey eid = SpawnMessage (EnemySpawnEngagedWithPrey_ eid)

pattern EnemySpawnEngagedWith :: EnemyId -> InvestigatorMatcher -> Message
pattern EnemySpawnEngagedWith eid m = SpawnMessage (EnemySpawnEngagedWith_ eid m)

pattern EnemyEntered :: EnemyId -> LocationId -> Message
pattern EnemyEntered eid lid = SpawnMessage (EnemyEntered_ eid lid)

-- Bidirectional pattern synonyms preserving the public API for HuntMessage.
pattern EnemyMove :: EnemyId -> LocationId -> Message
pattern EnemyMove eid lid = HuntMessage (EnemyMove_ eid lid)

pattern HuntersMove :: Message
pattern HuntersMove = HuntMessage HuntersMove_

pattern HunterMove :: EnemyId -> Message
pattern HunterMove eid = HuntMessage (HunterMove_ eid)

pattern PatrolMove :: EnemyId -> LocationMatcher -> Message
pattern PatrolMove eid m = HuntMessage (PatrolMove_ eid m)

pattern MoveToward :: Target -> LocationMatcher -> Message
pattern MoveToward t m = HuntMessage (MoveToward_ t m)

pattern MoveUntil :: LocationId -> Target -> Message
pattern MoveUntil lid t = HuntMessage (MoveUntil_ lid t)

pattern WillMoveEnemy :: EnemyId -> Message -> Message
pattern WillMoveEnemy eid msg = HuntMessage (WillMoveEnemy_ eid msg)

pattern HandleElusive :: EnemyId -> Message
pattern HandleElusive eid = HuntMessage (HandleElusive_ eid)

-- Bidirectional pattern synonyms preserving the public API for ClueMessage.
pattern PlaceCluesUpToClueValue :: LocationId -> Source -> Int -> Message
pattern PlaceCluesUpToClueValue lid src n = ClueMessage (PlaceCluesUpToClueValue_ lid src n)

pattern MoveAllCluesTo :: Source -> Target -> Message
pattern MoveAllCluesTo src tgt = ClueMessage (MoveAllCluesTo_ src tgt)

pattern RemoveAllClues :: Source -> Target -> Message
pattern RemoveAllClues src tgt = ClueMessage (RemoveAllClues_ src tgt)

pattern FlipClues :: Target -> Int -> Message
pattern FlipClues tgt n = ClueMessage (FlipClues_ tgt n)

-- Bidirectional pattern synonyms preserving the public API for DoomMessage.
pattern RemoveAllDoom :: Source -> Target -> Message
pattern RemoveAllDoom src tgt = DoomMessage (RemoveAllDoom_ src tgt)

pattern RemoveAllDoomFromPlay :: RemoveDoomMatchers -> Message
pattern RemoveAllDoomFromPlay m = DoomMessage (RemoveAllDoomFromPlay_ m)

pattern FlipDoom :: Target -> Int -> Message
pattern FlipDoom tgt n = DoomMessage (FlipDoom_ tgt n)

-- Bidirectional pattern synonyms preserving the public API for TokenMessage.
pattern PlaceTokens :: Source -> Target -> Token.Token -> Int -> Message
pattern PlaceTokens src tgt t n = TokenMessage (PlaceTokens_ src tgt t n)

pattern RemoveTokens :: Source -> Target -> Token.Token -> Int -> Message
pattern RemoveTokens src tgt t n = TokenMessage (RemoveTokens_ src tgt t n)

pattern ClearTokens :: Target -> Message
pattern ClearTokens tgt = TokenMessage (ClearTokens_ tgt)

pattern MoveTokens :: Source -> Source -> Target -> Token.Token -> Int -> Message
pattern MoveTokens s1 s2 tgt t n = TokenMessage (MoveTokens_ s1 s2 tgt t n)

pattern MoveTokensNoDefeated :: Source -> Source -> Target -> Token.Token -> Int -> Message
pattern MoveTokensNoDefeated s1 s2 tgt t n = TokenMessage (MoveTokensNoDefeated_ s1 s2 tgt t n)

pattern RemoveAllTokens :: Source -> Target -> Message
pattern RemoveAllTokens src tgt = TokenMessage (RemoveAllTokens_ src tgt)

-- Bidirectional pattern synonyms preserving the public API for SealMessage.
pattern SealChaosToken :: ChaosToken -> Message
pattern SealChaosToken t = SealMessage (SealChaosToken_ t)

pattern SealedChaosToken :: ChaosToken -> Maybe InvestigatorId -> Target -> Message
pattern SealedChaosToken t miid tgt = SealMessage (SealedChaosToken_ t miid tgt)

pattern SetChaosTokenAside :: ChaosToken -> Message
pattern SetChaosTokenAside t = SealMessage (SetChaosTokenAside_ t)

pattern UnsealChaosToken :: ChaosToken -> Message
pattern UnsealChaosToken t = SealMessage (UnsealChaosToken_ t)

pattern RemoveAllChaosTokens :: ChaosTokenFace -> Message
pattern RemoveAllChaosTokens f = SealMessage (RemoveAllChaosTokens_ f)

-- Bidirectional pattern synonyms preserving the public API for HorrorMessage.
pattern HealAllHorror :: Target -> Source -> Message
pattern HealAllHorror tgt src = HorrorMessage (HealAllHorror_ tgt src)

pattern HealHorror :: Target -> Source -> Int -> Message
pattern HealHorror tgt src n = HorrorMessage (HealHorror_ tgt src n)

pattern ExcessHealHorror :: InvestigatorId -> Source -> Int -> Message
pattern ExcessHealHorror iid src n = HorrorMessage (ExcessHealHorror_ iid src n)

pattern CancelHorror :: InvestigatorId -> Int -> Message
pattern CancelHorror iid n = HorrorMessage (CancelHorror_ iid n)

pattern CancelAssetHorror :: AssetId -> Source -> Int -> Message
pattern CancelAssetHorror aid src n = HorrorMessage (CancelAssetHorror_ aid src n)

deblank :: Message -> Message
deblank = \case
  Blanked msg -> msg
  msg -> msg

mconcat
  [ deriveToJSON defaultOptions ''Message
  , [d|
      instance FromJSON Message where
        parseJSON = withObject "Message" \o -> do
          t :: Text <- o .: "tag"
          case t of
            "KonamiCode" -> KonamiCode <$> (o .:? "contents" .!= PlayerId UUID.nil)
            "SkillTestResultOption" -> do
              econtents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
              pure $ case econtents of
                Right a -> SkillTestResultOption a
                Left (a, b) ->
                  SkillTestResultOption
                    $ SkillTestOption
                      { option = Label a b
                      , kind = OriginalOptionKind
                      , criteria = Nothing
                      }
            "SkillTestResultOptions" -> do
              econtents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
              pure $ case econtents of
                Right as -> SkillTestResultOptions as
                Left as ->
                  SkillTestResultOptions $ as & map \a ->
                    SkillTestOption
                      { option = a
                      , kind = OriginalOptionKind
                      , criteria = Nothing
                      }
            "LoseAll" -> do
              (iid, source, tkn) <- o .: "contents"
              pure $ LoseTokens iid source tkn AllLost
            "SetRole" -> do
              (iid, role :: ClassSymbol) <- o .: "contents"
              pure $ InvestigatorSpecific iid "setRole" (toJSON role)
            "AddToHandQuiet" -> do
              contents <- o .: "contents"
              pure $ uncurry AddToHand contents
            "PlayerWindow" -> do
              contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
              case contents of
                Right (a, b, c, d) -> pure $ PlayerWindow a b c d
                Left (a, b, c) -> pure $ PlayerWindow a b c True
            "AddToVictory" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ AddToVictory a b
                Left a -> pure $ AddToVictory Nothing a
            "DefeatedAddToVictory" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ AddToVictory a b
                Left a -> pure $ AddToVictory Nothing a
            "StartScenrio" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ StartScenario a b
                Left a -> pure $ StartScenario a Nothing
            "AssignedDamage" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b, c) -> pure $ AssignedDamage a b c
                Left a -> pure $ AssignedDamage a 0 0
            "RemoveCampaignCard" -> RemoveCampaignCardFromDeck "00000" <$> o .: "contents"
            "ResolvedMovement" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ ResolvedMovement a b
                Left a -> pure $ ResolvedMovement a (MovementId UUID.nil)
            "CheckAttackOfOpportunity" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b, c) -> pure $ CheckAttackOfOpportunity a b c
                Left (a, b) -> pure $ CheckAttackOfOpportunity a b Nothing
            "AddCardEntity" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ AddCardEntity a b
                Left b -> pure $ AddCardEntity (fromWords64 6128981282234515924 12039885860129472512) b
            "RemoveCardEntity" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ RemoveCardEntity a b
                Left b -> pure $ RemoveCardEntity (fromWords64 6128981282234515924 12039885860129472512) b
            "LoseAllResources" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left i -> pure $ LoseAll i GameSource Token.Resource
                Right (i, s) -> pure $ LoseAll i s Token.Resource
            "DrawEnded" -> do
              contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
              case contents of
                Right (a, b) -> pure $ DrawEnded a b
                Left a -> pure $ DrawEnded (CardDrawId UUID.nil) a
            "PreSearchFound" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b, c, d) -> pure $ PreSearchFound a b c d
                Left (a, b, c, d) -> pure $ PreSearchFound a (Just b) c d
            "CancelEachNext" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b, c) -> pure $ CancelEachNext a b c
                Left (b, c) -> pure $ CancelEachNext Nothing b c
            "HandleGroupTargets" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (a, b, c) -> pure $ HandleGroupTargets a b c
                Left (b, c) -> pure $ HandleGroupTargets Manual b c
            "RefillSlots" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (iid, xs) -> pure $ RefillSlots iid xs
                Left iid -> pure $ RefillSlots iid []
            "InvestigatorClearUnusedAssetSlots" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right (iid, xs) -> pure $ InvestigatorClearUnusedAssetSlots iid xs
                Left iid -> pure $ InvestigatorClearUnusedAssetSlots iid []
            "EnemySpawn" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Right details -> pure $ EnemySpawn details
                Left (miid, lid, eid) ->
                  pure
                    $ EnemySpawn
                    $ (mkSpawnDetails eid (Arkham.Spawn.SpawnAtLocation lid)) {spawnDetailsInvestigator = miid}
            "FightEnemy" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (sid, iid, eid, src, mTarget, sType, isAction) ->
                  pure
                    $ FightEnemy eid
                    $ ChooseFight
                      iid
                      (EnemyWithId eid)
                      src
                      mTarget
                      sType
                      isAction
                      isAction
                      False
                      False
                      sid
                      DefaultChooseFightDifficulty
                Right (eid, cf) -> pure $ FightEnemy eid cf
            "AttackEnemy" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (sid, iid, eid, src, mTarget, sType) ->
                  pure
                    $ AttackEnemy eid
                    $ ChooseFight
                      iid
                      (EnemyWithId eid)
                      src
                      mTarget
                      sType
                      False
                      False
                      False
                      False
                      sid
                      DefaultChooseFightDifficulty
                Right (eid, cf) -> pure $ AttackEnemy eid cf
            "FailedAttackEnemy" -> do
              -- Legacy: this constructor used to live on Message directly; saves
              -- written before the FightMessage extraction tag it like this.
              (iid, eid) <- o .: "contents"
              pure $ FailedAttackEnemy iid eid
            "ChooseFightEnemy" -> do
              -- Legacy: see FailedAttackEnemy above.
              cf <- o .: "contents"
              pure $ ChooseFightEnemy cf
            "ChooseEvadeEnemy" -> do
              -- Legacy: pre-EvadeMessage save format.
              ce <- o .: "contents"
              pure $ ChooseEvadeEnemy ce
            "TryEvadeEnemy" -> do
              (sid, iid, eid, src, mt, st) <- o .: "contents"
              pure $ TryEvadeEnemy sid iid eid src mt st
            "EvadeEnemy" -> do
              (sid, iid, eid, src, mt, st, b) <- o .: "contents"
              pure $ EvadeEnemy sid iid eid src mt st b
            "EnemyEvaded" -> do
              (iid, eid) <- o .: "contents"
              pure $ EnemyEvaded iid eid
            "ChosenEvadeEnemy" -> do
              (sid, src, eid) <- o .: "contents"
              pure $ ChosenEvadeEnemy sid src eid
            "AfterEvadeEnemy" -> do
              (iid, eid) <- o .: "contents"
              pure $ AfterEvadeEnemy iid eid
            -- Legacy: pre-EnemyAttackMessage save format.
            "EnemiesAttack" -> pure EnemiesAttack
            "EnemyWillAttack" -> do
              d <- o .: "contents"
              pure $ EnemyWillAttack d
            "EnemyAttack" -> do
              d <- o .: "contents"
              pure $ EnemyAttack d
            "InitiateEnemyAttack" -> do
              d <- o .: "contents"
              pure $ InitiateEnemyAttack d
            "PerformEnemyAttack" -> do
              eid <- o .: "contents"
              pure $ PerformEnemyAttack eid
            "AfterEnemyAttack" -> do
              (eid, msgs) <- o .: "contents"
              pure $ AfterEnemyAttack eid msgs
            "EnemyAttackFromDiscard" -> do
              (iid, src, c) <- o .: "contents"
              pure $ EnemyAttackFromDiscard iid src c
            "EnemyAttackIfEngaged" -> do
              (eid, miid) <- o .: "contents"
              pure $ EnemyAttackIfEngaged eid miid
            "EnemyAttacks" -> do
              msgs <- o .: "contents"
              pure $ EnemyAttacks msgs
            "ChangeEnemyAttackTarget" -> do
              (eid, tgt) <- o .: "contents"
              pure $ ChangeEnemyAttackTarget eid tgt
            "ChangeEnemyAttackDetails" -> do
              (eid, d) <- o .: "contents"
              pure $ ChangeEnemyAttackDetails eid d
            "HealDamage" -> do
              (tgt, src, n) <- o .: "contents"
              pure $ HealDamage tgt src n
            "HealAllDamage" -> do
              (tgt, src) <- o .: "contents"
              pure $ HealAllDamage tgt src
            "PlaceAdditionalDamage" -> do
              (tgt, src, h, s) <- o .: "contents"
              pure $ PlaceAdditionalDamage tgt src h s
            -- Legacy: pre-DefeatMessage save format.
            "AssetDefeated" -> do
              (src, aid) <- o .: "contents"
              pure $ AssetDefeated src aid
            "CheckDefeated" -> do
              (src, tgt) <- o .: "contents"
              pure $ CheckDefeated src tgt
            "DefeatEnemy" -> do
              (eid, iid, src) <- o .: "contents"
              pure $ DefeatEnemy eid iid src
            "EnemyLocationDefeated" -> do
              (lid, cid, src, ts) <- o .: "contents"
              pure $ EnemyLocationDefeated lid cid src ts
            -- Legacy: pre-ExhaustMessage save format. Note: the "Exhaust" tag is
            -- handled by the existing legacy case further down.
            "Ready" -> do
              tgt <- o .: "contents"
              pure $ Ready tgt
            "ReadyAlternative" -> do
              (src, tgt) <- o .: "contents"
              pure $ ReadyAlternative src tgt
            "ReadyExhausted" -> pure ReadyExhausted
            "ObtainCard" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (card :: Card) -> pure $ ObtainCard (toCardId card)
                Right cardId -> pure $ ObtainCard cardId
            "BeforeSkillTest" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left st -> pure $ BeforeSkillTest (skillTestId st)
                Right stId -> pure $ BeforeSkillTest stId
            "RepeatSkillTest" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (stId, st) -> pure $ RepeatSkillTest stId (skillTestId st)
                Right (stId, stId') -> pure $ RepeatSkillTest stId stId'
            "CommitToSkillTest" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (st, uim) -> pure $ CommitToSkillTest (skillTestId st) uim
                Right (stId, uim) -> pure $ CommitToSkillTest stId uim
            "AddCampaignCardToDeck" -> do
              contents <-
                (Left <$> o .: "contents")
                  <|> (Right . Left <$> o .: "contents")
                  <|> (Right . Right <$> o .: "contents")
                  <|> fail ("Invalid AddCampaignCardToDeck: " <> show o)
              case contents of
                Left (iid, card :: Card) -> pure $ AddCampaignCardToDeck iid ShuffleIn card
                Right (Left (iid, cardDef :: CardDef)) -> pure $ AddCampaignCardToDeck iid ShuffleIn (lookupCard cardDef.cardCode (unsafeMakeCardId nil))
                Right (Right (iid, shouldShuffleIn, card :: Card)) -> pure $ AddCampaignCardToDeck iid shouldShuffleIn card
            "SealedChaosToken" -> do
              contents <-
                (Left <$> o .: "contents")
                  <|> (Right . Left <$> o .: "contents")
                  <|> (Right . Right <$> o .: "contents")
              case contents of
                Left (token, card :: Card) -> pure $ SealedChaosToken token Nothing (toTarget card)
                Right (Left (token, target)) -> pure $ SealedChaosToken token Nothing target
                Right (Right (token, miid, target)) -> pure $ SealedChaosToken token miid target
            "ExcessHealHorror" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (i, n) -> pure $ ExcessHealHorror i GameSource n
                Right (i, s, n) -> pure $ ExcessHealHorror i s n
            "ExcessHealDamage" -> do
              contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case contents of
                Left (i, n) -> pure $ ExcessHealDamage i GameSource n
                Right (i, s, n) -> pure $ ExcessHealDamage i s n
            "RunWindow" -> do
              (_a :: Value, b) <- o .: "contents"
              pure $ CheckWindows b
            "CheckWindow" -> do
              (_a :: Value, b) <- o .: "contents"
              pure $ CheckWindows b
            "AssetDamageWithCheck" -> do
              (a, b, c, d, e) <- o .: "contents"
              pure $ DealAssetDamageWithCheck a b c d e
            "InvestigatorDrewPlayerCard" -> do
              (a, b) <- o .: "contents"
              pure $ InvestigatorDrewPlayerCardFrom a b Nothing
            "ReportXp" -> do
              ReportXp <$> (o .: "contents" <|> (snd @ScenarioId <$> o .: "contents"))
            "ReadStoryWithPlacement" -> do
              (a, b, c, d, e) <- o .: "contents"
              pure $ StoryMessage $ ReadStoryWithPlacement a b c d e
            "ReadStory" -> do
              (a, b, c, d) <- o .: "contents"
              pure $ StoryMessage $ ReadStory a b c d
            "ResolveStory" -> do
              (a, b, c) <- o .: "contents"
              pure $ StoryMessage $ ResolveStory a b c
            "ResolvedStory" -> do
              (a, b) <- o .: "contents"
              pure $ StoryMessage $ ResolvedStory a b
            "PlaceStory" -> do
              (a, b) <- o .: "contents"
              pure $ StoryMessage $ PlaceStory a b
            "RemoveStory" -> do
              a <- o .: "contents"
              pure $ StoryMessage $ RemoveStory a
            "ResolveSearch" -> do
              ea <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case ea of
                Left a -> pure $ ResolveSearch (InvestigatorTarget a)
                Right a -> pure $ ResolveSearch a
            "SearchEnded" -> do
              ea <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case ea of
                Left a -> pure $ SearchEnded (InvestigatorTarget a)
                Right a -> pure $ SearchEnded a
            "CancelSearch" -> do
              ea <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              case ea of
                Left a -> pure $ CancelSearch (InvestigatorTarget a)
                Right a -> pure $ CancelSearch a
            "Exhaust" -> do
              ea <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              pure $ case ea of
                Left target -> Exhaust $ Exhaustion GameSource target []
                Right exhaustion -> Exhaust exhaustion
            "ExhaustThen" -> do
              (target, msgs) <- o .: "contents"
              pure $ Exhaust $ Exhaustion GameSource target msgs
            "ShuffleBackIntoEncounterDeck" -> do
              ea <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
              pure $ case ea of
                Left target -> ShuffleBackIntoEncounterDeck GameSource target
                Right (source, target) -> ShuffleBackIntoEncounterDeck source target
            "FindEncounterCard" -> do
              contents <- (Right <$> o .: "contents") <|> (Left <$> o .: "contents")
              pure $ case contents of
                Right (a, b, c, d, s) -> FindEncounterCard a b c d s
                Left (a, b, c, d) -> FindEncounterCard a b c d LeadChooses
            _ -> defaultParseMessage (Object o)
      |]
  ]

defaultParseMessage :: Value -> Parser Message
defaultParseMessage = $(mkParseJSON defaultOptions ''Message)
{-# NOINLINE defaultParseMessage #-}

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  CostLabel _ msgs -> Run msgs
  InvalidLabel {} -> error "InvalidLabel in uiToRun"
  Info {} -> error "Info in uiToRun"
  TooltipLabel _ _ msgs -> Run msgs
  CardLabel _ _ msgs -> Run msgs
  ChaosTokenLabel _ msgs -> Run msgs
  PortraitLabel _ msgs -> Run msgs
  KeyLabel _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  GridLabel _ msgs -> Run msgs
  TarotLabel _ msgs -> Run msgs
  SkillLabel _ msgs -> Run msgs
  SkillLabelWithLabel _ _ msgs -> Run msgs
  EvadeLabel _ msgs -> Run msgs
  EvadeLabelWithSkill _ _ msgs -> Run msgs
  FightLabel _ msgs -> Run msgs
  FightLabelWithSkill _ _ msgs -> Run msgs
  EngageLabel _ msgs -> Run msgs
  AbilityLabel iid ab windows before msgs -> Run $ before <> [UseAbility iid ab windows] <> msgs
  ComponentLabel _ msgs -> Run msgs
  AuxiliaryComponentLabel _ msgs -> Run msgs
  EndTurnButton _ msgs -> Run msgs
  StartSkillTestButton iid -> Run [StartSkillTest iid]
  SkillTestApplyResultsButton -> Run [SkillTestApplyResults]
  ChaosTokenGroupChoice source iid step -> Run [ChooseChaosTokenGroups source iid step]
  EffectActionButton _ _ msgs -> Run msgs
  Done _ -> Run []
  SkipTriggersButton iid -> Run [SkippedWindow iid]
  CardPile _ msgs -> Run msgs
  ScenarioLabel _ _ msgs -> Run msgs

chooseOrRunOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid msgs = chooseOne pid msgs

questionLabel :: Text -> PlayerId -> Question Message -> Message
questionLabel lbl pid q = Ask pid (QuestionLabel lbl Nothing q)

questionLabelWithCard :: Text -> CardCode -> PlayerId -> Question Message -> Message
questionLabelWithCard lbl cCode pid q = Ask pid (QuestionLabel lbl (Just cCode) q)

asWindowChoose :: HasCallStack => [Window] -> Message -> Message
asWindowChoose ws (Ask pid q) = WindowAsk ws pid q
asWindowChoose _ _ = error "asWindowChoose: expected Ask"

chooseOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error $ "No messages for chooseOne: " <> prettyCallStack callStack
chooseOne pid msgs = Ask pid (ChooseOne msgs)

chooseOneFromEach :: HasCallStack => PlayerId -> [[UI Message]] -> Message
chooseOneFromEach _ [] = error $ "No messages for chooseOneFromEach: " <> prettyCallStack callStack
chooseOneFromEach pid msgs = Ask pid (ChooseOneFromEach msgs)

chooseOneDropDown :: PlayerId -> [(Text, Message)] -> Message
chooseOneDropDown _ [] = throw $ InvalidState "No messages for chooseOneDropDown"
chooseOneDropDown pid msgs = Ask pid (DropDown msgs)

chooseOneAtATime :: PlayerId -> [UI Message] -> Message
chooseOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOneAtATime pid msgs = Ask pid (ChooseOneAtATime msgs)

chooseOrRunOneAtATime :: PlayerId -> [UI Message] -> Message
chooseOrRunOneAtATime _ [] = throw $ InvalidState "No messages for chooseOrRunOneAtATime"
chooseOrRunOneAtATime _ [x] = uiToRun x
chooseOrRunOneAtATime pid msgs = Ask pid (ChooseOneAtATime msgs)

chooseOrRunOneAtATimeWithLabel :: Text -> PlayerId -> [UI Message] -> Message
chooseOrRunOneAtATimeWithLabel _ _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOrRunOneAtATimeWithLabel _ _ [x] = uiToRun x
chooseOrRunOneAtATimeWithLabel lbl pid msgs = Ask pid (QuestionLabel lbl Nothing $ ChooseOneAtATime msgs)

chooseSome :: PlayerId -> Text -> [UI Message] -> Message
chooseSome _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseSome pid doneText msgs = Ask pid (ChooseSome $ Done doneText : msgs)

chooseSome1 :: PlayerId -> Text -> [UI Message] -> Message
chooseSome1 _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseSome1 pid doneText msgs = Ask pid (ChooseSome1 doneText msgs)

chooseUpToN :: PlayerId -> Int -> Text -> [UI Message] -> Message
chooseUpToN _ _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseUpToN pid n doneText msgs =
  Ask pid (ChooseUpToN n $ Done doneText : msgs)

chooseN :: PlayerId -> Int -> [UI Message] -> Message
chooseN _ _ [] = throw $ InvalidState "No messages for chooseN"
chooseN pid n msgs = Ask pid (ChooseN n msgs)

chooseOrRunN :: PlayerId -> Int -> [UI Message] -> Message
chooseOrRunN _ _ [] = throw $ InvalidState "No messages for chooseN"
chooseOrRunN _ n msgs | length msgs == n = Run $ map uiToRun msgs
chooseOrRunN pid n msgs = Ask pid (ChooseN n msgs)

chooseAmounts
  :: (Targetable target, MonadRandom m)
  => PlayerId
  -> Text
  -> AmountTarget
  -> [(Text, (Int, Int))]
  -> target
  -> m Message
chooseAmounts pid label total choiceMap (toTarget -> target) = do
  rs <- getRandoms
  pure $ Ask pid (ChooseAmounts label total (amountChoices rs) target)
 where
  amountChoices rs = zipWith (curry toAmountChoice) rs choiceMap
  toAmountChoice (choiceId, (l, (m, n))) = AmountChoice choiceId l m n

chooseAmountsLabeled
  :: (Targetable target, MonadRandom m)
  => PlayerId
  -> Text
  -> Text
  -> AmountTarget
  -> [(Text, (Int, Int))]
  -> target
  -> m Message
chooseAmountsLabeled pid title label total choiceMap (toTarget -> target) = do
  rs <- getRandoms
  pure $ Ask pid (QuestionLabel title Nothing $ ChooseAmounts label total (amountChoices rs) target)
 where
  amountChoices rs = zipWith (curry toAmountChoice) rs choiceMap
  toAmountChoice (choiceId, (l, (m, n))) = AmountChoice choiceId l m n

chooseUpgradeDecks :: [PlayerId] -> Message
chooseUpgradeDecks pids =
  Run
    [ SetGameState (IsChooseDecks pids)
    , UpgradingDecks
    , AskMap $ mapFromList $ map (,ChooseUpgradeDeck) pids
    , DoneUpgradingDecks
    ]

chooseDecks :: [PlayerId] -> Message
chooseDecks pids =
  Run
    [ SetGameState (IsChooseDecks pids)
    , ChoosingDecks
    , AskMap $ mapFromList $ map (,ChooseDeck) pids
    , DoneChoosingDecks
    ]
-- 

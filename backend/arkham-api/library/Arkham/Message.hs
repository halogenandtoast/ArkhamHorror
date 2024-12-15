{-# LANGUAGE TemplateHaskell #-}

module Arkham.Message (
  module Arkham.Message,
  module X,
) where

import Arkham.Prelude

import Arkham.Message.Type as X
import Arkham.Question as X
import Arkham.Strategy as X
import Arkham.Text as X

import Arkham.Ability.Types
import Arkham.Act.Sequence
import Arkham.Action hiding (Explore)
import Arkham.Action.Additional
import Arkham.Agenda.Sequence
import Arkham.Asset.Uses
import Arkham.Attack.Types
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Supply
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
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Evade.Types
import Arkham.Exception
import Arkham.Field
import Arkham.Fight.Types
import Arkham.Game.State
import Arkham.Helpers
import Arkham.History
import Arkham.Id
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Key
import Arkham.Layout
import Arkham.Location.FloodLevel
import Arkham.Location.Grid
import {-# SOURCE #-} Arkham.Location.Types
import Arkham.Matcher hiding (EnemyDefeated, InvestigatorDefeated, RevealChaosToken)
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.RequestedChaosTokenStrategy
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.Scenarios.BeforeTheBlackThrone.Cosmos
import {-# SOURCE #-} Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult qualified as SkillTest
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Tarot
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (Window, WindowType)
import Arkham.Xp
import Data.Aeson.Key qualified as Aeson
import Data.Aeson.TH
import Data.UUID (nil)
import GHC.OverloadedLabels

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack {} = Just AttackMessage
messageType (After (PerformEnemyAttack {})) = Just AttackMessage
messageType Revelation {} = Just RevelationMessage
messageType DrawChaosToken {} = Just DrawChaosTokenMessage
messageType ResolveChaosToken {} = Just ResolveChaosTokenMessage
messageType EnemySpawn {} = Just EnemySpawnMessage
messageType InvestigatorDrawEnemy {} = Just DrawEnemyMessage
messageType EnemyDefeated {} = Just EnemyDefeatedMessage
messageType (Discard _ GameSource (EnemyTarget _)) = Just EnemyDefeatedMessage
messageType RevealChaosToken {} = Just RevealChaosTokenMessage
messageType InvestigatorDamage {} = Just DamageMessage
messageType InvestigatorDoAssignDamage {} = Just DamageMessage
messageType InvestigatorDrewEncounterCard {} = Just DrawEncounterCardMessage
messageType InvestigatorDefeated {} = Just InvestigatorDefeatedMessage
messageType InvestigatorIsDefeated {} = Just InvestigatorDefeatedMessage
messageType CheckWindows {} = Just CheckWindowMessage
messageType Explore {} = Just ExploreMessage
messageType (Do msg) = messageType msg
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

data AdvancementMethod = AdvancedWithClues | AdvancedWithOther
  deriving stock (Generic, Eq, Show, Data)
  deriving anyclass (FromJSON, ToJSON)

instance IsLabel "clues" AdvancementMethod where
  fromLabel = AdvancedWithClues

instance IsLabel "other" AdvancementMethod where
  fromLabel = AdvancedWithOther

data AgendaAdvancementMethod = AgendaAdvancedWithDoom | AgendaAdvancedWithOther
  deriving stock (Generic, Eq, Show, Data)
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
pattern AdvanceAgenda aid <- AdvanceAgendaBy aid AgendaAdvancedWithDoom
  where
    AdvanceAgenda aid = AdvanceAgendaBy aid AgendaAdvancedWithDoom

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
pattern CancelNext source msgType = CancelEachNext source [msgType]

pattern CancelRevelation :: Source -> Message
pattern CancelRevelation source = CancelEachNext source [RevelationMessage]

pattern PlayThisEvent :: InvestigatorId -> EventId -> Message
pattern PlayThisEvent iid eid <- InvestigatorPlayEvent iid eid _ _ _

getChoiceAmount :: Text -> [(NamedUUID, Int)] -> Int
getChoiceAmount key choices =
  let choicesMap = mapFromList @(Map Text Int) $ map (first nuName) choices
   in findWithDefault 0 key choicesMap

class IsMessage msg where
  toMessage :: msg -> Message

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
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data StoryMode = ResolveIt | DoNotResolveIt
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data IncludeDiscard = IncludeDiscard | ExcludeDiscard
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

data SearchType = Searching | Looking | Revealing
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype FromSkillType = FromSkillType SkillType
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

newtype ToSkillType = ToSkillType SkillType
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

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
  deriving stock (Show, Eq, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

class AndThen a where
  andThen :: a -> Message -> a

instance AndThen (CardDraw Message) where
  andThen cd msg = cd {cardDrawAndThen = Just msg}

data Message
  = UseAbility InvestigatorId Ability [Window]
  | UpdateGlobalSetting InvestigatorId SetGlobalSetting
  | UpdateCardSetting InvestigatorId CardCode SetCardSetting
  | SetDriver AssetId InvestigatorId
  | SetGameState GameState
  | SetGlobal Target Aeson.Key Value
  | IncreaseFloodLevel LocationId
  | DecreaseFloodLevel LocationId
  | SetFloodLevel LocationId FloodLevel
  | Devour InvestigatorId
  | Devoured InvestigatorId Card
  | MoveWithSkillTest Message
  | MovedWithSkillTest SkillTestId Message
  | NextSkillTest SkillTestId
  | AddSubscriber Target
  | WithSource Source Message
  | SetInvestigator PlayerId Investigator
  | ResolvedAbility Ability -- INTERNAL, See Arbiter of Fates
  | -- Story Card Messages
    ReadStory InvestigatorId Card StoryMode (Maybe Target)
  | ReadStoryWithPlacement InvestigatorId Card StoryMode (Maybe Target) Placement
  | ResolveStory InvestigatorId StoryMode StoryId
  | ResolvedStory StoryMode StoryId
  | PlaceStory Card Placement
  | -- | ResolveStoryStep InvestigatorId StoryId Int
    RemoveStory StoryId
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
  | AddCampaignCardToDeck InvestigatorId Card
  | RemoveCardFromDeckForCampaign InvestigatorId PlayerCard
  | AddCardToDeckForCampaign InvestigatorId PlayerCard
  | -- Adding Cards to Hand
    AddFocusedToHand InvestigatorId Target Zone CardId
  | AddToHand InvestigatorId [Card]
  | DebugAddToHand InvestigatorId CardId
  | DrawFocusedToHand InvestigatorId Target Zone CardId
  | DrawToHand InvestigatorId [Card]
  | DrawToHandFrom InvestigatorId DeckSignifier [Card]
  | AddToHandQuiet InvestigatorId [Card] -- used for playing cards
  | ReturnToHand InvestigatorId Target
  | -- Adding Cards to Deck
    AddFocusedToTopOfDeck InvestigatorId Target CardId
  | -- Adding Cards to Player Discard
    AddToDiscard InvestigatorId PlayerCard
  | AddToEncounterDiscard EncounterCard
  | -- Slot Messages
    AddSlot InvestigatorId SlotType Slot
  | RemoveSlot InvestigatorId SlotType
  | RemoveSlotFrom InvestigatorId Source SlotType
  | -- Adding Cards to Encounter Deck
    AddToEncounterDeck EncounterCard
  | -- Scenario Deck Messages
    AddToScenarioDeck ScenarioDeckKey Target
  | AddCardToScenarioDeck ScenarioDeckKey Card
  | ShuffleScenarioDeckIntoEncounterDeck ScenarioDeckKey
  | DrawStartingHand InvestigatorId
  | DrawCards InvestigatorId (CardDraw Message)
  | DoDrawCards InvestigatorId
  | DrawEnded InvestigatorId
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
    AddToVictory Target
  | DefeatedAddToVictory Target
  | -- Tokens
    AddChaosToken ChaosTokenFace
  | SwapChaosToken ChaosTokenFace ChaosTokenFace
  | RemoveAllChaosTokens ChaosTokenFace
  | RemoveChaosToken ChaosTokenFace
  | -- Asset Uses
    AddUses Source AssetId UseType Int
  | -- Asks
    AskPlayer Message
  | Ask PlayerId (Question Message)
  | WindowAsk [Window] PlayerId (Question Message)
  | AskMap (Map PlayerId (Question Message))
  | After Message -- TODO: REMOVE
  | AfterEvadeEnemy InvestigatorId EnemyId
  | AfterRevelation InvestigatorId TreacheryId
  | AllCheckHandSize
  | AllDrawCardAndResource
  | AllDrawEncounterCard
  | AllInvestigatorsResigned
  | AllRandomDiscard Source CardMatcher
  | DealAssetDamageWithCheck AssetId Source Int Int Bool
  | DealAssetDirectDamage AssetId Source Int Int
  | AssignAssetDamageWithCheck AssetId Source Int Int Bool
  | AssetDefeated Source AssetId
  | -- Attach
    AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AttachStoryTreacheryTo TreacheryId Card Target
  | AttackEnemy SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType
  | BeforeRevealChaosTokens
  | AfterRevealChaosTokens
  | BeforeSkillTest SkillTestId
  | ChangeSkillTestType SkillTestType SkillTestBaseValue
  | IncreaseSkillTestDifficulty Int
  | -- Game State Control
    BeginGame
  | Begin Phase
  | Again Message -- if we repeat the investigation phase we need to reset actions
  | PhaseStep PhaseStep [Message]
  | BeginRound
  | ReplaceSkillTestSkill FromSkillType ToSkillType
  | BeginSkillTestWithPreMessages Bool [Message] SkillTest
  | BeginSkillTestWithPreMessages' [Message] SkillTest
  | RepeatSkillTest SkillTestId SkillTestId
  | BeginSkillTestAfterFast
  | SetSkillTestTarget Target
  | SetSkillTestResolveFailureInvestigator InvestigatorId
  | BeginTrade InvestigatorId Source Target [InvestigatorId]
  | BeginTurn InvestigatorId
  | Blanked Message
  | HandleOption CampaignOption
  | CampaignStep CampaignStep
  | CancelEachNext Source [MessageType]
  | CancelSkillEffects -- used by scenarios to cancel skill cards
  | CancelHorror InvestigatorId Int
  | CancelDamage InvestigatorId Int
  | CancelAssetDamage AssetId Source Int
  | CheckAttackOfOpportunity InvestigatorId Bool
  | CheckDefeated Source Target
  | AssignDamage Target
  | CancelAssignedDamage Target Int Int
  | AssignedDamage Target
  | AssignedHealing Target
  | CheckHandSize InvestigatorId
  | CheckWindows [Window]
  | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
  | ChooseAndDiscardAsset InvestigatorId Source AssetMatcher
  | DiscardFromHand (HandDiscard Message)
  | DoneDiscarding InvestigatorId
  | DiscardCard InvestigatorId Source CardId
  | ChooseEndTurn InvestigatorId
  | ChooseEvadeEnemy ChooseEvade
  | ChooseFightEnemy ChooseFight
  | ChooseEngageEnemy InvestigatorId Source (Maybe Target) EnemyMatcher Bool -- If we add ChooseEngageEnemy update Tony Morgan
  | ChooseLeadInvestigator
  | PreScenarioSetup
  | StandaloneSetup
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | ChoosePlayerOrder InvestigatorId [InvestigatorId] [InvestigatorId]
  | ChooseRandomLocation Target [LocationId]
  | ChosenRandomLocation Target LocationId
  | ChooseChaosTokenGroups Source InvestigatorId ChaosBagStep
  | CommitCard InvestigatorId Card
  | CommitToSkillTest SkillTestId (UI Message)
  | Continue Text
  | CreateEffect EffectBuilder
  | ObtainCard CardId
  | ObtainChaosToken ChaosToken
  | CreateEnemy (EnemyCreation Message)
  | CreateSkill SkillId Card InvestigatorId Placement
  | CreatedEnemyAt EnemyId LocationId Target
  | -- new payment bs
    PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | CancelCost ActiveCostId
  | SetCost ActiveCostId Cost
  | PaySealCost InvestigatorId CardId Cost
  | PayAdditionalCost InvestigatorId BatchId Cost
  | CheckAdditionalCosts ActiveCostId
  | PayCosts ActiveCostId
  | PayCost ActiveCostId InvestigatorId Bool Cost
  | PayCostFinished ActiveCostId
  | PaidCost ActiveCostId InvestigatorId (Maybe Action) Payment
  | PaidAllCosts
  | -- end  new payment bs
    CreateWindowModifierEffect EffectWindow (EffectMetadata Window Message) Source Target
  | CreateChaosTokenEffect (EffectMetadata Window Message) Source ChaosToken
  | CreateOnRevealChaosTokenEffect SkillTestId ChaosTokenMatcher Source Target [Message]
  | CreateEndOfTurnEffect Source InvestigatorId [Message]
  | CreateEndOfRoundEffect Source [Message]
  | CreateAssetAt AssetId Card Placement
  | CreateEventAt InvestigatorId Card Placement
  | PlaceAsset AssetId Placement
  | PlaceEvent EventId Placement
  | PlaceTreachery TreacheryId Placement
  | PlaceSkill SkillId Placement
  | PlaceKey Target ArkhamKey
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateChaosTokenValueEffect SkillTestId Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Window Message)) Source Target
  | UpdateEffectMeta EffectId (EffectMetadata Window Message)
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
  | DiscardedCard CardId
  | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Source Target
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Source Target
  | DiscoverClues InvestigatorId Discover
  | DisengageEnemy InvestigatorId EnemyId
  | DisengageEnemyFromAll EnemyId
  | DrawAnotherChaosToken InvestigatorId
  | DrawChaosToken InvestigatorId ChaosToken
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
  | SearchEnded InvestigatorId
  | CancelSearch InvestigatorId
  | EndTurn InvestigatorId
  | EndUpkeep
  | EnemiesAttack
  | EnemyWillAttack EnemyAttackDetails
  | EnemyAttack EnemyAttackDetails
  | HandleElusive EnemyId
  | InitiateEnemyAttack EnemyAttackDetails
  | PerformEnemyAttack EnemyId -- Internal
  | AfterEnemyAttack EnemyId [Message]
  | EnemyAttackFromDiscard InvestigatorId Source Card
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | EnemyAttacks [Message]
  | ChangeEnemyAttackTarget EnemyId Target
  | ChangeEnemyAttackDetails EnemyId EnemyAttackDetails
  | CheckEnemyEngagement InvestigatorId
  | EnemyCheckEngagement EnemyId
  | EnemyDamage EnemyId DamageAssignment
  | EnemyDamaged EnemyId DamageAssignment -- INTERNAL ONLY
  | DefeatEnemy EnemyId InvestigatorId Source -- use `defeatEnemy`
  | EnemyDefeated EnemyId CardId Source [Trait]
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyMove EnemyId LocationId
  | EnemyEntered EnemyId LocationId
  | SetBearer Target InvestigatorId
  | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
  | EnemySpawnEngagedWithPrey EnemyId
  | EnemySpawnEngagedWith EnemyId InvestigatorMatcher
  | EnemySpawnFromOutOfPlay OutOfPlayZone (Maybe InvestigatorId) LocationId EnemyId
  | EngageEnemy InvestigatorId EnemyId (Maybe Target) Bool
  | EvadeEnemy SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | Exhaust Target
  | ExhaustThen Target [Message]
  | FailSkillTest
  | FailedAttackEnemy InvestigatorId EnemyId
  | FailedSkillTest InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | ChoseEnemy SkillTestId InvestigatorId Source EnemyId
  | FightEnemy SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | FindAndDrawEncounterCard InvestigatorId CardMatcher IncludeDiscard
  | FindEncounterCard InvestigatorId Target [ScenarioZone] CardMatcher
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
  | GainXP InvestigatorId Source Int
  | SpendXP InvestigatorId Int
  | GameOver
  | HandlePointOfFailure InvestigatorId Target Int -- Really do x n times, does not have to be failure
  | ApplyHealing Source
  | HealAllDamage Target Source
  | HealAllHorror Target Source
  | ReportXp XpBreakdown
  | HealAllDamageAndHorror Target Source
  | ExcessHealDamage InvestigatorId Source Int
  | ExcessHealHorror InvestigatorId Source Int
  | HealDamage Target Source Int
  | HealHorror Target Source Int
  | HealDamageDelayed Target Source Int
  | HealHorrorDelayed Target Source Int
  | ReassignHorror Source Target Int
  | HealHorrorWithAdditional Target Source Int
  | AdditionalHealHorror Target Source Int
  | HealDamageDirectly Target Source Int
  | HealHorrorDirectly Target Source Int
  | HuntersMove
  | HunterMove EnemyId
  | PatrolMove EnemyId LocationMatcher
  | InDiscard InvestigatorId Message -- Nothing uses this yet
  | InSearch Message
  | InHand InvestigatorId Message
  | InitDeck InvestigatorId (Maybe Text) (Deck PlayerCard) -- used to initialize the deck for the campaign
  | LoadSideDeck InvestigatorId [PlayerCard] -- used to initialize the side deck for the campaign
  | LoadDecklist PlayerId ArkhamDBDecklist
  | UpgradeDeck InvestigatorId (Maybe Text) (Deck PlayerCard) -- used to upgrade deck during campaign
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
  | -- | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
    CheckAdditionalActionCosts InvestigatorId Target Action [Message]
  | CheckAllAdditionalCommitCosts
  | CheckAdditionalCommitCosts InvestigatorId [Card]
  | -- Maybe Target is handler for success
    Investigate Investigate
  | -- | uses the internal method and then checks defeat
    InvestigatorAssignDamage InvestigatorId Source DamageStrategy Int Int
  | InvestigatorCommittedCard InvestigatorId Card
  | InvestigatorCommittedSkill InvestigatorId SkillId
  | InvestigatorDamage InvestigatorId Source Int Int
  | InvestigatorDamageEnemy InvestigatorId EnemyId Source
  | InvestigatorDamageInvestigator InvestigatorId InvestigatorId
  | InvestigatorDefeated Source InvestigatorId
  | InvestigatorIsDefeated Source InvestigatorId
  | InvestigatorDirectDamage InvestigatorId Source Int Int
  | InvestigatorDiscardAllClues Source InvestigatorId
  | -- | meant to be used internally by investigators                  ^ damage ^ horror
    InvestigatorDoAssignDamage
      InvestigatorId
      Source
      DamageStrategy
      AssetMatcher
      Int
      Int
      [Target]
      [Target]
  | InvestigatorDrawEnemy InvestigatorId EnemyId
  | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
  | InvestigatorDrewPlayerCardFrom InvestigatorId PlayerCard (Maybe DeckSignifier)
  | InvestigatorEliminated InvestigatorId
  | InvestigatorKilled Source InvestigatorId
  | InvestigatorMulligan InvestigatorId
  | InvestigatorsMulligan
  | -- | This message exists in case the number of clues will change
    InvestigatorPlaceAllCluesOnLocation InvestigatorId Source
  | InvestigatorPlaceCluesOnLocation InvestigatorId Source Int
  | InvestigatorPlayAsset InvestigatorId AssetId
  | InvestigatorClearUnusedAssetSlots InvestigatorId
  | InvestigatorAdjustAssetSlots InvestigatorId AssetId
  | InvestigatorAdjustSlot InvestigatorId Slot SlotType SlotType
  | InvestigatorPlayedAsset InvestigatorId AssetId
  | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target) [Window] Zone
  | FinishedEvent EventId
  | InvestigatorResigned InvestigatorId
  | InvestigatorSpendClues InvestigatorId Int
  | InvestigatorWhenDefeated Source InvestigatorId
  | InvestigatorWhenEliminated Source InvestigatorId (Maybe Message)
  | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
  | LookAtRevealed InvestigatorId Source Target
  | LookAtTopOfDeck InvestigatorId Target Int
  | LoseActions InvestigatorId Source Int
  | LoseResources InvestigatorId Source Int
  | LoseAllResources InvestigatorId Source
  | SpendActions InvestigatorId Source [Action] Int
  | -- | Handles complex movement for a target, triggers Moves windows, and uses MoveFrom, MoveTo messages
    Move Movement
  | -- | When bool is True, This triggers the windows for PerformAction, as
    -- well as BeginAction and Finish action brackets.
    -- When bool is false it only triggers the after move action window, but
    -- also the (When, After) messages
    -- Eventually calls the Move message with a movement it built
    MoveAction InvestigatorId LocationId Cost Bool
  | -- | Only calls MoveTo for all investigators
    MoveAllTo Source LocationId
  | -- | Pretty useless, simply triggers Will/After variants that just push
    -- windows, possibly we could inline this, the only benefit seems to be the
    -- ability to cancel the batch easily
    MoveFrom Source InvestigatorId LocationId
  | -- | Actual movement, will add MovedBy, MovedBut, and after Entering windows
    MoveTo Movement
  | -- | Move target one location toward a matching location
    MoveToward Target LocationMatcher
  | -- | Move target one location at a time until arrive at location
    MoveUntil LocationId Target
  | WhenCanMove InvestigatorId [Message]
  | MoveAllCluesTo Source Target
  | MoveTopOfDeckToBottom Source DeckSignifier Int
  | NextCampaignStep (Maybe CampaignStep)
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | Noop
  | PassSkillTest
  | PassSkillTestBy Int
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId Card [Window]
  | PaidForCardCost InvestigatorId Card Payment
  | PayForCardAbility InvestigatorId Source [Window] Int Payment
  | PlaceCluesUpToClueValue LocationId Source Int
  | FlipClues Target Int
  | FlipDoom Target Int
  | PlaceAdditionalDamage Target Source Int Int
  | PlaceDoomOnAgenda Int CanAdvance
  | PlaceEnemyOutOfPlay OutOfPlayZone EnemyId
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Card
  | PlaceLocationMatching CardMatcher
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | ClearTokens Target
  | MoveTokens Source Source Target Token Int
  | MoveTokensNoDefeated Source Source Target Token Int
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
  | PlayerWindow InvestigatorId [UI Message] Bool
  | PutCampaignCardIntoPlay InvestigatorId CardDef
  | PutCardIntoPlay InvestigatorId Card (Maybe Target) Payment [Window]
  | PutCardIntoPlayWithAdditionalCosts InvestigatorId Card (Maybe Target) Payment [Window]
  | PutCardOnTopOfDeck InvestigatorId DeckSignifier Card
  | PutOnTopOfDeck InvestigatorId DeckSignifier Target
  | PutCardOnBottomOfDeck InvestigatorId DeckSignifier Card
  | PutOnBottomOfDeck InvestigatorId DeckSignifier Target
  | Ready Target
  | ReadyAlternative Source Target
  | ReadyExhausted
  | Record CampaignLogKey
  | RecordForInvestigator InvestigatorId CampaignLogKey
  | RecordCount CampaignLogKey Int
  | IncrementRecordCount CampaignLogKey Int
  | RecordSetInsert CampaignLogKey [SomeRecorded]
  | RecordSetReplace CampaignLogKey SomeRecorded SomeRecorded
  | CrossOutRecordSetEntries CampaignLogKey [SomeRecorded]
  | RefillSlots InvestigatorId
  | Remember ScenarioLogKey
  | ScenarioCountSet ScenarioCountKey Int
  | ScenarioCountIncrementBy ScenarioCountKey Int
  | ScenarioCountDecrementBy ScenarioCountKey Int
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemoveAllCopiesOfEncounterCardFromGame CardMatcher
  | RemovePlayerCardFromGame Bool Card
  | RemoveAllTokens Source Target
  | RemoveAllAttachments Source Target
  | RemoveAllClues Source Target
  | RemoveAllDoomFromPlay RemoveDoomMatchers
  | RemoveAllDoom Source Target
  | RemoveCampaignCard CardDef
  | RemoveCampaignCardFromDeck InvestigatorId CardDef
  | RemoveCardFromHand InvestigatorId CardId
  | RemoveCardFromSearch InvestigatorId CardId
  | RemoveDiscardFromGame InvestigatorId
  | RemoveAsset AssetId
  | RemoveEnemy EnemyId
  | RemoveEvent EventId
  | RemoveSkill SkillId
  | RemoveTreachery TreacheryId
  | RemoveFromDiscard InvestigatorId CardId
  | RemoveFromEncounterDiscard EncounterCard
  | RemoveFromEncounterDeck EncounterCard
  | RemoveFromGame Target
  | QuietlyRemoveFromGame Target
  | RemoveCompletedActFromGame Int ActId
  | RemovedFromGame Card
  | RemoveLocation LocationId
  | RemovedLocation LocationId
  | SetCardAside Card
  | PlaceInvestigator InvestigatorId Placement
  | PlaceInBonded InvestigatorId Card
  | RemoveFromPlay Source
  | RemovedFromPlay Source
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
  | ReplaceEntireDraw Source InvestigatorId ChaosBagStep
  | SetChaosBagChoice Source InvestigatorId ChaosBagStep -- internal
  | RequestSetAsideCard Source CardCode
  | RequestChaosTokens Source (Maybe InvestigatorId) RevealStrategy RequestedChaosTokenStrategy
  | RequestedEncounterCard Source (Maybe InvestigatorId) (Maybe EncounterCard)
  | RequestedEncounterCards Target [EncounterCard]
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard) [PlayerCard]
  | RequestedSetAsideCard Source Card
  | RequestedChaosTokens Source (Maybe InvestigatorId) [ChaosToken]
  | RerunSkillTest
  | ResetInvestigators
  | ResetGame
  | ReloadDecks
  | ResetChaosTokens Source
  | Reset Target
  | ReturnChaosTokensToPool [ChaosToken]
  | Resign InvestigatorId
  | ResignWith Target
  | ResolveAmounts InvestigatorId [(NamedUUID, Int)] Target
  | ResolveEvent InvestigatorId EventId (Maybe Target) [Window]
  | ResolveEventChoice InvestigatorId EventId Int (Maybe Target) [Window]
  | ResolveSkill SkillId
  | ResolveChaosToken ChaosToken ChaosTokenFace InvestigatorId -- since tokens can have their face changed we use this to represent that; TODO: use a real modifier
  | TargetResolveChaosToken Target ChaosToken ChaosTokenFace InvestigatorId -- since tokens can have their face changed we use this to represent that; TODO: use a real modifier
  | ReturnSkillTestRevealedChaosTokens
  | ReturnChaosTokens [ChaosToken]
  | RevealCard CardId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | UnrevealLocation LocationId
  | RevealSkillTestChaosTokens InvestigatorId
  | RevealSkillTestChaosTokensAgain InvestigatorId -- meant for when we reveal during resolving
  | RevealChaosToken Source InvestigatorId ChaosToken
  | Revelation InvestigatorId Source
  | RevelationChoice InvestigatorId Source Int
  | RevelationSkillTest SkillTestId InvestigatorId Source SkillType SkillTestDifficulty
  | ResolveRevelation InvestigatorId Card
  | Run [Message]
  | RunBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunSkillTest InvestigatorId
  | RecalculateSkillTestResults
  | RecalculateSkillTestResultsCanChangeAutomatic Bool
  | RemoveFromBearersDeckOrDiscard PlayerCard
  | SearchCollectionForRandom InvestigatorId Source CardMatcher
  | FinishedSearch
  | Search
      SearchType
      InvestigatorId
      Source
      Target
      [(Zone, ZoneReturnStrategy)]
      ExtendedCardMatcher
      FoundCardsStrategy
  | ResolveSearch InvestigatorId
  | SearchFound InvestigatorId Target DeckSignifier [Card]
  | FoundCards (Map Zone [Card])
  | SearchNoneFound InvestigatorId Target
  | UpdateSearchReturnStrategy InvestigatorId Zone ZoneReturnStrategy
  | SetActions InvestigatorId Source Int
  | SetEncounterDeck (Deck EncounterCard)
  | SetLayout [GridTemplateRow]
  | SetLocationLabel LocationId Text
  | SetRole InvestigatorId ClassSymbol
  | ForceChaosTokenDraw ChaosTokenFace
  | SetActiveInvestigator InvestigatorId
  | SetActivePlayer PlayerId
  | SetChaosTokens [ChaosTokenFace]
  | SetChaosTokensForScenario
  | Setup
  | EndSetup
  | SetupInvestigators
  | SetupInvestigator InvestigatorId
  | SetupStep Target Int
  | ShuffleAllFocusedIntoDeck InvestigatorId Target
  | PutAllFocusedIntoDiscard InvestigatorId Target
  | ShuffleAllInEncounterDiscardBackIn CardCode
  | ShuffleBackIntoEncounterDeck Target
  | ShuffleCardsIntoDeck DeckSignifier [Card]
  | ShuffleDiscardBackIn InvestigatorId
  | ShuffleEncounterDiscardBackIn
  | ShuffleDeck DeckSignifier
  | ShuffleIntoDeck DeckSignifier Target
  | ShuffleCardsIntoTopOfDeck DeckSignifier Int [Card]
  | SkillTestApplyResults
  | SkillTestApplyResultsAfter
  | SkillTestAsk Message
  | SkillTestCommitCard InvestigatorId Card
  | SkillTestEnds SkillTestId InvestigatorId Source
  | SkillTestEnded SkillTestId
  | AfterSkillTestEnds Source Target SkillTest.SkillTestResult
  | EndSkillTestWindow
  | SkillTestResults SkillTestResultsData
  | SkillTestUncommitCard InvestigatorId Card
  | SpawnEnemyAt Card LocationId
  | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
  | SpendClues Int [InvestigatorId]
  | SpendResources InvestigatorId Int
  | SpendUses Source Target UseType Int
  | SpentAllUses Target
  | StartCampaign
  | StartScenario ScenarioId
  | RestartScenario
  | StartSkillTest InvestigatorId
  | SkippedWindow InvestigatorId
  | -- There are two targets, one associated to the action and one
    -- to handle the result
    Successful (Action, Target) InvestigatorId Source Target Int
  | Failed (Action, Target) InvestigatorId Source Target Int
  | SufferTrauma InvestigatorId Int Int
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
  | ChosenEvadeEnemy SkillTestId Source EnemyId
  | TriggerSkillTest InvestigatorId
  | TryEvadeEnemy SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType
  | UnfocusCards
  | ClearFound Zone
  | UnfocusChaosTokens
  | SealChaosToken ChaosToken
  | SealedChaosToken ChaosToken Target
  | SetChaosTokenAside ChaosToken -- see: Favor of the Moon (1)
  | UnsealChaosToken ChaosToken
  | ChaosTokenIgnored InvestigatorId Source ChaosToken
  | ChaosTokenCanceled InvestigatorId Source ChaosToken
  | SetActiveCard Card
  | UnsetActiveCard
  | AddCardEntity Card
  | RemoveCardEntity Card
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
  | WillMoveEnemy EnemyId Message
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
  | SetScenarioMeta Value
  | ScenarioSpecific Text Value
  | SetCampaignMeta Value
  | DoStep Int Message
  | ForInvestigator InvestigatorId Message
  | ForTarget Target Message
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
  | PerformReading TarotReading
  | DrawAndChooseTarot InvestigatorId TarotCardFacing Int
  | PlaceTarot InvestigatorId TarotCard
  | FocusTarotCards [TarotCard]
  | UnfocusTarotCards
  | RotateTarot TarotCard
  | Incursion LocationId
  | -- The Dream Eaters
    PlaceSwarmCards InvestigatorId EnemyId Int
  | PlacedSwarmCard EnemyId Card
  | UpdateLocation LocationId (Update Location)
  | UpdateEnemy EnemyId (Update Enemy)
  | If WindowType [Message]
  | SendMessage Target Message
  | IfEnemyExists EnemyMatcher [Message]
  | ExcessDamage EnemyId [Message]
  | AddDeckBuildingAdjustment InvestigatorId DeckBuildingAdjustment
  | IncreaseCustomization InvestigatorId CardCode Customization [CustomizationChoice]
  | ChoosingDecks
  | DoneChoosingDecks
  | SetPartnerStatus CardCode PartnerStatus
  | -- Commit
    Do Message
  | DoBatch BatchId Message
  | -- UI
    ClearUI
  deriving stock (Show, Eq, Data)

$(deriveToJSON defaultOptions ''Message)

instance FromJSON Message where
  parseJSON = withObject "Message" \o -> do
    t :: Text <- o .: "tag"
    case t of
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
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left (iid, card :: Card) -> pure $ AddCampaignCardToDeck iid card
          Right (iid, cardDef :: CardDef) -> pure $ AddCampaignCardToDeck iid (lookupCard cardDef.cardCode (unsafeMakeCardId nil))
      "SealedChaosToken" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left (token, card :: Card) -> pure $ SealedChaosToken token (toTarget card)
          Right (token, target) -> pure $ SealedChaosToken token target
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
      "LoseAllResources" -> do
        contents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        case contents of
          Left i -> pure $ LoseAllResources i GameSource
          Right (i, s) -> pure $ LoseAllResources i s
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
      _ -> $(mkParseJSON defaultOptions ''Message) (Object o)

stepMessage :: Int -> Message -> Message
stepMessage n = \case
  UseCardAbility iid source idx ws payment ->
    UseCardAbilityStep iid source idx ws payment n
  other -> other

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TooltipLabel _ _ msgs -> Run msgs
  CardLabel _ msgs -> Run msgs
  PortraitLabel _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  GridLabel _ msgs -> Run msgs
  TarotLabel _ msgs -> Run msgs
  SkillLabel _ msgs -> Run msgs
  SkillLabelWithLabel _ _ msgs -> Run msgs
  EvadeLabel _ msgs -> Run msgs
  FightLabel _ msgs -> Run msgs
  EngageLabel _ msgs -> Run msgs
  AbilityLabel iid ab windows before msgs -> Run $ before <> [UseAbility iid ab windows] <> msgs
  ComponentLabel _ msgs -> Run msgs
  EndTurnButton _ msgs -> Run msgs
  StartSkillTestButton iid -> Run [StartSkillTest iid]
  SkillTestApplyResultsButton -> Run [SkillTestApplyResults]
  ChaosTokenGroupChoice source iid step -> Run [ChooseChaosTokenGroups source iid step]
  EffectActionButton _ _ msgs -> Run msgs
  Done _ -> Run []
  SkipTriggersButton iid -> Run [SkippedWindow iid]
  CardPile _ msgs -> Run msgs

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
chooseOne _ [] = error "No messages for chooseOne"
chooseOne pid msgs = Ask pid (ChooseOne msgs)

chooseOneFromEach :: HasCallStack => PlayerId -> [[UI Message]] -> Message
chooseOneFromEach _ [] = error "No messages for chooseOne"
chooseOneFromEach pid msgs = Ask pid (ChooseOneFromEach msgs)

chooseOneDropDown :: PlayerId -> [(Text, Message)] -> Message
chooseOneDropDown _ [] = throw $ InvalidState "No messages for chooseOne"
chooseOneDropDown pid msgs = Ask pid (DropDown msgs)

chooseOneAtATime :: PlayerId -> [UI Message] -> Message
chooseOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOneAtATime pid msgs = Ask pid (ChooseOneAtATime msgs)

chooseOrRunOneAtATime :: PlayerId -> [UI Message] -> Message
chooseOrRunOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
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
  amountChoices rs = map toAmountChoice (zip rs choiceMap)
  toAmountChoice (choiceId, (l, (m, n))) = AmountChoice choiceId l m n

chooseUpgradeDeck :: PlayerId -> Message
chooseUpgradeDeck pid = Ask pid ChooseUpgradeDeck

chooseDecks :: [PlayerId] -> Message
chooseDecks pids =
  Run
    [ SetGameState (IsChooseDecks pids)
    , ChoosingDecks
    , AskMap $ mapFromList $ map (,ChooseDeck) pids
    , DoneChoosingDecks
    ]

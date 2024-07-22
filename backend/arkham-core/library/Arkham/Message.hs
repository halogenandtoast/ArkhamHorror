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
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
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
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard.Source
import Arkham.Enemy.Creation
import {-# SOURCE #-} Arkham.Enemy.Types
import Arkham.Evade.Types
import Arkham.Exception
import Arkham.Field
import Arkham.Fight.Types
import Arkham.Helpers
import Arkham.History
import Arkham.Id
import Arkham.Investigate.Types
import {-# SOURCE #-} Arkham.Investigator
import Arkham.Key
import Arkham.Layout
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
messageType RunWindow {} = Just RunWindowMessage
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
    (mapFromList [(pid, Read flavor [Label "$continue" []]) | pid <- pids])

storyWithChooseOne :: PlayerId -> [PlayerId] -> FlavorText -> [UI Message] -> Message
storyWithChooseOne lead pids flavor choices =
  AskMap
    (mapFromList [(pid, Read flavor $ if pid == lead then choices else []) | pid <- pids])

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

createCardEffect
  :: (Sourceable source, Targetable target)
  => CardDef
  -> Maybe (EffectMetadata Window Message)
  -> source
  -> target
  -> Message
createCardEffect def mMeta (toSource -> source) (toTarget -> target) = CreateEffect (toCardCode def) mMeta source target

getChoiceAmount :: Text -> [(Text, Int)] -> Int
getChoiceAmount key choices =
  let choicesMap = mapFromList @(Map Text Int) choices
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

pattern AssetDamage :: AssetId -> Source -> Int -> Int -> Message
pattern AssetDamage aid source damage horror <- AssetDamageWithCheck aid source damage horror True
  where
    AssetDamage aid source damage horror = AssetDamageWithCheck aid source damage horror True

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
  | MoveWithSkillTest Message
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
  | ReplacedLocation LocationId LocationId
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
  | AddCampaignCardToDeck InvestigatorId CardDef
  | RemoveCardFromDeckForCampaign InvestigatorId PlayerCard
  | AddCardToDeckForCampaign InvestigatorId PlayerCard
  | -- Adding Cards to Hand
    AddFocusedToHand InvestigatorId Target Zone CardId
  | AddToHand InvestigatorId [Card]
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
  | AskMap (Map PlayerId (Question Message))
  | After Message -- TODO: REMOVE
  | AfterEvadeEnemy InvestigatorId EnemyId
  | AfterRevelation InvestigatorId TreacheryId
  | AllCheckHandSize
  | AllDrawCardAndResource
  | AllDrawEncounterCard
  | AllInvestigatorsResigned
  | AllRandomDiscard Source CardMatcher
  | AssetDamageWithCheck AssetId Source Int Int Bool
  | AssetDefeated AssetId
  | -- Attach
    AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AttachStoryTreacheryTo TreacheryId Card Target
  | AttackEnemy SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType
  | BeforeRevealChaosTokens
  | BeforeSkillTest SkillTest
  | ChangeSkillTestType SkillTestType SkillTestBaseValue
  | -- Game State Control
    BeginGame
  | Begin Phase
  | Again Message -- if we repeat the investigation phase we need to reset actions
  | PhaseStep PhaseStep [Message]
  | BeginRound
  | ReplaceSkillTestSkill FromSkillType ToSkillType
  | BeginSkillTestWithPreMessages Bool [Message] SkillTest
  | BeginSkillTestWithPreMessages' [Message] SkillTest
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
  | CheckWindow [InvestigatorId] [Window]
  | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
  | RunWindow InvestigatorId [Window]
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
  | CommitToSkillTest SkillTest (UI Message)
  | Continue Text
  | CreateEffect CardCode (Maybe (EffectMetadata Window Message)) Source Target
  | ObtainCard Card
  | CreateEnemy (EnemyCreation Message)
  | CreateSkill SkillId Card InvestigatorId Placement
  | CreatedEnemyAt EnemyId LocationId Target
  | -- new payment bs
    PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | CancelCost ActiveCostId
  | SetCost ActiveCostId Cost
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
  | CreateAssetAt AssetId Card Placement
  | CreateEventAt InvestigatorId Card Placement
  | PlaceAsset AssetId Placement
  | PlaceEvent InvestigatorId EventId Placement
  | PlaceTreachery TreacheryId Placement
  | PlaceSkill SkillId Placement
  | PlaceKey Target ArkhamKey
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateChaosTokenValueEffect SkillTestId Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Window Message)) Source Target
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
  | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnedAt LocationId EnemyId
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
  | FoundEnemyInVoid InvestigatorId Target EnemyId
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
  | HealAllDamageAndHorror Target Source
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
  | InOutOfPlay Message
  | InitDeck InvestigatorId (Deck PlayerCard) -- used to initialize the deck for the campaign
  | LoadDecklist PlayerId ArkhamDBDecklist
  | UpgradeDeck InvestigatorId (Deck PlayerCard) -- used to upgrade deck during campaign
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
  | InvestigatorDrewPlayerCard InvestigatorId PlayerCard
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
  | LoseAllResources InvestigatorId
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
  | PlaceEnemyInVoid EnemyId
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Card
  | PlaceLocationMatching CardMatcher
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | MoveTokens Source Source Target Token Int
  | PlaceUnderneath Target [Card]
  | PlacedUnderneath Target Card
  | PlaceNextTo Target [Card]
  | PlacedLocation Name CardCode LocationId
  | PlacedLocationDirection LocationId Direction LocationId
  | LocationMoved LocationId
  | PlayCard InvestigatorId Card (Maybe Target) Payment [Window] Bool
  | CardEnteredPlay InvestigatorId Card
  | ResolvedCard InvestigatorId Card
  | ResolvedPlayCard InvestigatorId Card
  | PlayerWindow InvestigatorId [UI Message] Bool
  | PutCampaignCardIntoPlay InvestigatorId CardDef
  | PutCardIntoPlay InvestigatorId Card (Maybe Target) Payment [Window]
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
  | CrossOutRecordSetEntries CampaignLogKey [SomeRecorded]
  | RefillSlots InvestigatorId
  | Remember ScenarioLogKey
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
  | SetOutOfPlay OutOfPlayZone Target
  | PlaceInvestigator InvestigatorId Placement
  | PlaceInBonded InvestigatorId Card
  | DoSetOutOfPlay OutOfPlayZone Target
  | RemoveFromPlay Source
  | RemovedFromPlay Source
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
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
  | ResetChaosTokens Source
  | ReturnChaosTokensToPool [ChaosToken]
  | Resign InvestigatorId
  | ResignWith Target
  | ResolveAmounts InvestigatorId [(Text, Int)] Target
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
  | RevealChaosToken Source InvestigatorId ChaosToken
  | Revelation InvestigatorId Source
  | RevelationChoice InvestigatorId Source Int
  | RevelationSkillTest SkillTestId InvestigatorId Source SkillType SkillTestDifficulty
  | Run [Message]
  | RunBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunSkillTest InvestigatorId
  | RecalculateSkillTestResults
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
  | ReplaceInvestigatorAsset InvestigatorId AssetId Card
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
  | UnfocusTargets
  | UnfocusChaosTokens
  | SealChaosToken ChaosToken
  | SealedChaosToken ChaosToken Card
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
  | BeginCardPayment Card
  | FinishCardPayment Card
  | ReplaceCard CardId Card
  | UpdateHistory InvestigatorId HistoryItem
  | -- The Forgotten Age
    PickSupply InvestigatorId Supply
  | UseSupply InvestigatorId Supply
  | Explore InvestigatorId Source CardMatcher
  | BecomeYithian InvestigatorId
  | SetScenarioMeta Value
  | DoStep Int Message
  | ForInvestigator InvestigatorId Message
  | ForTarget Target Message
  | ForPlayer PlayerId Message
  | ForSkillType SkillType Message
  | -- The Circle Undone
    BecomePrologueInvestigator InvestigatorId InvestigatorId
  | PutLocationInFrontOf InvestigatorId LocationId
  | PutLocationInCenter LocationId
  | PlaceBreaches Target Int
  | RemoveBreaches Target Int
  | RunCosmos InvestigatorId LocationId [Message]
  | PlaceCosmos InvestigatorId LocationId (CosmosLocation Card LocationId)
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
  | -- Commit
    Do Message
  | DoBatch BatchId Message
  | -- UI
    ClearUI
  deriving stock (Show, Eq, Data)

$(deriveToJSON defaultOptions ''Message)

instance FromJSON Message where
  parseJSON = withObject "Message" $ \o -> do
    tag :: Text <- o .: "tag"
    case tag of
      "AttackEnemy" -> do
        -- SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, e, s, mt, stype) -> AttackEnemy st i e s mt stype)
            (\(i, e, s, mt, stype) -> AttackEnemy (SkillTestId nil) i e s mt stype)
            eContents
      "CreateChaosTokenValueEffect" -> do
        -- SkillTestId Int Source Target
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, s, t) -> CreateChaosTokenValueEffect st i s t)
            (\(i, s, t) -> CreateChaosTokenValueEffect (SkillTestId nil) i s t)
            eContents
      "EvadeEnemy" -> do
        -- SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, e, s, mt, stype, b) -> EvadeEnemy st i e s mt stype b)
            (\(i, e, s, mt, stype, b) -> EvadeEnemy (SkillTestId nil) i e s mt stype b)
            eContents
      "ChoseEnemy" -> do
        -- SkillTestId InvestigatorId Source EnemyId
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, s, e) -> ChoseEnemy st i s e)
            (\(i, s, e) -> ChoseEnemy (SkillTestId nil) i s e)
            eContents
      "FightEnemy" -> do
        -- SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, e, s, mt, stype, b) -> FightEnemy st i e s mt stype b)
            (\(i, e, s, mt, stype, b) -> FightEnemy (SkillTestId nil) i e s mt stype b)
            eContents
      "RevelationSkillTest" -> do
        -- SkillTestId InvestigatorId Source SkillType SkillTestDifficulty
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, s, mt, d) -> RevelationSkillTest st i s mt d)
            (\(i, s, mt, d) -> RevelationSkillTest (SkillTestId nil) i s mt d)
            eContents
      "SkillTestEnds" -> do
        -- SkillTestId InvestigatorId Source
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, s) -> SkillTestEnds st i s)
            (\(i, s) -> SkillTestEnds (SkillTestId nil) i s)
            eContents
      "SkillTestEnded" -> do
        -- SkillTestId
        st <- o .:? "contents" .!= SkillTestId nil
        pure $ Arkham.Message.SkillTestEnded st
      "ChosenEvadeEnemy" -> do
        -- SkillTestId Source EnemyId
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, s, e) -> ChosenEvadeEnemy st s e)
            (\(s, e) -> ChosenEvadeEnemy (SkillTestId nil) s e)
            eContents
      "TryEvadeEnemy" -> do
        -- SkillTestId InvestigatorId EnemyId Source (Maybe Target) SkillType
        eContents <- (Left <$> o .: "contents") <|> (Right <$> o .: "contents")
        pure
          $ either
            (\(st, i, e, s, mt, stype) -> TryEvadeEnemy st i e s mt stype)
            (\(i, e, s, mt, stype) -> TryEvadeEnemy (SkillTestId nil) i e s mt stype)
            eContents
      -- the rest of the message can be parsed normally
      "UseAbility" -> do
        contents <- o .: "contents"
        pure $ uncurry3 UseAbility contents
      "MoveWithSkillTest" -> do
        contents <- o .: "contents"
        pure $ MoveWithSkillTest contents
      "NextSkillTest" -> do
        contents <- o .: "contents"
        pure $ NextSkillTest contents
      "AddSubscriber" -> do
        contents <- o .: "contents"
        pure $ AddSubscriber contents
      "WithSource" -> do
        contents <- o .: "contents"
        pure $ uncurry WithSource contents
      "SetInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry SetInvestigator contents
      "ResolvedAbility" -> do
        contents <- o .: "contents"
        pure $ ResolvedAbility contents
      "ReadStory" -> do
        contents <- o .: "contents"
        pure $ uncurry4 ReadStory contents
      "ReadStoryWithPlacement" -> do
        contents <- o .: "contents"
        pure $ uncurry5 ReadStoryWithPlacement contents
      "ResolveStory" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ResolveStory contents
      "ResolvedStory" -> do
        contents <- o .: "contents"
        pure $ uncurry ResolvedStory contents
      "RemoveStory" -> do
        contents <- o .: "contents"
        pure $ RemoveStory contents
      "DiscardedCost" -> do
        contents <- o .: "contents"
        pure $ DiscardedCost contents
      "SetActDeck" -> pure $ SetActDeck
      "SetActDeckCards" -> do
        contents <- o .: "contents"
        pure $ uncurry SetActDeckCards contents
      "AddAct" -> do
        contents <- o .: "contents"
        pure $ uncurry AddAct contents
      "AdvanceAct" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AdvanceAct contents
      "NextAdvanceActStep" -> do
        contents <- o .: "contents"
        pure $ uncurry NextAdvanceActStep contents
      "ReplaceAct" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplaceAct contents
      "RevertAct" -> do
        contents <- o .: "contents"
        pure $ DiscardedCost contents
      "ResetActDeckToStage" -> do
        contents <- o .: "contents"
        pure $ ResetActDeckToStage contents
      "AdvanceActDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry AdvanceActDeck contents
      "AdvanceToAct" -> do
        contents <- o .: "contents"
        pure $ uncurry4 AdvanceToAct contents
      "SetCurrentActDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry SetCurrentActDeck contents
      "SetAgendaDeck" -> pure SetAgendaDeck
      "SetAgendaDeckCards" -> do
        contents <- o .: "contents"
        pure $ uncurry SetAgendaDeckCards contents
      "AddAgenda" -> do
        contents <- o .: "contents"
        pure $ uncurry AddAgenda contents
      "SetCurrentAgendaDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry SetCurrentAgendaDeck contents
      "AdvanceAgendaBy" -> do
        contents <- o .: "contents"
        pure $ uncurry AdvanceAgendaBy contents
      "AdvanceToAgenda" -> do
        contents <- o .: "contents"
        pure $ uncurry4 AdvanceToAgenda contents
      "NextAdvanceAgendaStep" -> do
        contents <- o .: "contents"
        pure $ uncurry NextAdvanceAgendaStep contents
      "AdvanceAgendaIfThresholdSatisfied" -> pure AdvanceAgendaIfThresholdSatisfied
      "AdvanceAgendaDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry AdvanceAgendaDeck contents
      "AdvanceCurrentAgenda" -> pure AdvanceCurrentAgenda
      "ReplaceLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ReplaceLocation contents
      "ReplaceEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ReplaceEnemy contents
      "ReplacedLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplacedLocation contents
      "ReplaceAgenda" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplaceAgenda contents
      "RevertAgenda" -> do
        contents <- o .: "contents"
        pure $ RevertAgenda contents
      "ResetAgendaDeckToStage" -> do
        contents <- o .: "contents"
        pure $ ResetAgendaDeckToStage contents
      "SetNoRemainingInvestigatorsHandler" -> do
        contents <- o .: "contents"
        pure $ SetNoRemainingInvestigatorsHandler contents
      "HandleNoRemainingInvestigators" -> do
        contents <- o .: "contents"
        pure $ HandleNoRemainingInvestigators contents
      "CheckForRemainingInvestigators" -> pure CheckForRemainingInvestigators
      "AddDirectConnection" -> do
        contents <- o .: "contents"
        pure $ uncurry AddDirectConnection contents
      "SetConnections" -> do
        contents <- o .: "contents"
        pure $ uncurry SetConnections contents
      "SetFlippable" -> do
        contents <- o .: "contents"
        pure $ uncurry SetFlippable contents
      "AddCampaignCardToDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry AddCampaignCardToDeck contents
      "RemoveCardFromDeckForCampaign" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCardFromDeckForCampaign contents
      "AddCardToDeckForCampaign" -> do
        contents <- o .: "contents"
        pure $ uncurry AddCardToDeckForCampaign contents
      "AddFocusedToHand" -> do
        contents <- o .: "contents"
        pure $ uncurry4 AddFocusedToHand contents
      "AddToHand" -> do
        contents <- o .: "contents"
        pure $ uncurry AddToHand contents
      "ReturnToHand" -> do
        contents <- o .: "contents"
        pure $ uncurry ReturnToHand contents
      "AddFocusedToTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AddFocusedToTopOfDeck contents
      "AddToDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry AddToDiscard contents
      "AddToEncounterDiscard" -> do
        contents <- o .: "contents"
        pure $ AddToEncounterDiscard contents
      "AddSlot" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AddSlot contents
      "RemoveSlot" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveSlot contents
      "RemoveSlotFrom" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RemoveSlotFrom contents
      "AddToEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ AddToEncounterDeck contents
      "AddToScenarioDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry AddToScenarioDeck contents
      "AddCardToScenarioDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry AddCardToScenarioDeck contents
      "ShuffleScenarioDeckIntoEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ ShuffleScenarioDeckIntoEncounterDeck contents
      "DrawStartingHand" -> do
        contents <- o .: "contents"
        pure $ DrawStartingHand contents
      "DrawCards" -> do
        contents <- o .: "contents"
        pure $ uncurry DrawCards contents
      "DoDrawCards" -> do
        contents <- o .: "contents"
        pure $ DoDrawCards contents
      "DrawEnded" -> do
        contents <- o .: "contents"
        pure $ DrawEnded contents
      "Instead" -> do
        contents <- o .: "contents"
        pure $ uncurry Instead contents
      "ReplaceCurrentCardDraw" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplaceCurrentCardDraw contents
      "DrawEncounterCards" -> do
        contents <- o .: "contents"
        pure $ uncurry DrawEncounterCards contents
      "DrewCards" -> do
        contents <- o .: "contents"
        pure $ uncurry DrewCards contents
      "ChooseFrom" -> do
        contents <- o .: "contents"
        pure $ uncurry ChooseFrom contents
      "ChoseCards" -> do
        contents <- o .: "contents"
        pure $ uncurry ChoseCards contents
      "SetScenarioDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry SetScenarioDeck contents
      "RemoveCardFromScenarioDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCardFromScenarioDeck contents
      "SwapPlaces" -> do
        contents <- o .: "contents"
        pure $ uncurry SwapPlaces contents
      "AddToVictory" -> do
        contents <- o .: "contents"
        pure $ AddToVictory contents
      "DefeatedAddToVictory" -> do
        contents <- o .: "contents"
        pure $ DefeatedAddToVictory contents
      "AddChaosToken" -> do
        contents <- o .: "contents"
        pure $ AddChaosToken contents
      "SwapChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry SwapChaosToken contents
      "RemoveAllChaosTokens" -> do
        contents <- o .: "contents"
        pure $ RemoveAllChaosTokens contents
      "RemoveChaosToken" -> do
        contents <- o .: "contents"
        pure $ RemoveChaosToken contents
      "AddUses" -> do
        contents <- o .: "contents"
        pure $ uncurry4 AddUses contents
      "AskPlayer" -> do
        contents <- o .: "contents"
        pure $ AskPlayer contents
      "Ask" -> do
        contents <- o .: "contents"
        pure $ uncurry Ask contents
      "AskMap" -> do
        contents <- o .: "contents"
        pure $ AskMap contents
      "After" -> do
        contents <- o .: "contents"
        pure $ After contents
      "AfterEvadeEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry AfterEvadeEnemy contents
      "AfterRevelation" -> do
        contents <- o .: "contents"
        pure $ uncurry AfterRevelation contents
      "AllCheckHandSize" -> pure AllCheckHandSize
      "AllDrawCardAndResource" -> pure AllDrawCardAndResource
      "AllDrawEncounterCard" -> pure AllDrawEncounterCard
      "AllInvestigatorsResigned" -> pure AllInvestigatorsResigned
      "AllRandomDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry AllRandomDiscard contents
      "AssetDamageWithCheck" -> do
        contents <- o .: "contents"
        pure $ uncurry5 AssetDamageWithCheck contents
      "AssetDefeated" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.AssetDefeated contents
      "AttachAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry AttachAsset contents
      "AttachEvent" -> do
        contents <- o .: "contents"
        pure $ uncurry AttachEvent contents
      "AttachStoryTreacheryTo" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AttachStoryTreacheryTo contents
      "BeforeRevealChaosTokens" -> pure BeforeRevealChaosTokens
      "BeforeSkillTest" -> do
        contents <- o .: "contents"
        pure $ BeforeSkillTest contents
      "ChangeSkillTestType" -> do
        contents <- o .: "contents"
        pure $ uncurry ChangeSkillTestType contents
      "BeginGame" -> pure BeginGame
      "Begin" -> do
        contents <- o .: "contents"
        pure $ Begin contents
      "Again" -> do
        contents <- o .: "contents"
        pure $ Again contents
      "PhaseStep" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.PhaseStep contents
      "BeginRound" -> pure BeginRound
      "ReplaceSkillTestSkill" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplaceSkillTestSkill contents
      "BeginSkillTestWithPreMessages" -> do
        contents <- o .: "contents"
        pure $ uncurry3 BeginSkillTestWithPreMessages contents
      "BeginSkillTestWithPreMessages'" -> do
        contents <- o .: "contents"
        pure $ uncurry BeginSkillTestWithPreMessages' contents
      "BeginSkillTestAfterFast" -> pure BeginSkillTestAfterFast
      "SetSkillTestTarget" -> do
        contents <- o .: "contents"
        pure $ SetSkillTestTarget contents
      "SetSkillTestResolveFailureInvestigator" -> do
        contents <- o .: "contents"
        pure $ SetSkillTestResolveFailureInvestigator contents
      "BeginTrade" -> do
        contents <- o .: "contents"
        pure $ uncurry4 BeginTrade contents
      "BeginTurn" -> do
        contents <- o .: "contents"
        pure $ BeginTurn contents
      "Blanked" -> do
        contents <- o .: "contents"
        pure $ Blanked contents
      "HandleOption" -> do
        contents <- o .: "contents"
        pure $ HandleOption contents
      "CampaignStep" -> do
        contents <- o .: "contents"
        pure $ CampaignStep contents
      "CancelEachNext" -> do
        contents <- o .: "contents"
        pure $ uncurry CancelEachNext contents
      "CancelSkillEffects" -> pure CancelSkillEffects
      "CancelHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry CancelHorror contents
      "CancelDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry CancelDamage contents
      "CancelAssetDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CancelAssetDamage contents
      "CheckAttackOfOpportunity" -> do
        contents <- o .: "contents"
        pure $ uncurry CheckAttackOfOpportunity contents
      "CheckDefeated" -> do
        contents <- o .: "contents"
        pure $ uncurry CheckDefeated contents
      "AssignDamage" -> do
        contents <- o .: "contents"
        pure $ AssignDamage contents
      "CancelAssignedDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CancelAssignedDamage contents
      "AssignedDamage" -> do
        contents <- o .: "contents"
        pure $ AssignedDamage contents
      "AssignedHealing" -> do
        contents <- o .: "contents"
        pure $ AssignedHealing contents
      "CheckHandSize" -> do
        contents <- o .: "contents"
        pure $ CheckHandSize contents
      "CheckWindow" -> do
        contents <- o .: "contents"
        pure $ uncurry CheckWindow contents
      "ChooseOneRewardByEachPlayer" -> do
        contents <- o .: "contents"
        pure $ uncurry ChooseOneRewardByEachPlayer contents
      "RunWindow" -> do
        contents <- o .: "contents"
        pure $ uncurry RunWindow contents
      "ChooseAndDiscardAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ChooseAndDiscardAsset contents
      "DiscardFromHand" -> do
        contents <- o .: "contents"
        pure $ DiscardFromHand contents
      "DoneDiscarding" -> do
        contents <- o .: "contents"
        pure $ DoneDiscarding contents
      "DiscardCard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 DiscardCard contents
      "ChooseEndTurn" -> do
        contents <- o .: "contents"
        pure $ ChooseEndTurn contents
      "ChooseEvadeEnemy" -> do
        contents <- o .: "contents"
        pure $ ChooseEvadeEnemy contents
      "ChooseFightEnemy" -> do
        contents <- o .: "contents"
        pure $ ChooseFightEnemy contents
      "ChooseEngageEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry5 ChooseEngageEnemy contents
      "ChooseLeadInvestigator" -> pure ChooseLeadInvestigator
      "PreScenarioSetup" -> pure PreScenarioSetup
      "StandaloneSetup" -> pure StandaloneSetup
      "ChoosePlayer" -> do
        contents <- o .: "contents"
        pure $ uncurry ChoosePlayer contents
      "ChoosePlayerOrder" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ChoosePlayerOrder contents
      "ChooseRandomLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry ChooseRandomLocation contents
      "ChosenRandomLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.ChosenRandomLocation contents
      "ChooseChaosTokenGroups" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ChooseChaosTokenGroups contents
      "CommitCard" -> do
        contents <- o .: "contents"
        pure $ uncurry CommitCard contents
      "CommitToSkillTest" -> do
        contents <- o .: "contents"
        pure $ uncurry CommitToSkillTest contents
      "Continue" -> do
        contents <- o .: "contents"
        pure $ Continue contents
      "CreateEffect" -> do
        contents <- o .: "contents"
        pure $ uncurry4 CreateEffect contents
      "ObtainCard" -> do
        contents <- o .: "contents"
        pure $ ObtainCard contents
      "CreateEnemy" -> do
        contents <- o .: "contents"
        pure $ CreateEnemy contents
      "CreateSkill" -> do
        contents <- o .: "contents"
        pure $ uncurry4 CreateSkill contents
      "CreatedEnemyAt" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CreatedEnemyAt contents
      "PayForAbility" -> do
        contents <- o .: "contents"
        pure $ uncurry PayForAbility contents
      "CreatedCost" -> do
        contents <- o .: "contents"
        pure $ CreatedCost contents
      "CancelCost" -> do
        contents <- o .: "contents"
        pure $ CancelCost contents
      "SetCost" -> do
        contents <- o .: "contents"
        pure $ uncurry SetCost contents
      "PayAdditionalCost" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PayAdditionalCost contents
      "CheckAdditionalCosts" -> do
        contents <- o .: "contents"
        pure $ CheckAdditionalCosts contents
      "PayCosts" -> do
        contents <- o .: "contents"
        pure $ PayCosts contents
      "PayCost" -> do
        contents <- o .: "contents"
        pure $ uncurry4 PayCost contents
      "PayCostFinished" -> do
        contents <- o .: "contents"
        pure $ PayCostFinished contents
      "PaidCost" -> do
        contents <- o .: "contents"
        pure $ uncurry4 Arkham.Message.PaidCost contents
      "PaidAllCosts" -> pure PaidAllCosts
      "CreateWindowModifierEffect" -> do
        contents <- o .: "contents"
        pure $ uncurry4 CreateWindowModifierEffect contents
      "CreateChaosTokenEffect" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CreateChaosTokenEffect contents
      "CreateAssetAt" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CreateAssetAt contents
      "CreateEventAt" -> do
        contents <- o .: "contents"
        pure $ uncurry3 CreateEventAt contents
      "PlaceAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceAsset contents
      "PlaceEvent" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlaceEvent contents
      "PlaceTreachery" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceTreachery contents
      "PlaceSkill" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceSkill contents
      "PlaceKey" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceKey contents
      "CreateStoryAssetAtLocationMatching" -> do
        contents <- o .: "contents"
        pure $ uncurry CreateStoryAssetAtLocationMatching contents
      "CreateWeaknessInThreatArea" -> do
        contents <- o .: "contents"
        pure $ uncurry CreateWeaknessInThreatArea contents
      "CreatedEffect" -> do
        contents <- o .: "contents"
        pure $ uncurry4 CreatedEffect contents
      "CrossOutRecord" -> do
        contents <- o .: "contents"
        pure $ CrossOutRecord contents
      "SetCampaignLog" -> do
        contents <- o .: "contents"
        pure $ SetCampaignLog contents
      "DeckHasNoCards" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.DeckHasNoCards contents
      "DisableEffect" -> do
        contents <- o .: "contents"
        pure $ DisableEffect contents
      "Discard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 Discard contents
      "DiscardHand" -> do
        contents <- o .: "contents"
        pure $ uncurry DiscardHand contents
      "RevealUntilFirst" -> do
        contents <- o .: "contents"
        pure $ uncurry4 RevealUntilFirst contents
      "RevealedCards" -> do
        contents <- o .: "contents"
        pure $ uncurry5 RevealedCards contents
      "DiscardUntilFirst" -> do
        contents <- o .: "contents"
        pure $ uncurry4 DiscardUntilFirst contents
      "DiscardUntilN" -> do
        contents <- o .: "contents"
        pure $ uncurry6 DiscardUntilN contents
      "DiscardTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry4 DiscardTopOfDeck contents
      "DiscardTopOfEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry4 DiscardTopOfEncounterDeck contents
      "DiscardTopOfEncounterDeckWithDiscardedCards" -> do
        contents <- o .: "contents"
        pure $ uncurry5 DiscardTopOfEncounterDeckWithDiscardedCards contents
      "Discarded" -> do
        contents <- o .: "contents"
        pure $ uncurry3 Arkham.Message.Discarded contents
      "DiscardedTopOfEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry4 DiscardedTopOfEncounterDeck contents
      "DiscardedTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry4 DiscardedTopOfDeck contents
      "DiscoverClues" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.DiscoverClues contents
      "DisengageEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry DisengageEnemy contents
      "DisengageEnemyFromAll" -> do
        contents <- o .: "contents"
        pure $ DisengageEnemyFromAll contents
      "DrawAnotherChaosToken" -> do
        contents <- o .: "contents"
        pure $ DrawAnotherChaosToken contents
      "DrawChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry DrawChaosToken contents
      "DrewPlayerEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry DrewPlayerEnemy contents
      "DrewTreachery" -> do
        contents <- o .: "contents"
        pure $ uncurry3 DrewTreachery contents
      "ResolveTreachery" -> do
        contents <- o .: "contents"
        pure $ uncurry ResolveTreachery contents
      "DrivenInsane" -> do
        contents <- o .: "contents"
        pure $ DrivenInsane contents
      "EmptyDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry EmptyDeck contents
      "EndPhase" -> pure EndPhase
      "EndCheckWindow" -> pure EndCheckWindow
      "EndEnemy" -> pure EndEnemy
      "EndInvestigation" -> pure EndInvestigation
      "EndMythos" -> pure EndMythos
      "EndOfGame" -> do
        contents <- o .: "contents"
        pure $ EndOfGame contents
      "Exile" -> do
        contents <- o .: "contents"
        pure $ Exile contents
      "Exiled" -> do
        contents <- o .: "contents"
        pure $ uncurry Exiled contents
      "ScenarioResolution" -> do
        contents <- o .: "contents"
        pure $ ScenarioResolution contents
      "ScenarioResolutionStep" -> do
        contents <- o .: "contents"
        pure $ uncurry ScenarioResolutionStep contents
      "EndOfScenario" -> do
        contents <- o .: "contents"
        pure $ EndOfScenario contents
      "EndRound" -> pure EndRound
      "BeginRoundWindow" -> pure BeginRoundWindow
      "EndRoundWindow" -> pure EndRoundWindow
      "EndSearch" -> do
        contents <- o .: "contents"
        pure $ uncurry4 EndSearch contents
      "SearchEnded" -> do
        contents <- o .: "contents"
        pure $ SearchEnded contents
      "CancelSearch" -> do
        contents <- o .: "contents"
        pure $ CancelSearch contents
      "EndTurn" -> do
        contents <- o .: "contents"
        pure $ EndTurn contents
      "EndUpkeep" -> pure EndUpkeep
      "EnemiesAttack" -> pure EnemiesAttack
      "EnemyWillAttack" -> do
        contents <- o .: "contents"
        pure $ EnemyWillAttack contents
      "EnemyAttack" -> do
        contents <- o .: "contents"
        pure $ EnemyAttack contents
      "InitiateEnemyAttack" -> do
        contents <- o .: "contents"
        pure $ InitiateEnemyAttack contents
      "PerformEnemyAttack" -> do
        contents <- o .: "contents"
        pure $ PerformEnemyAttack contents
      "AfterEnemyAttack" -> do
        contents <- o .: "contents"
        pure $ uncurry AfterEnemyAttack contents
      "EnemyAttackFromDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 EnemyAttackFromDiscard contents
      "EnemyAttackIfEngaged" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyAttackIfEngaged contents
      "EnemyAttacks" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.EnemyAttacks contents
      "ChangeEnemyAttackTarget" -> do
        contents <- o .: "contents"
        pure $ uncurry ChangeEnemyAttackTarget contents
      "ChangeEnemyAttackDetails" -> do
        contents <- o .: "contents"
        pure $ uncurry ChangeEnemyAttackDetails contents
      "CheckEnemyEngagement" -> do
        contents <- o .: "contents"
        pure $ CheckEnemyEngagement contents
      "EnemyCheckEngagement" -> do
        contents <- o .: "contents"
        pure $ EnemyCheckEngagement contents
      "EnemyDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyDamage contents
      "EnemyDamaged" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyDamaged contents
      "DefeatEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry3 DefeatEnemy contents
      "EnemyDefeated" -> do
        contents <- o .: "contents"
        pure $ uncurry4 EnemyDefeated contents
      "EnemyEngageInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyEngageInvestigator contents
      "EnemyEvaded" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.EnemyEvaded contents
      "EnemyMove" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyMove contents
      "EnemyEntered" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemyEntered contents
      "SetBearer" -> do
        contents <- o .: "contents"
        pure $ uncurry SetBearer contents
      "EnemySpawn" -> do
        contents <- o .: "contents"
        pure $ uncurry3 EnemySpawn contents
      "EnemySpawnAtLocationMatching" -> do
        contents <- o .: "contents"
        pure $ uncurry3 EnemySpawnAtLocationMatching contents
      "EnemySpawnEngagedWithPrey" -> do
        contents <- o .: "contents"
        pure $ EnemySpawnEngagedWithPrey contents
      "EnemySpawnEngagedWith" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemySpawnEngagedWith contents
      "EnemySpawnFromVoid" -> do
        contents <- o .: "contents"
        pure $ uncurry3 EnemySpawnFromVoid contents
      "EnemySpawnedAt" -> do
        contents <- o .: "contents"
        pure $ uncurry EnemySpawnedAt contents
      "EngageEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry4 EngageEnemy contents
      "Exhaust" -> do
        contents <- o .: "contents"
        pure $ Exhaust contents
      "ExhaustThen" -> do
        contents <- o .: "contents"
        pure $ uncurry ExhaustThen contents
      "FailSkillTest" -> pure FailSkillTest
      "FailedAttackEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry FailedAttackEnemy contents
      "FailedSkillTest" -> do
        contents <- o .: "contents"
        pure $ uncurry6 FailedSkillTest contents
      "FindAndDrawEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 FindAndDrawEncounterCard contents
      "FindEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry4 FindEncounterCard contents
      "FinishedWithMulligan" -> do
        contents <- o .: "contents"
        pure $ FinishedWithMulligan contents
      "FocusCards" -> do
        contents <- o .: "contents"
        pure $ FocusCards contents
      "FocusChaosTokens" -> do
        contents <- o .: "contents"
        pure $ FocusChaosTokens contents
      "Force" -> do
        contents <- o .: "contents"
        pure $ Force contents
      "FoundAndDrewEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 FoundAndDrewEncounterCard contents
      "FoundEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 FoundEncounterCard contents
      "FoundEncounterCardFrom" -> do
        contents <- o .: "contents"
        pure $ uncurry4 FoundEncounterCardFrom contents
      "FoundEnemyInVoid" -> do
        contents <- o .: "contents"
        pure $ uncurry3 FoundEnemyInVoid contents
      "GainActions" -> do
        contents <- o .: "contents"
        pure $ uncurry3 GainActions contents
      "LoseAdditionalAction" -> do
        contents <- o .: "contents"
        pure $ uncurry LoseAdditionalAction contents
      "UseEffectAction" -> do
        contents <- o .: "contents"
        pure $ uncurry3 UseEffectAction contents
      "GainClues" -> do
        contents <- o .: "contents"
        pure $ uncurry3 GainClues contents
      "GainXP" -> do
        contents <- o .: "contents"
        pure $ uncurry3 GainXP contents
      "SpendXP" -> do
        contents <- o .: "contents"
        pure $ uncurry SpendXP contents
      "GameOver" -> pure GameOver
      "HandlePointOfFailure" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HandlePointOfFailure contents
      "ApplyHealing" -> do
        contents <- o .: "contents"
        pure $ ApplyHealing contents
      "HealAllDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry HealAllDamage contents
      "HealAllHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry HealAllHorror contents
      "HealAllDamageAndHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry HealAllDamageAndHorror contents
      "HealDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealDamage contents
      "HealHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealHorror contents
      "HealDamageDelayed" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealDamageDelayed contents
      "HealHorrorDelayed" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealHorrorDelayed contents
      "ReassignHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ReassignHorror contents
      "HealHorrorWithAdditional" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealHorrorWithAdditional contents
      "AdditionalHealHorror" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AdditionalHealHorror contents
      "HealDamageDirectly" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealDamageDirectly contents
      "HealHorrorDirectly" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealHorrorDirectly contents
      "HuntersMove" -> pure HuntersMove
      "HunterMove" -> do
        contents <- o .: "contents"
        pure $ HunterMove contents
      "PatrolMove" -> do
        contents <- o .: "contents"
        pure $ uncurry PatrolMove contents
      "InDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry InDiscard contents
      "InSearch" -> do
        contents <- o .: "contents"
        pure $ InSearch contents
      "InHand" -> do
        contents <- o .: "contents"
        pure $ uncurry InHand contents
      "InOutOfPlay" -> do
        contents <- o .: "contents"
        pure $ InOutOfPlay contents
      "InitDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry InitDeck contents
      "LoadDecklist" -> do
        contents <- o .: "contents"
        pure $ uncurry LoadDecklist contents
      "UpgradeDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry UpgradeDeck contents
      "FinishedUpgradingDecks" -> pure FinishedUpgradingDecks
      "Flip" -> do
        contents <- o .: "contents"
        pure $ uncurry3 Flip contents
      "Flipped" -> do
        contents <- o .: "contents"
        pure $ uncurry Flipped contents
      "InitiatePlayCardAsChoose" -> do
        contents <- o .: "contents"
        pure $ uncurry8 InitiatePlayCardAsChoose contents
      "InitiatePlayCardAs" -> do
        contents <- o .: "contents"
        pure $ uncurry8 InitiatePlayCardAs contents
      "InitiatePlayCard" -> do
        contents <- o .: "contents"
        pure $ uncurry6 InitiatePlayCard contents
      "CheckAdditionalActionCosts" -> do
        contents <- o .: "contents"
        pure $ uncurry4 CheckAdditionalActionCosts contents
      "CheckAllAdditionalCommitCosts" -> pure CheckAllAdditionalCommitCosts
      "CheckAdditionalCommitCosts" -> do
        contents <- o .: "contents"
        pure $ uncurry CheckAdditionalCommitCosts contents
      "Investigate" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.Investigate contents
      "InvestigatorAssignDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry5 InvestigatorAssignDamage contents
      "InvestigatorCommittedCard" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorCommittedCard contents
      "InvestigatorCommittedSkill" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorCommittedSkill contents
      "InvestigatorDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry4 InvestigatorDamage contents
      "InvestigatorDamageEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry3 InvestigatorDamageEnemy contents
      "InvestigatorDamageInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDamageInvestigator contents
      "InvestigatorDefeated" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDefeated contents
      "InvestigatorIsDefeated" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorIsDefeated contents
      "InvestigatorDirectDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry4 InvestigatorDirectDamage contents
      "InvestigatorDiscardAllClues" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDiscardAllClues contents
      "InvestigatorDoAssignDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry8 InvestigatorDoAssignDamage contents
      "InvestigatorDrawEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDrawEnemy contents
      "InvestigatorDrewEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDrewEncounterCard contents
      "InvestigatorDrewPlayerCard" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorDrewPlayerCard contents
      "InvestigatorEliminated" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.InvestigatorEliminated contents
      "InvestigatorKilled" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorKilled contents
      "InvestigatorMulligan" -> do
        contents <- o .: "contents"
        pure $ InvestigatorMulligan contents
      "InvestigatorsMulligan" -> pure InvestigatorsMulligan
      "InvestigatorPlaceAllCluesOnLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorPlaceAllCluesOnLocation contents
      "InvestigatorPlaceCluesOnLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry3 InvestigatorPlaceCluesOnLocation contents
      "InvestigatorPlayAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorPlayAsset contents
      "InvestigatorClearUnusedAssetSlots" -> do
        contents <- o .: "contents"
        pure $ InvestigatorClearUnusedAssetSlots contents
      "InvestigatorAdjustAssetSlots" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorAdjustAssetSlots contents
      "InvestigatorAdjustSlot" -> do
        contents <- o .: "contents"
        pure $ uncurry4 InvestigatorAdjustSlot contents
      "InvestigatorPlayedAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorPlayedAsset contents
      "InvestigatorPlayEvent" -> do
        contents <- o .: "contents"
        pure $ uncurry5 InvestigatorPlayEvent contents
      "FinishedEvent" -> do
        contents <- o .: "contents"
        pure $ FinishedEvent contents
      "InvestigatorResigned" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.InvestigatorResigned contents
      "InvestigatorSpendClues" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorSpendClues contents
      "InvestigatorWhenDefeated" -> do
        contents <- o .: "contents"
        pure $ uncurry InvestigatorWhenDefeated contents
      "InvestigatorWhenEliminated" -> do
        contents <- o .: "contents"
        pure $ uncurry3 InvestigatorWhenEliminated contents
      "LoadDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry LoadDeck contents
      "LookAtRevealed" -> do
        contents <- o .: "contents"
        pure $ uncurry3 LookAtRevealed contents
      "LookAtTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 LookAtTopOfDeck contents
      "LoseActions" -> do
        contents <- o .: "contents"
        pure $ uncurry3 LoseActions contents
      "LoseResources" -> do
        contents <- o .: "contents"
        pure $ uncurry3 LoseResources contents
      "LoseAllResources" -> do
        contents <- o .: "contents"
        pure $ LoseAllResources contents
      "SpendActions" -> do
        contents <- o .: "contents"
        pure $ uncurry4 SpendActions contents
      "Move" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.Move contents
      "MoveAction" -> do
        contents <- o .: "contents"
        pure $ uncurry4 Arkham.Message.MoveAction contents
      "MoveAllTo" -> do
        contents <- o .: "contents"
        pure $ uncurry MoveAllTo contents
      "MoveFrom" -> do
        contents <- o .: "contents"
        pure $ uncurry3 MoveFrom contents
      "MoveTo" -> do
        contents <- o .: "contents"
        pure $ MoveTo contents
      "MoveToward" -> do
        contents <- o .: "contents"
        pure $ uncurry MoveToward contents
      "MoveUntil" -> do
        contents <- o .: "contents"
        pure $ uncurry MoveUntil contents
      "WhenCanMove" -> do
        contents <- o .: "contents"
        pure $ uncurry WhenCanMove contents
      "MoveAllCluesTo" -> do
        contents <- o .: "contents"
        pure $ uncurry MoveAllCluesTo contents
      "MoveTopOfDeckToBottom" -> do
        contents <- o .: "contents"
        pure $ uncurry3 MoveTopOfDeckToBottom contents
      "NextCampaignStep" -> do
        contents <- o .: "contents"
        pure $ NextCampaignStep contents
      "NextChaosBagStep" -> do
        contents <- o .: "contents"
        pure $ uncurry3 NextChaosBagStep contents
      "Noop" -> pure Noop
      "PassSkillTest" -> pure PassSkillTest
      "PassSkillTestBy" -> do
        contents <- o .: "contents"
        pure $ PassSkillTestBy contents
      "PassedSkillTest" -> do
        contents <- o .: "contents"
        pure $ uncurry6 PassedSkillTest contents
      "PaidAbilityCost" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PaidAbilityCost contents
      "PayCardCost" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PayCardCost contents
      "PaidForCardCost" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PaidForCardCost contents
      "PayForCardAbility" -> do
        contents <- o .: "contents"
        pure $ uncurry5 PayForCardAbility contents
      "PlaceCluesUpToClueValue" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlaceCluesUpToClueValue contents
      "FlipClues" -> do
        contents <- o .: "contents"
        pure $ uncurry FlipClues contents
      "FlipDoom" -> do
        contents <- o .: "contents"
        pure $ uncurry FlipDoom contents
      "PlaceAdditionalDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry4 PlaceAdditionalDamage contents
      "PlaceDoomOnAgenda" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceDoomOnAgenda contents
      "PlaceEnemyInVoid" -> do
        contents <- o .: "contents"
        pure $ PlaceEnemyInVoid contents
      "PlaceEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceEnemy contents
      "PlaceLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceLocation contents
      "PlaceLocationMatching" -> do
        contents <- o .: "contents"
        pure $ PlaceLocationMatching contents
      "PlaceTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry4 PlaceTokens contents
      "RemoveTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry4 RemoveTokens contents
      "MoveTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry5 MoveTokens contents
      "PlaceUnderneath" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.PlaceUnderneath contents
      "PlacedUnderneath" -> do
        contents <- o .: "contents"
        pure $ uncurry PlacedUnderneath contents
      "PlaceNextTo" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceNextTo contents
      "PlacedLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlacedLocation contents
      "PlacedLocationDirection" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlacedLocationDirection contents
      "LocationMoved" -> do
        contents <- o .: "contents"
        pure $ LocationMoved contents
      "PlayCard" -> do
        contents <- o .: "contents"
        pure $ uncurry6 Arkham.Message.PlayCard contents
      "CardEnteredPlay" -> do
        contents <- o .: "contents"
        pure $ uncurry CardEnteredPlay contents
      "ResolvedCard" -> do
        contents <- o .: "contents"
        pure $ uncurry ResolvedCard contents
      "ResolvedPlayCard" -> do
        contents <- o .: "contents"
        pure $ uncurry ResolvedPlayCard contents
      "PlayerWindow" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlayerWindow contents
      "PutCampaignCardIntoPlay" -> do
        contents <- o .: "contents"
        pure $ uncurry PutCampaignCardIntoPlay contents
      "PutCardIntoPlay" -> do
        contents <- o .: "contents"
        pure $ uncurry5 PutCardIntoPlay contents
      "PutCardOnTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PutCardOnTopOfDeck contents
      "PutOnTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PutOnTopOfDeck contents
      "PutCardOnBottomOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PutCardOnBottomOfDeck contents
      "PutOnBottomOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PutOnBottomOfDeck contents
      "Ready" -> do
        contents <- o .: "contents"
        pure $ Ready contents
      "ReadyAlternative" -> do
        contents <- o .: "contents"
        pure $ uncurry ReadyAlternative contents
      "ReadyExhausted" -> pure ReadyExhausted
      "Record" -> do
        contents <- o .: "contents"
        pure $ Record contents
      "RecordForInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry RecordForInvestigator contents
      "RecordCount" -> do
        contents <- o .: "contents"
        pure $ uncurry RecordCount contents
      "IncrementRecordCount" -> do
        contents <- o .: "contents"
        pure $ uncurry IncrementRecordCount contents
      "RecordSetInsert" -> do
        contents <- o .: "contents"
        pure $ uncurry RecordSetInsert contents
      "CrossOutRecordSetEntries" -> do
        contents <- o .: "contents"
        pure $ uncurry CrossOutRecordSetEntries contents
      "RefillSlots" -> do
        contents <- o .: "contents"
        pure $ RefillSlots contents
      "Remember" -> do
        contents <- o .: "contents"
        pure $ Remember contents
      "ScenarioCountIncrementBy" -> do
        contents <- o .: "contents"
        pure $ uncurry ScenarioCountIncrementBy contents
      "ScenarioCountDecrementBy" -> do
        contents <- o .: "contents"
        pure $ uncurry ScenarioCountDecrementBy contents
      "RemoveAllCopiesOfCardFromGame" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveAllCopiesOfCardFromGame contents
      "RemoveAllCopiesOfEncounterCardFromGame" -> do
        contents <- o .: "contents"
        pure $ RemoveAllCopiesOfEncounterCardFromGame contents
      "RemovePlayerCardFromGame" -> do
        contents <- o .: "contents"
        pure $ uncurry RemovePlayerCardFromGame contents
      "RemoveAllTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveAllTokens contents
      "RemoveAllAttachments" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveAllAttachments contents
      "RemoveAllClues" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveAllClues contents
      "RemoveAllDoomFromPlay" -> do
        contents <- o .: "contents"
        pure $ RemoveAllDoomFromPlay contents
      "RemoveAllDoom" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveAllDoom contents
      "RemoveCampaignCard" -> do
        contents <- o .: "contents"
        pure $ RemoveCampaignCard contents
      "RemoveCampaignCardFromDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCampaignCardFromDeck contents
      "RemoveCardFromHand" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCardFromHand contents
      "RemoveCardFromSearch" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCardFromSearch contents
      "RemoveDiscardFromGame" -> do
        contents <- o .: "contents"
        pure $ RemoveDiscardFromGame contents
      "RemoveAsset" -> do
        contents <- o .: "contents"
        pure $ RemoveAsset contents
      "RemoveEnemy" -> do
        contents <- o .: "contents"
        pure $ RemoveEnemy contents
      "RemoveEvent" -> do
        contents <- o .: "contents"
        pure $ RemoveEvent contents
      "RemoveSkill" -> do
        contents <- o .: "contents"
        pure $ RemoveSkill contents
      "RemoveTreachery" -> do
        contents <- o .: "contents"
        pure $ RemoveTreachery contents
      "RemoveFromDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveFromDiscard contents
      "RemoveFromEncounterDiscard" -> do
        contents <- o .: "contents"
        pure $ RemoveFromEncounterDiscard contents
      "RemoveFromEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ RemoveFromEncounterDeck contents
      "RemoveFromGame" -> do
        contents <- o .: "contents"
        pure $ RemoveFromGame contents
      "QuietlyRemoveFromGame" -> do
        contents <- o .: "contents"
        pure $ QuietlyRemoveFromGame contents
      "RemoveCompletedActFromGame" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveCompletedActFromGame contents
      "RemovedFromGame" -> do
        contents <- o .: "contents"
        pure $ RemovedFromGame contents
      "RemoveLocation" -> do
        contents <- o .: "contents"
        pure $ RemoveLocation contents
      "RemovedLocation" -> do
        contents <- o .: "contents"
        pure $ RemovedLocation contents
      "SetCardAside" -> do
        contents <- o .: "contents"
        pure $ SetCardAside contents
      "SetOutOfPlay" -> do
        contents <- o .: "contents"
        pure $ uncurry SetOutOfPlay contents
      "PlaceInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceInvestigator contents
      "PlaceInBonded" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceInBonded contents
      "DoSetOutOfPlay" -> do
        contents <- o .: "contents"
        pure $ uncurry DoSetOutOfPlay contents
      "RemoveFromPlay" -> do
        contents <- o .: "contents"
        pure $ RemoveFromPlay contents
      "RemovedFromPlay" -> do
        contents <- o .: "contents"
        pure $ RemovedFromPlay contents
      "ReplaceCurrentDraw" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ReplaceCurrentDraw contents
      "SetChaosBagChoice" -> do
        contents <- o .: "contents"
        pure $ uncurry3 SetChaosBagChoice contents
      "RequestSetAsideCard" -> do
        contents <- o .: "contents"
        pure $ uncurry RequestSetAsideCard contents
      "RequestChaosTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry4 RequestChaosTokens contents
      "RequestedEncounterCard" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RequestedEncounterCard contents
      "RequestedEncounterCards" -> do
        contents <- o .: "contents"
        pure $ uncurry RequestedEncounterCards contents
      "RequestedPlayerCard" -> do
        contents <- o .: "contents"
        pure $ uncurry4 RequestedPlayerCard contents
      "RequestedSetAsideCard" -> do
        contents <- o .: "contents"
        pure $ uncurry RequestedSetAsideCard contents
      "RequestedChaosTokens" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RequestedChaosTokens contents
      "RerunSkillTest" -> pure RerunSkillTest
      "ResetInvestigators" -> pure ResetInvestigators
      "ResetGame" -> pure ResetGame
      "ResetChaosTokens" -> do
        contents <- o .: "contents"
        pure $ ResetChaosTokens contents
      "ReturnChaosTokensToPool" -> do
        contents <- o .: "contents"
        pure $ ReturnChaosTokensToPool contents
      "Resign" -> do
        contents <- o .: "contents"
        pure $ Arkham.Message.Resign contents
      "ResignWith" -> do
        contents <- o .: "contents"
        pure $ ResignWith contents
      "ResolveAmounts" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ResolveAmounts contents
      "ResolveEvent" -> do
        contents <- o .: "contents"
        pure $ uncurry4 ResolveEvent contents
      "ResolveEventChoice" -> do
        contents <- o .: "contents"
        pure $ uncurry5 ResolveEventChoice contents
      "ResolveSkill" -> do
        contents <- o .: "contents"
        pure $ ResolveSkill contents
      "ResolveChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ResolveChaosToken contents
      "TargetResolveChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry4 TargetResolveChaosToken contents
      "ReturnSkillTestRevealedChaosTokens" -> pure ReturnSkillTestRevealedChaosTokens
      "ReturnChaosTokens" -> do
        contents <- o .: "contents"
        pure $ ReturnChaosTokens contents
      "RevealCard" -> do
        contents <- o .: "contents"
        pure $ RevealCard contents
      "RevealLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry Arkham.Message.RevealLocation contents
      "UnrevealLocation" -> do
        contents <- o .: "contents"
        pure $ UnrevealLocation contents
      "RevealSkillTestChaosTokens" -> do
        contents <- o .: "contents"
        pure $ RevealSkillTestChaosTokens contents
      "RevealChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RevealChaosToken contents
      "Revelation" -> do
        contents <- o .: "contents"
        pure $ uncurry Revelation contents
      "RevelationChoice" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RevelationChoice contents
      "Run" -> do
        contents <- o .: "contents"
        pure $ Run contents
      "RunBag" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RunBag contents
      "RunDrawFromBag" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RunDrawFromBag contents
      "RunSkillTest" -> do
        contents <- o .: "contents"
        pure $ RunSkillTest contents
      "RecalculateSkillTestResults" -> pure RecalculateSkillTestResults
      "RemoveFromBearersDeckOrDiscard" -> do
        contents <- o .: "contents"
        pure $ RemoveFromBearersDeckOrDiscard contents
      "SearchCollectionForRandom" -> do
        contents <- o .: "contents"
        pure $ uncurry3 SearchCollectionForRandom contents
      "FinishedSearch" -> pure FinishedSearch
      "Search" -> do
        contents <- o .: "contents"
        pure $ uncurry7 Search contents
      "ResolveSearch" -> do
        contents <- o .: "contents"
        pure $ ResolveSearch contents
      "SearchFound" -> do
        contents <- o .: "contents"
        pure $ uncurry4 SearchFound contents
      "FoundCards" -> do
        contents <- o .: "contents"
        pure $ FoundCards contents
      "SearchNoneFound" -> do
        contents <- o .: "contents"
        pure $ uncurry SearchNoneFound contents
      "UpdateSearchReturnStrategy" -> do
        contents <- o .: "contents"
        pure $ uncurry3 UpdateSearchReturnStrategy contents
      "SetActions" -> do
        contents <- o .: "contents"
        pure $ uncurry3 SetActions contents
      "SetEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ SetEncounterDeck contents
      "SetLayout" -> do
        contents <- o .: "contents"
        pure $ SetLayout contents
      "SetLocationLabel" -> do
        contents <- o .: "contents"
        pure $ uncurry SetLocationLabel contents
      "SetRole" -> do
        contents <- o .: "contents"
        pure $ uncurry SetRole contents
      "ForceChaosTokenDraw" -> do
        contents <- o .: "contents"
        pure $ ForceChaosTokenDraw contents
      "SetActiveInvestigator" -> do
        contents <- o .: "contents"
        pure $ SetActiveInvestigator contents
      "SetChaosTokens" -> do
        contents <- o .: "contents"
        pure $ SetChaosTokens contents
      "SetChaosTokensForScenario" -> pure SetChaosTokensForScenario
      "Setup" -> pure Setup
      "EndSetup" -> pure EndSetup
      "SetupInvestigators" -> pure SetupInvestigators
      "SetupInvestigator" -> do
        contents <- o .: "contents"
        pure $ SetupInvestigator contents
      "SetupStep" -> do
        contents <- o .: "contents"
        pure $ uncurry SetupStep contents
      "ShuffleAllFocusedIntoDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry ShuffleAllFocusedIntoDeck contents
      "PutAllFocusedIntoDiscard" -> do
        contents <- o .: "contents"
        pure $ uncurry PutAllFocusedIntoDiscard contents
      "ShuffleAllInEncounterDiscardBackIn" -> do
        contents <- o .: "contents"
        pure $ ShuffleAllInEncounterDiscardBackIn contents
      "ShuffleBackIntoEncounterDeck" -> do
        contents <- o .: "contents"
        pure $ ShuffleBackIntoEncounterDeck contents
      "ShuffleCardsIntoDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry ShuffleCardsIntoDeck contents
      "ShuffleDiscardBackIn" -> do
        contents <- o .: "contents"
        pure $ ShuffleDiscardBackIn contents
      "ShuffleEncounterDiscardBackIn" -> pure ShuffleEncounterDiscardBackIn
      "ShuffleDeck" -> do
        contents <- o .: "contents"
        pure $ ShuffleDeck contents
      "ShuffleIntoDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry ShuffleIntoDeck contents
      "ShuffleCardsIntoTopOfDeck" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ShuffleCardsIntoTopOfDeck contents
      "SkillTestApplyResults" -> pure SkillTestApplyResults
      "SkillTestApplyResultsAfter" -> pure SkillTestApplyResultsAfter
      "SkillTestAsk" -> do
        contents <- o .: "contents"
        pure $ SkillTestAsk contents
      "SkillTestCommitCard" -> do
        contents <- o .: "contents"
        pure $ uncurry SkillTestCommitCard contents
      "AfterSkillTestEnds" -> do
        contents <- o .: "contents"
        pure $ uncurry3 AfterSkillTestEnds contents
      "EndSkillTestWindow" -> pure EndSkillTestWindow
      "SkillTestResults" -> do
        contents <- o .: "contents"
        pure $ SkillTestResults contents
      "SkillTestUncommitCard" -> do
        contents <- o .: "contents"
        pure $ uncurry SkillTestUncommitCard contents
      "SpawnEnemyAt" -> do
        contents <- o .: "contents"
        pure $ uncurry SpawnEnemyAt contents
      "SpawnEnemyAtEngagedWith" -> do
        contents <- o .: "contents"
        pure $ uncurry3 SpawnEnemyAtEngagedWith contents
      "SpendClues" -> do
        contents <- o .: "contents"
        pure $ uncurry SpendClues contents
      "SpendResources" -> do
        contents <- o .: "contents"
        pure $ uncurry SpendResources contents
      "SpendUses" -> do
        contents <- o .: "contents"
        pure $ uncurry4 SpendUses contents
      "SpentAllUses" -> do
        contents <- o .: "contents"
        pure $ SpentAllUses contents
      "StartCampaign" -> pure StartCampaign
      "StartScenario" -> do
        contents <- o .: "contents"
        pure $ StartScenario contents
      "RestartScenario" -> pure RestartScenario
      "StartSkillTest" -> do
        contents <- o .: "contents"
        pure $ StartSkillTest contents
      "Successful" -> do
        contents <- o .: "contents"
        pure $ uncurry5 Successful contents
      "Failed" -> do
        contents <- o .: "contents"
        pure $ uncurry5 Failed contents
      "SufferTrauma" -> do
        contents <- o .: "contents"
        pure $ uncurry3 SufferTrauma contents
      "CheckTrauma" -> do
        contents <- o .: "contents"
        pure $ CheckTrauma contents
      "HealTrauma" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HealTrauma contents
      "GainSurge" -> do
        contents <- o .: "contents"
        pure $ uncurry GainSurge contents
      "CancelSurge" -> do
        contents <- o .: "contents"
        pure $ CancelSurge contents
      "Surge" -> do
        contents <- o .: "contents"
        pure $ uncurry Surge contents
      "TakeActions" -> do
        contents <- o .: "contents"
        pure $ uncurry3 TakeActions contents
      "TakeControlOfAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry TakeControlOfAsset contents
      "ReplaceInvestigatorAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ReplaceInvestigatorAsset contents
      "ReplacedInvestigatorAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplacedInvestigatorAsset contents
      "TakeControlOfSetAsideAsset" -> do
        contents <- o .: "contents"
        pure $ uncurry TakeControlOfSetAsideAsset contents
      "SetAsideCards" -> do
        contents <- o .: "contents"
        pure $ SetAsideCards contents
      "TakeResources" -> do
        contents <- o .: "contents"
        pure $ uncurry4 TakeResources contents
      "DrawStartingHands" -> pure DrawStartingHands
      "TakeStartingResources" -> do
        contents <- o .: "contents"
        pure $ TakeStartingResources contents
      "TakenActions" -> do
        contents <- o .: "contents"
        pure $ uncurry TakenActions contents
      "PerformedActions" -> do
        contents <- o .: "contents"
        pure $ uncurry PerformedActions contents
      "TriggerSkillTest" -> do
        contents <- o .: "contents"
        pure $ TriggerSkillTest contents
      "UnfocusCards" -> pure UnfocusCards
      "ClearFound" -> do
        contents <- o .: "contents"
        pure $ ClearFound contents
      "UnfocusTargets" -> pure UnfocusTargets
      "UnfocusChaosTokens" -> pure UnfocusChaosTokens
      "SealChaosToken" -> do
        contents <- o .: "contents"
        pure $ SealChaosToken contents
      "SealedChaosToken" -> do
        contents <- o .: "contents"
        pure $ uncurry SealedChaosToken contents
      "SetChaosTokenAside" -> do
        contents <- o .: "contents"
        pure $ SetChaosTokenAside contents
      "UnsealChaosToken" -> do
        contents <- o .: "contents"
        pure $ UnsealChaosToken contents
      "ChaosTokenIgnored" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ChaosTokenIgnored contents
      "ChaosTokenCanceled" -> do
        contents <- o .: "contents"
        pure $ uncurry3 ChaosTokenCanceled contents
      "SetActiveCard" -> do
        contents <- o .: "contents"
        pure $ SetActiveCard contents
      "UnsetActiveCard" -> pure UnsetActiveCard
      "AddCardEntity" -> do
        contents <- o .: "contents"
        pure $ AddCardEntity contents
      "RemoveCardEntity" -> do
        contents <- o .: "contents"
        pure $ RemoveCardEntity contents
      "UseCardAbility" -> do
        contents <- o .: "contents"
        pure $ uncurry5 UseCardAbility contents
      "UseCardAbilityStep" -> do
        contents <- o .: "contents"
        pure $ uncurry6 UseCardAbilityStep contents
      "UseCardAbilityChoice" -> do
        contents <- o .: "contents"
        pure $ uncurry6 UseCardAbilityChoice contents
      "UseCardAbilityChoiceTarget" -> do
        contents <- o .: "contents"
        pure $ uncurry6 UseCardAbilityChoiceTarget contents
      "HandleTargetChoice" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HandleTargetChoice contents
      "HandleAbilityOption" -> do
        contents <- o .: "contents"
        pure $ uncurry3 HandleAbilityOption contents
      "ResetMetadata" -> do
        contents <- o .: "contents"
        pure $ ResetMetadata contents
      "DoNotCountUseTowardsAbilityLimit" -> do
        contents <- o .: "contents"
        pure $ uncurry DoNotCountUseTowardsAbilityLimit contents
      "When" -> do
        contents <- o .: "contents"
        pure $ When contents
      "Would" -> do
        contents <- o .: "contents"
        pure $ uncurry Would contents
      "CancelBatch" -> do
        contents <- o .: "contents"
        pure $ CancelBatch contents
      "IgnoreBatch" -> do
        contents <- o .: "contents"
        pure $ IgnoreBatch contents
      "WhenWillEnterLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry WhenWillEnterLocation contents
      "EnterLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry EnterLocation contents
      "Will" -> do
        contents <- o .: "contents"
        pure $ Will contents
      "WillMoveEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry WillMoveEnemy contents
      "SetOriginalCardCode" -> do
        contents <- o .: "contents"
        pure $ SetOriginalCardCode contents
      "ActionCannotBeUndone" -> pure ActionCannotBeUndone
      "UndoAction" -> pure UndoAction
      "BeginAction" -> pure BeginAction
      "FinishAction" -> pure FinishAction
      "BeginCardPayment" -> do
        contents <- o .: "contents"
        pure $ BeginCardPayment contents
      "FinishCardPayment" -> do
        contents <- o .: "contents"
        pure $ FinishCardPayment contents
      "ReplaceCard" -> do
        contents <- o .: "contents"
        pure $ uncurry ReplaceCard contents
      "UpdateHistory" -> do
        contents <- o .: "contents"
        pure $ uncurry UpdateHistory contents
      "PickSupply" -> do
        contents <- o .: "contents"
        pure $ uncurry PickSupply contents
      "UseSupply" -> do
        contents <- o .: "contents"
        pure $ uncurry UseSupply contents
      "Explore" -> do
        contents <- o .: "contents"
        pure $ uncurry3 Explore contents
      "BecomeYithian" -> do
        contents <- o .: "contents"
        pure $ BecomeYithian contents
      "SetScenarioMeta" -> do
        contents <- o .: "contents"
        pure $ SetScenarioMeta contents
      "DoStep" -> do
        contents <- o .: "contents"
        pure $ uncurry DoStep contents
      "ForInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry ForInvestigator contents
      "ForTarget" -> do
        contents <- o .: "contents"
        pure $ uncurry ForTarget contents
      "ForPlayer" -> do
        contents <- o .: "contents"
        pure $ uncurry ForPlayer contents
      "ForSkillType" -> do
        contents <- o .: "contents"
        pure $ uncurry ForSkillType contents
      "BecomePrologueInvestigator" -> do
        contents <- o .: "contents"
        pure $ uncurry BecomePrologueInvestigator contents
      "PutLocationInFrontOf" -> do
        contents <- o .: "contents"
        pure $ uncurry PutLocationInFrontOf contents
      "PutLocationInCenter" -> do
        contents <- o .: "contents"
        pure $ PutLocationInCenter contents
      "PlaceBreaches" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceBreaches contents
      "RemoveBreaches" -> do
        contents <- o .: "contents"
        pure $ uncurry RemoveBreaches contents
      "RunCosmos" -> do
        contents <- o .: "contents"
        pure $ uncurry3 RunCosmos contents
      "PlaceCosmos" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlaceCosmos contents
      "LoadTarotDeck" -> pure LoadTarotDeck
      "PerformTarotReading" -> pure PerformTarotReading
      "PerformReading" -> do
        contents <- o .: "contents"
        pure $ PerformReading contents
      "DrawAndChooseTarot" -> do
        contents <- o .: "contents"
        pure $ uncurry3 DrawAndChooseTarot contents
      "PlaceTarot" -> do
        contents <- o .: "contents"
        pure $ uncurry PlaceTarot contents
      "FocusTarotCards" -> do
        contents <- o .: "contents"
        pure $ FocusTarotCards contents
      "UnfocusTarotCards" -> pure UnfocusTarotCards
      "RotateTarot" -> do
        contents <- o .: "contents"
        pure $ RotateTarot contents
      "Incursion" -> do
        contents <- o .: "contents"
        pure $ Incursion contents
      "PlaceSwarmCards" -> do
        contents <- o .: "contents"
        pure $ uncurry3 PlaceSwarmCards contents
      "PlacedSwarmCard" -> do
        contents <- o .: "contents"
        pure $ uncurry PlacedSwarmCard contents
      "UpdateLocation" -> do
        contents <- o .: "contents"
        pure $ uncurry UpdateLocation contents
      "UpdateEnemy" -> do
        contents <- o .: "contents"
        pure $ uncurry UpdateEnemy contents
      "If" -> do
        contents <- o .: "contents"
        pure $ uncurry If contents
      "SendMessage" -> do
        contents <- o .: "contents"
        pure $ uncurry SendMessage contents
      "IfEnemyExists" -> do
        contents <- o .: "contents"
        pure $ uncurry IfEnemyExists contents
      "ExcessDamage" -> do
        contents <- o .: "contents"
        pure $ uncurry ExcessDamage contents
      "AddDeckBuildingAdjustment" -> do
        contents <- o .: "contents"
        pure $ uncurry AddDeckBuildingAdjustment contents
      "IncreaseCustomization" -> do
        contents <- o .: "contents"
        pure $ uncurry4 IncreaseCustomization contents
      "Do" -> do
        contents <- o .: "contents"
        pure $ Do contents
      "DoBatch" -> do
        contents <- o .: "contents"
        pure $ uncurry DoBatch contents
      "ClearUI" -> pure ClearUI
      _ -> error $ "Unknown message type: " <> show tag

uncurry3 :: forall a b c d. (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

uncurry5 :: forall a b c d e f. (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f (a, b, c, d, e) = f a b c d e

uncurry6 :: forall a b c d e f g. (a -> b -> c -> d -> e -> f -> g) -> (a, b, c, d, e, f) -> g
uncurry6 f (a, b, c, d, e, f') = f a b c d e f'

uncurry7
  :: forall a b c d e f g h. (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 f (a, b, c, d, e, f', g) = f a b c d e f' g

uncurry8
  :: forall a b c d e f g h i
   . (a -> b -> c -> d -> e -> f -> g -> h -> i)
  -> (a, b, c, d, e, f, g, h)
  -> i
uncurry8 f (a, b, c, d, e, f', g, h) = f a b c d e f' g h

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
  SkipTriggersButton _ -> Run []

chooseOrRunOne :: PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid msgs = chooseOne pid msgs

questionLabel :: Text -> PlayerId -> Question Message -> Message
questionLabel lbl pid q = Ask pid (QuestionLabel lbl Nothing q)

questionLabelWithCard :: Text -> CardCode -> PlayerId -> Question Message -> Message
questionLabelWithCard lbl cCode pid q = Ask pid (QuestionLabel lbl (Just cCode) q)

chooseOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "No messages for chooseOne"
chooseOne pid msgs = Ask pid (ChooseOne msgs)

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
  :: Targetable target
  => PlayerId
  -> Text
  -> AmountTarget
  -> [(Text, (Int, Int))]
  -> target
  -> Message
chooseAmounts pid label total choiceMap (toTarget -> target) =
  Ask
    pid
    (ChooseAmounts label total amountChoices target)
 where
  amountChoices = map toAmountChoice choiceMap
  toAmountChoice (l, (m, n)) = AmountChoice l m n

chooseUpgradeDeck :: PlayerId -> Message
chooseUpgradeDeck pid = Ask pid ChooseUpgradeDeck

chooseDecks :: [PlayerId] -> Message
chooseDecks pids = AskMap $ mapFromList $ map (,ChooseDeck) pids

module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  , LeftoverCardStrategy(..)
  , ChoosePlayerChoice(..)
  , MessageType(..)
  , ActionType(..)
  , messageType
  , chooseOne
  , chooseOneAtATime
  , chooseSome
  , chooseN
  , resolve
  , story
  )
where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Card
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Card.Id
import Arkham.Types.ChaosBagStepState
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.ScenarioId
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Control.Exception

data MessageType
  = RevelationMessage
  | AttackMessage
  | DrawTokenMessage
  | RevealTokenMessage
  | ResolveTokenMessage
  | EnemySpawnMessage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType RevealToken{} = Just RevealTokenMessage
messageType _ = Nothing

resolve :: Message -> [Message]
resolve msg = [When msg, msg, After msg]

story :: [InvestigatorId] -> Message -> Message
story iids msg = AskMap
  (mapFromList
    [ (iid, ChooseOne [Run [Continue "Continue", msg]]) | iid <- iids ]
  )

data EncounterCardSource = FromDiscard | FromEncounterDeck | FromTheVoid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeftoverCardStrategy = ShuffleBackIn | PutBackInAnyOrder
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionType
  = EnemyActionType
  | LocationActionType
  | AssetActionType
  | TreacheryActionType
  | ActActionType
  | AgendaActionType
  | InvestigatorActionType
  deriving stock (Bounded, Enum)

data Message
  = Setup
  | SetupStep Int
  | ApplyModifiers Target
  | BeginTurn InvestigatorId
  | AddToken Token
  | CompleteObjective
  | AddCampaignCardToDeck InvestigatorId CardCode
  | AddCampaignCardToEncounterDeck CardCode
  | RemoveCampaignCardFromDeck InvestigatorId CardCode
  | SetLocationLabel LocationId Text
  | EndOfGame
  | EndOfScenario -- used by the game to clear queue
  | StartCampaign
  | ResetGame
  | CampaignStep (Maybe CampaignStep)
  | NextCampaignStep (Maybe CampaignStep)
  | Remember ScenarioLogKey
  | Record CampaignLogKey
  | CrossOutRecord CampaignLogKey
  | RecordCount CampaignLogKey Int
  | RecordSet CampaignLogKey [CardCode]
  | StartScenario ScenarioId
  | SetupInvestigators
  | TakeStartingResources InvestigatorId
  | FlavorText (Maybe Text) [Text]
  | InvestigatorMulligan InvestigatorId
  | FinishedWithMulligan InvestigatorId
  | SearchCollectionForRandom InvestigatorId Source (PlayerCardType, Maybe Trait)
  | SearchDeckForTraits InvestigatorId Target [Trait]
  | SearchTopOfDeck InvestigatorId Target Int [Trait] LeftoverCardStrategy
  | SearchDiscard InvestigatorId Target [Trait]
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemovedFromPlay Source
  | RemoveFromGame Target
  | RemoveFromDiscard InvestigatorId CardId
  | InitDeck InvestigatorId [PlayerCard] -- used to initialize the deck for the campaign
  | LoadDeck InvestigatorId [PlayerCard] -- used to reset the deck of the investigator
  | EndPhase
  | BeginRound
  | EndRoundWindow
  | EndRound
  | BeginMythos
  | EndMythos
  | BeginInvestigation
  | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
  | EndInvestigation
  | BeginEnemy
  | EndEnemy
  | BeginUpkeep
  | EndUpkeep
  | HuntersMove
  | EnemiesAttack
  | ReadyExhausted
  | Exhaust Target
  | Ready Target
  | ReadyAlternative Source Target
  | AllDrawCardAndResource
  | AllCheckHandSize
  | CheckHandSize InvestigatorId
  | PlaceDoomOnAgenda
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgenda AgendaId
  | AdvanceCurrentAgenda
  | AdvanceAct ActId
  | RevertAct ActId
  | NextAdvanceActStep ActId Int
  | AllDrawEncounterCard
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
  | RequestedEncounterCards Target [EncounterCard]
  | PlaceLocation LocationId
  | PlaceLocationNamed LocationName
  | PlacedLocation LocationId
  | AddConnection LocationId LocationSymbol
  | AddConnectionBack LocationId LocationSymbol
  | AddedConnection LocationId LocationId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | RemoveLocation LocationId
  | RemoveEnemy EnemyId
  | MoveAllTo LocationId
  | MoveAction InvestigatorId LocationId Bool
  | Move InvestigatorId LocationId LocationId
  | MoveTo InvestigatorId LocationId
  | MoveFrom InvestigatorId LocationId
  | MoveUntil LocationId Target
  | MoveToward Target LocationMatcher
  | PlayerWindow InvestigatorId [Message]
  | Ask InvestigatorId Question
  | AskMap (HashMap InvestigatorId Question)
  | TakeAction InvestigatorId Int (Maybe Action)
  | LoseActions InvestigatorId Source Int
  | SetActions InvestigatorId Source Int
  | GainActions InvestigatorId Source Int
  | UseLimitedAbility InvestigatorId Ability
  | ChooseActivateCardAbilityAction InvestigatorId
  | ActivateCardAbilityAction InvestigatorId Ability
  | ActivateCardAbilityActionWithDynamicCost InvestigatorId Ability
  | UseCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int
  | PayForCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int
  | UseScenarioSpecificAbility InvestigatorId Int
  | PutSetAsideIntoPlay Target
  | AddUses Target UseType Int
  | ResolveToken DrawnToken Token InvestigatorId
  | Investigate InvestigatorId LocationId Source SkillType Bool
  | ChooseFightEnemy InvestigatorId Source SkillType Bool
  | ChooseFightEnemyNotEngagedWithInvestigator InvestigatorId Source SkillType Bool
  | ChooseEvadeEnemy InvestigatorId Source SkillType Bool
  | EngageEnemy InvestigatorId EnemyId Bool
  | DisengageEnemy InvestigatorId EnemyId
  | ChooseEndTurn InvestigatorId
  | EndTurn InvestigatorId
  | CheckAttackOfOpportunity InvestigatorId Bool
  | TakeResources InvestigatorId Int Bool
  | SpendResources InvestigatorId Int
  | EnemyWillAttack InvestigatorId EnemyId
  | EnemyAttacks [Message]
  | EnemyAttack InvestigatorId EnemyId
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | PerformEnemyAttack InvestigatorId EnemyId
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
  | InvestigatorDrewPlayerCard InvestigatorId PlayerCard
  | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
  | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnAtLocationNamed (Maybe InvestigatorId) LocationName EnemyId
  | CreateEnemyRequest Source CardCode
  | RequestedEnemy Source EnemyId
  | EnemySpawnedAt LocationId EnemyId
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | InvestigatorDamageEnemy InvestigatorId EnemyId
  | InvestigatorDamageInvestigator InvestigatorId InvestigatorId
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId LocationId CardCode Source [Trait]
  | Damage Target Source Int
  | AddToVictory Target
  | InitiatePlayCard InvestigatorId CardId (Maybe Target) Bool
  | PlayCard InvestigatorId CardId (Maybe Target) Bool
  | InitiatePlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
  | PlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
  | PlayedCard InvestigatorId CardId
  | PayedForDynamicCard InvestigatorId CardId Int Bool
  | InvestigatorTakeDamage InvestigatorId Source Int Int
  | InvestigatorDirectDamage InvestigatorId Source Int Int
  | InvestigatorAssignDamage InvestigatorId Source Int Int
  -- ^ uses the internal method and then checks defeat
  | InvestigatorDoAssignDamage InvestigatorId Source Int Int [Target] [Target]
  -- ^ meant to be used internally by investigators          ^ damage ^ horror
  | DidReceiveDamage Target Source
  | DidReceiveHorror Target Source
  | GainXP InvestigatorId Int
  | SufferTrauma InvestigatorId Int Int
  | AssetDamage AssetId Source Int Int
  | AssetDefeated AssetId
  | Discarded Target Card
  | InvestigatorDamage InvestigatorId Source Int Int
  | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
  | InvestigatorPlayDynamicAsset InvestigatorId AssetId [SlotType] [Trait] Int
  | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target)
  | ResolveEvent InvestigatorId EventId (Maybe Target)
  | BeginTrade InvestigatorId Target [InvestigatorId]
  | InvestigatorPlayDynamicEvent InvestigatorId EventId Int
  | GainClues InvestigatorId Int
  | PlaceClues Target Int
  | RemoveClues Target Int
  | PlaceResources Target Int
  | InvestigatorDiscoverClues InvestigatorId LocationId Int
  | InvestigatorDiscoverCluesAtTheirLocation InvestigatorId Int
  | DiscoverClues InvestigatorId LocationId Int
  | DiscoverCluesAtLocation InvestigatorId LocationId Int
  | AfterDiscoverClues InvestigatorId LocationId Int
  | BeginSkillTest InvestigatorId Source Target (Maybe Action) SkillType Int
  | BeginSkillTestAfterFast InvestigatorId Source Target (Maybe Action) SkillType Int
  | StartSkillTest InvestigatorId
  | BeforeSkillTest InvestigatorId SkillType
  | TriggerSkillTest InvestigatorId
  | RunSkillTest InvestigatorId
  | RerunSkillTest
  | RunSkillTestSourceNotification InvestigatorId Source
  | HandlePointOfFailure InvestigatorId Target Int -- Really do x n times, does not have to be failure
  | SkillTestResults
  | SkillTestApplyResults
  | SkillTestApplyResultsAfter
  | SkillTestCommitCard InvestigatorId CardId
  | CommitCard InvestigatorId CardId
  | InvestigatorCommittedCard InvestigatorId CardId
  | InvestigatorCommittedSkill InvestigatorId SkillId
  | SkillTestAsk Message
  | SkillTestUncommitCard InvestigatorId CardId
  | AddSkillTestSubscriber Target
  | PassSkillTest
  | FailSkillTest
  | InvestigatorPlaceCluesOnLocation InvestigatorId Int
  | InvestigatorPlaceAllCluesOnLocation InvestigatorId
  -- ^ This message exists in case the number of clues will change
  | FindAndDrawEncounterCard InvestigatorId EncounterCardMatcher
  | FindEncounterCard InvestigatorId Target EncounterCardMatcher
  | FoundEncounterCard InvestigatorId Target EncounterCard
  | FoundEncounterCardFrom InvestigatorId Target EncounterCardSource EncounterCard
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | FoundEnemyInVoid InvestigatorId EnemyId
  | AddToEncounterDeck EncounterCard
  | SkillTestEnds
  | ReturnSkillTestRevealedTokens
  | RevealToken Source InvestigatorId Token
  | RevealSkillTestTokens InvestigatorId
  | DrawToken InvestigatorId Token
  | EmptyDeck InvestigatorId
  | DeckHasNoCards InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | HealHorror Target Int
  | HealDamage Target Int
  | HealAllDamage Target
  | DrewPlayerTreachery InvestigatorId CardCode CardId
  | DrewPlayerEnemy InvestigatorId CardCode CardId
  | RemoveCardFromHand InvestigatorId CardCode
  | AddToDiscard InvestigatorId PlayerCard
  | ChooseAndDiscardCard InvestigatorId
  | DiscardCard InvestigatorId CardId
  | DiscardTopOfDeck InvestigatorId Int (Maybe Target)
  | DiscardTopOfEncounterDeck InvestigatorId Int (Maybe Target)
  | RemoveFromEncounterDiscard EncounterCard
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Target
  | DrewTreachery InvestigatorId CardCode
  | PayCardCost InvestigatorId CardId
  | PayDynamicCardCost InvestigatorId CardId Int [Message]
  | AddAct ActId
  | AddAgenda AgendaId
  | AllRandomDiscard
  | RandomDiscard InvestigatorId
  | NextAgenda AgendaId AgendaId
  | NextAct ActId ActId
  | WhenEnterLocation InvestigatorId LocationId
  | AfterEnterLocation InvestigatorId LocationId
  | EnemyMove EnemyId LocationId LocationId
  | SpawnEnemyAt Card LocationId
  | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
  | CreateEnemyAt CardCode LocationId
  | CreateEnemyAtLocationNamed CardCode LocationName
  | CreateEnemyEngagedWithPrey CardCode
  | EnemySpawnEngagedWithPrey EnemyId
  | Revelation InvestigatorId Source
  | AfterRevelation InvestigatorId TreacheryId
  | RevelationSkillTest InvestigatorId Source SkillType Int
  | Discard Target
  | SetEncounterDeck [EncounterCard]
  | ChooseAndDiscardAsset InvestigatorId
  | FightEnemy InvestigatorId EnemyId Source SkillType Bool
  | WhenAttackEnemy InvestigatorId EnemyId
  | AttackEnemy InvestigatorId EnemyId Source SkillType
  | AfterAttackEnemy InvestigatorId EnemyId
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId Source SkillType Bool
  | TryEvadeEnemy InvestigatorId EnemyId Source SkillType
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation InvestigatorId LocationId Source
  | SuccessfulAttackEnemy InvestigatorId EnemyId
  | FailedAttackEnemy InvestigatorId EnemyId
  | AttachTreachery TreacheryId Target
  | AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AddSlot InvestigatorId SlotType Slot
  | RefillSlots InvestigatorId SlotType [AssetId]
  | RequestedEncounterCard Source (Maybe EncounterCard)
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard)
  | ShuffleIntoEncounterDeck [EncounterCard]
  | ShuffleBackIntoEncounterDeck Target
  | ShuffleEncounterDiscardBackIn
  | ShuffleAllInEncounterDiscardBackIn CardCode
  | ShuffleDiscardBackIn InvestigatorId
  | DiscardEncounterUntilFirst Source EncounterCardMatcher
  | SpendClues Int [InvestigatorId]
  | InvestigatorSpendClues InvestigatorId Int
  | CreateWeaknessInThreatArea CardCode InvestigatorId
  | AttachStoryTreacheryTo CardCode Target
  | CreateStoryAssetAt CardCode LocationId
  | CreateStoryAssetAtLocationNamed CardCode LocationName
  | TakeControlOfAsset InvestigatorId AssetId
  | TakeControlOfSetAsideAsset InvestigatorId CardCode
  | PutCardIntoPlay InvestigatorId Card (Maybe Target)
  | Resolution Int
  | GameOver
  | NoResolution
  | Resign InvestigatorId
  | InvestigatorKilled InvestigatorId
  | InvestigatorWhenDefeated InvestigatorId
  | InvestigatorDefeated InvestigatorId
  | AddAbility Source Ability
  | RemoveAbilitiesFrom Source
  | InvestigatorResigned InvestigatorId
  | AllInvestigatorsResigned
  | InvestigatorWhenEliminated InvestigatorId
  | InvestigatorEliminated InvestigatorId
  | CheckWindow InvestigatorId [Window]
  | EndCheckWindow
  | CancelNext MessageType
  | Run [Message]
  | Continue Text
  | AddToHandFromDeck InvestigatorId CardId
  | FocusCards [Card]
  | UnfocusCards
  | AddFocusedToHand InvestigatorId Target CardId
  | AddFocusedToTopOfDeck InvestigatorId Target CardId
  | ShuffleAllFocusedIntoDeck InvestigatorId Target
  | ShuffleCardsIntoDeck InvestigatorId [PlayerCard]
  | PutOnTopOfDeck InvestigatorId PlayerCard
  | PutOnTopOfEncounterDeck InvestigatorId EncounterCard
  | AddToHand InvestigatorId Card
  | EnemySetBearer EnemyId BearerId
  | CheckDefeated
  | ChooseLeadInvestigator
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | Label Text [Message]
  | TargetLabel Target [Message]
  | UnengageNonMatching InvestigatorId [Trait]
  | PlaceDoom Target Int
  | RemoveDoom Target Int
  | RemoveAllDoom
  | Surge InvestigatorId Source
  | RevealInHand CardId
  | RemoveDiscardFromGame InvestigatorId
  | FailedSkillTest InvestigatorId (Maybe Action) Source Target Int
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target Int
  | ReturnToHand InvestigatorId Target
  | ShuffleIntoDeck InvestigatorId Target
  | FocusTokens [Token]
  | UnfocusTokens
  | Will Message
  | When Message
  | After Message
  | Blanked Message
  | DrawAnotherToken InvestigatorId
  | SetTokens [Token]
  | SetTokensForScenario
  | ResetTokens Source
  | ReturnTokens [Token]
  | RequestTokens Source (Maybe InvestigatorId) Int RequestedTokenStrategy
  | RunBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RequestedTokens Source (Maybe InvestigatorId) [Token]
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
  | AddTraits Target [Trait]
  | RemoveTraits Target [Trait]
  | ChangeCardToFast InvestigatorId CardId
  | CreateEffect CardCode (Maybe (EffectMetadata Message)) Source Target
  | CreateSkillTestEffect (EffectMetadata Message) Source Target
  | CreateTokenValueEffect Int Source Target
  | CreatePhaseEffect (EffectMetadata Message) Source Target
  | CreatedEffect EffectId (Maybe (EffectMetadata Message)) Source Target
  | DisableEffect EffectId
  | Done
  | PlaceEnemyInVoid EnemyId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

chooseOne :: InvestigatorId -> [Message] -> Message
chooseOne _ [] = throw $ InvalidState "No messages"
chooseOne iid msgs = Ask iid (ChooseOne msgs)

chooseOneAtATime :: InvestigatorId -> [Message] -> Message
chooseOneAtATime _ [] = throw $ InvalidState "No messages"
chooseOneAtATime iid msgs = Ask iid (ChooseOneAtATime msgs)

chooseSome :: InvestigatorId -> [Message] -> Message
chooseSome _ [] = throw $ InvalidState "No messages"
chooseSome iid msgs = Ask iid (ChooseSome $ Done : msgs)

chooseN :: InvestigatorId -> Int -> [Message] -> Message
chooseN _ _ [] = throw $ InvalidState "No messages"
chooseN iid n msgs = Ask iid (ChooseN n msgs)

data Question
  = ChooseOne [Message]
  | ChooseN Int [Message]
  | ChooseSome [Message]
  | ChooseOneAtATime [Message]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice
  = SetLeadInvestigator
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  , LeftoverCardStrategy(..)
  , ChoosePlayerChoice(..)
  , MessageType(..)
  , ActionType(..)
  , messageType
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.Asset.Uses
import Arkham.Types.AssetId
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.ChaosBagStepState
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.ScenarioId
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TokenResponse
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import ClassyPrelude

data MessageType = RevelationMessage | AttackMessage | DrawTokenMessage | ResolveTokenMessage | EnemySpawnMessage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType _ = Nothing

data EncounterCardSource = FromDiscard | FromEncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeftoverCardStrategy = ShuffleBackIn | PutBackInAnyOrder
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionType = EnemyActionType | LocationActionType | AssetActionType | TreacheryActionType | ActActionType | AgendaActionType | InvestigatorActionType

data Message
  = Setup
  | ApplyModifiers Target
  | BeginTurn InvestigatorId
  | AddToken Token
  | AddCampaignCardToDeck InvestigatorId PlayerCard
  | SetLocationLabel LocationId Text
  | EndOfGame
  | EndOfScenario -- used by the game to clear queue
  | StartCampaign
  | ResetGame
  | CampaignStep (Maybe CampaignStep)
  | NextCampaignStep
  | Record CampaignLogKey
  | CrossOutRecord CampaignLogKey
  | RecordCount CampaignLogKey Int
  | RecordSet CampaignLogKey [CardCode]
  | StartScenario ScenarioId
  | SetupInvestigators
  | FlavorText (Maybe Text) [Text]
  | InvestigatorMulligan InvestigatorId
  | FinishedWithMulligan InvestigatorId
  | SearchCollectionForRandom InvestigatorId Source (PlayerCardType, Maybe Trait)
  | SearchDeckForTraits InvestigatorId Target [Trait]
  | SearchTopOfDeck InvestigatorId Target Int [Trait] LeftoverCardStrategy
  | SearchDiscard InvestigatorId Target [Trait]
  | RemoveFromDiscard InvestigatorId CardId
  | InitDeck InvestigatorId [PlayerCard] -- used to initialize the deck for the campaign
  | LoadDeck InvestigatorId [PlayerCard] -- used to reset the deck of the investigator
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
  | AllDrawCardAndResource
  | AllCheckHandSize
  | CheckHandSize InvestigatorId
  | PlaceDoomOnAgenda
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgenda AgendaId
  | AdvanceAct ActId
  | AllDrawEncounterCard
  | PlaceLocation LocationId
  | PlacedLocation LocationId
  | AddConnection LocationId LocationSymbol
  | AddConnectionBack LocationId LocationSymbol
  | AddedConnection LocationId LocationId
  | RevealLocation LocationId
  | RemoveLocation LocationId
  | RemoveEnemy EnemyId
  | MoveAllTo LocationId
  | MoveAction InvestigatorId LocationId Bool
  | MoveTo InvestigatorId LocationId
  | MoveFrom InvestigatorId LocationId
  | PrePlayerWindow
  | PostPlayerWindow
  | PlayerWindow InvestigatorId [Message]
  | Ask InvestigatorId Question
  | AskMap (HashMap InvestigatorId Question)
  | TakeAction InvestigatorId Int (Maybe Action)
  | LoseActions InvestigatorId Source Int
  | GainActions InvestigatorId Source Int
  | ChooseActivateCardAbilityAction InvestigatorId
  | ActivateCardAbilityAction InvestigatorId Ability
  | UseCardAbility InvestigatorId Source Source (Maybe AbilityMetadata) Int
  | UseScenarioSpecificAbility InvestigatorId Int
  | AddUses Target UseType Int
  | ResolveToken Token InvestigatorId
  | Investigate InvestigatorId LocationId SkillType [Modifier] [TokenResponse Message] [Message] Bool
  | ChooseFightEnemy InvestigatorId SkillType [Modifier] [TokenResponse Message] Bool
  | ChooseEvadeEnemy InvestigatorId SkillType [Message] [Message] [TokenResponse Message] Bool
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
  | PerformEnemyAttack InvestigatorId EnemyId
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
  | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
  | EnemySpawn LocationId EnemyId
  | CreateEnemyRequest Source CardCode
  | RequestedEnemy Source EnemyId
  | EnemySpawnedAt LocationId EnemyId
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | InvestigatorDamageEnemy InvestigatorId EnemyId
  | InvestigatorDamageInvestigator InvestigatorId InvestigatorId
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId CardCode Source
  | Damage Target Source Int
  | AddToVictory Target
  | PlayCard InvestigatorId CardId Bool
  | PlayDynamicCard InvestigatorId CardId Int Bool -- Int is unused for Bool True
  | PlayedCard InvestigatorId CardId Bool
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
  | Discarded Target CardCode
  | InvestigatorDamage InvestigatorId Source Int Int
  | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
  | InvestigatorPlayDynamicAsset InvestigatorId AssetId [SlotType] [Trait] Int
  | InvestigatorPlayEvent InvestigatorId EventId
  | InvestigatorPlayDynamicEvent InvestigatorId EventId Int
  | GainClues InvestigatorId Int
  | DiscoverCluesAtLocation InvestigatorId LocationId Int
  | PlaceClues Target Int
  | InvestigatorDiscoverClues InvestigatorId LocationId Int
  | InvestigatorDiscoverCluesAtTheirLocation InvestigatorId Int
  | DiscoverClues InvestigatorId LocationId Int
  | AfterDiscoverClues InvestigatorId LocationId Int
  | BeginSkillTest InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier] [TokenResponse Message]
  | BeginSkillTestAfterFast InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier] [TokenResponse Message]
  | StartSkillTest InvestigatorId
  | BeforeSkillTest InvestigatorId SkillType
  | TriggerSkillTest InvestigatorId
  | RunSkillTest InvestigatorId TokenValue
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
  | FindAndDrawEncounterCard InvestigatorId
                         (EncounterCardType, Maybe Trait)
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | AddToEncounterDeck EncounterCard
  | SetAsideToken Token
  | SkillTestEnds
  | ReturnSkillTestRevealedTokens
  | RevealToken Source InvestigatorId Token
  | RevealSkillTestTokens InvestigatorId
  | DrawToken InvestigatorId Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | HealHorror Target Int
  | HealDamage Target Int
  | DrewPlayerTreachery InvestigatorId CardCode CardId
  | DrewPlayerEnemy InvestigatorId CardCode CardId
  | RemoveCardFromHand InvestigatorId CardCode
  | AddToDiscard InvestigatorId PlayerCard
  | ChooseAndDiscardCard InvestigatorId
  | DiscardCard InvestigatorId CardId
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
  | CreateEnemyAt CardCode LocationId
  | CreateEnemyEngagedWithPrey CardCode
  | EnemySpawnEngagedWithPrey EnemyId
  | Revelation InvestigatorId TreacheryId
  | AfterRevelation InvestigatorId TreacheryId
  | RevelationSkillTest InvestigatorId Source SkillType Int [Message] [Message]
  | Discard Target
  | SetEncounterDeck [EncounterCard]
  | ChooseAndDiscardAsset InvestigatorId
  | FightEnemy InvestigatorId EnemyId SkillType [Modifier] [TokenResponse Message] Bool
  | WhenAttackEnemy InvestigatorId EnemyId
  | AttackEnemy InvestigatorId EnemyId SkillType [Modifier] [TokenResponse Message]
  | AfterAttackEnemy InvestigatorId EnemyId
  | AfterEnemyAttacks EnemyId Target
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId SkillType [Message] [Message] [TokenResponse Message] Bool
  | TryEvadeEnemy InvestigatorId EnemyId SkillType [Message] [Message] [Modifier] [TokenResponse Message]
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation InvestigatorId LocationId
  | SuccessfulAttackEnemy InvestigatorId EnemyId
  | FailedAttackEnemy InvestigatorId EnemyId
  | AttachTreachery TreacheryId Target
  | AttachEventToLocation EventId LocationId
  | AddModifiers Target Source [Modifier]
  | SetModifiers Target Source [Modifier]
  | AddSlot InvestigatorId SlotType Slot
  | RefillSlots InvestigatorId SlotType [AssetId]
  | RemoveAllModifiersOnTargetFrom Target Source
  | RemoveAllModifiersFrom Source
  | RequestedEncounterCard Source (Maybe EncounterCard)
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard)
  | ShuffleEncounterDiscardBackIn
  | ShuffleDiscardBackIn InvestigatorId
  | DiscardEncounterUntilFirst Source (EncounterCardType, Maybe Trait)
  | SpendClues Int [InvestigatorId]
  | InvestigatorSpendClues InvestigatorId Int
  | CreateStoryAssetAt CardCode LocationId
  | PutCardIntoPlay InvestigatorId Card
  | AddAssetAt AssetId LocationId
  | Resolution Int
  | GameOver
  | NoResolution
  | Resign InvestigatorId
  | InvestigatorWhenDefeated InvestigatorId
  | InvestigatorDefeated InvestigatorId
  | AddAbility Source Ability
  | RemoveAbilitiesFrom Source
  | TakeControlOfAsset InvestigatorId AssetId
  | InvestigatorResigned InvestigatorId
  | InvestigatorWhenEliminated InvestigatorId
  | InvestigatorEliminated InvestigatorId
  | CheckWindow InvestigatorId [Window]
  | EndCheckWindow
  | CancelNext MessageType
  | Run [Message]
  | Continue Text
  | AddToHandFromDeck InvestigatorId CardId
  | FocusCards [Card]
  | AddFocusedToHand InvestigatorId Target CardId
  | AddFocusedToTopOfDeck InvestigatorId Target CardId
  | ShuffleAllFocusedIntoDeck InvestigatorId Target
  | ShuffleCardsIntoDeck InvestigatorId [PlayerCard]
  | PutOnTopOfDeck InvestigatorId PlayerCard
  | PutOnTopOfEncounterDeck InvestigatorId EncounterCard
  | AddToHand InvestigatorId Card
  | EnemySetBearer EnemyId BearerId
  | SetBearer CardId BearerId
  | CheckDefeated
  | ChooseLeadInvestigator
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | Label Text [Message]
  | TargetLabel Target [Message]
  | UnengageNonMatching InvestigatorId [Trait]
  | PlaceDoom Target Int
  | Surge InvestigatorId
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
  | DrawAnotherToken InvestigatorId Int
  | SetTokens [Token]
  | ResetTokens Source
  | ReturnTokens [Token]
  | RequestTokens Source InvestigatorId Int RequestedTokenStrategy
  | RunBag Source InvestigatorId RequestedTokenStrategy
  | RunDrawFromBag Source InvestigatorId RequestedTokenStrategy
  | RequestedTokens Source InvestigatorId [Token]
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | NextChaosBagStep Source InvestigatorId RequestedTokenStrategy
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Message]
  | ChooseOneAtATime [Message]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice
  = SetLeadInvestigator
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

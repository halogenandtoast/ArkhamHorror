module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  , LeftoverCardStrategy(..)
  , MessageType(..)
  , messageType
  )
where

import Arkham.Json
import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.Action
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.EnemyId
import Arkham.Types.FastWindow
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.SkillTestResult
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.TokenResponse
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

data MessageType = RevelationMessage | AttackMessage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType _ = Nothing

data EncounterCardSource = FromDiscard | FromEncounterDeck
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeftoverCardStrategy = ShuffleBackIn | PutBackInAnyOrder
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = Setup
  | Log Text
  | SetupInvestigators
  | FlavorText (Maybe Text) Text
  | InvestigatorMulligan InvestigatorId
  | MulliganCard InvestigatorId CardId
  | FinishedWithMulligan InvestigatorId
  | SearchDeckForTraits InvestigatorId Target [Trait]
  | SearchTopOfDeck InvestigatorId Target Int [Trait] LeftoverCardStrategy
  | RunEvent InvestigatorId CardCode
  | LoadDeck InvestigatorId [PlayerCard]
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
  | AllDrawCardAndResource
  | AllCheckHandSize
  | CheckHandSize InvestigatorId
  | AddDoom Target
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
  | PrePlayerWindow
  | PostPlayerWindow
  | PlayerWindow InvestigatorId [Message]
  | Ask InvestigatorId Question
  | AskMap (HashMap InvestigatorId Question)
  | TakeAction InvestigatorId Int (Maybe Action)
  | LoseAction InvestigatorId Source
  | ChoosePlayCardAction InvestigatorId
  | ChooseActivateCardAbilityAction InvestigatorId
  | ActivateCardAbilityAction InvestigatorId Ability
  | UseCardAbility InvestigatorId Ability
  | ResolveToken Token InvestigatorId Int
  | Investigate InvestigatorId LocationId SkillType [TokenResponse Message] Bool
  | ChooseFightEnemy InvestigatorId SkillType [Modifier] [TokenResponse Message] Bool
  | ChooseEvadeEnemy InvestigatorId SkillType [Message] [Message] [TokenResponse Message] Bool
  | EngageEnemy InvestigatorId EnemyId Bool
  | ChooseEndTurn InvestigatorId
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
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | InvestigatorDamageEnemy InvestigatorId EnemyId
  | InvestigatorDamageInvestigator InvestigatorId InvestigatorId
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId CardCode Source
  | PlayCard InvestigatorId CardId Bool
  | PlayedCard InvestigatorId CardId Bool
  | InvestigatorAssignDamage InvestigatorId Source Int Int
  -- ^ uses the internal method and then checks defeat
  | InvestigatorDoAssignDamage InvestigatorId Source Int Int
  -- ^ meant to be used internally by investigators
  | AssetDamage AssetId Source Int Int
  | AssetDefeated AssetId
  | DiscardAsset AssetId
  | AssetDiscarded AssetId CardCode
  | InvestigatorDamage InvestigatorId Source Int Int
  | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
  | DiscoverCluesAtLocation InvestigatorId LocationId Int
  | InvestigatorDiscoverClues InvestigatorId LocationId Int
  | DiscoverClues InvestigatorId LocationId Int
  | AfterDiscoverClues InvestigatorId LocationId Int
  | BeginSkillTest InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier] [TokenResponse Message]
  | BeginSkillTestAfterFast InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier] [TokenResponse Message]
  | StartSkillTest InvestigatorId
  | InvestigatorStartSkillTest InvestigatorId (Maybe Action) SkillType [Modifier]
  | BeforeSkillTest InvestigatorId SkillType
  | TriggerSkillTest InvestigatorId SkillType Int
  | RunSkillTest Int
  | SkillTestResults
  | SkillTestApplyResults
  | RunSkill InvestigatorId CardCode SkillTestResult
  | SkillTestCommitCard InvestigatorId CardId
  | SkillTestAsk Message
  | SkillTestUncommitCard InvestigatorId CardId
  | AddOnFailure Message
  | AddOnSuccess Message
  | FailSkillTest
  | FindAndDrawEncounterCard InvestigatorId
                         (EncounterCardType, Trait)
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | DrawAnotherToken InvestigatorId Int Token
  | SkillTestEnds
  | SkillTestEnded SkillTestResult [Token]
  | ReturnTokens [Token]
  | DrawToken Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | HealHorror Target Int
  | HealDamage Target Int
  | DrewRevelation InvestigatorId CardCode CardId
  | DrewPlayerEnemy InvestigatorId CardCode CardId
  | RemoveCardFromHand InvestigatorId CardCode
  | AddToDiscard InvestigatorId PlayerCard
  | DiscardCard InvestigatorId CardId
  | DrewTreachery InvestigatorId CardCode
  | PayCardCost InvestigatorId CardId
  | AddAct ActId
  | AddAgenda AgendaId
  | AllRandomDiscard
  | NextAgenda AgendaId AgendaId
  | NextAct ActId ActId
  | WhenEnterLocation InvestigatorId LocationId
  | AfterEnterLocation InvestigatorId LocationId
  | EnemyMove EnemyId LocationId LocationId
  | CreateEnemyAt CardCode LocationId
  | Revelation InvestigatorId TreacheryId
  | AfterRevelation InvestigatorId TreacheryId
  | RevelationSkillTest InvestigatorId Source SkillType Int [Message] [Message]
  | DamagePerPointOfFailure InvestigatorId
  | HorrorPerPointOfFailure InvestigatorId
  | Discard Target
  | SetEncounterDeck [EncounterCard]
  | TreacheryFailure InvestigatorId TreacheryId -- TODO: better name
  | ChooseAndDiscardAsset InvestigatorId
  | FightEnemy InvestigatorId EnemyId SkillType [Modifier] [TokenResponse Message] Bool
  | WhenAttackEnemy InvestigatorId EnemyId
  | AttackEnemy InvestigatorId EnemyId SkillType [Modifier] [TokenResponse Message]
  | AfterAttackEnemy InvestigatorId EnemyId
  | AfterEnemyAttacks EnemyId Target
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId SkillType [Message] [Message] [TokenResponse Message] Bool
  | TryEvadeEnemy InvestigatorId EnemyId SkillType [Message] [Message] [TokenResponse Message]
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation InvestigatorId LocationId
  | SuccessfulAttackEnemy InvestigatorId EnemyId
  | FailedAttackEnemy InvestigatorId EnemyId
  | AttachTreacheryToLocation TreacheryId LocationId
  | AttachTreacheryToInvestigator TreacheryId InvestigatorId
  | AddModifier Target Modifier
  | AddSlot InvestigatorId SlotType Slot
  | RefillSlots InvestigatorId SlotType [AssetId]
  | RemoveAllModifiersOnTargetFrom Target Source
  | RequestedEncounterCard Source (Maybe EncounterCard)
  | ShuffleEncounterDiscardBackIn
  | ShuffleDiscardBackIn InvestigatorId
  | DiscardEncounterUntilFirst Source (EncounterCardType, Trait)
  | SpendClues Int [InvestigatorId]
  | InvestigatorSpendClues InvestigatorId Int
  | CreateStoryAssetAt CardCode LocationId
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
  | CheckFastWindow InvestigatorId [FastWindow]
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
  | CheckDefeated
  | Label Text [Message]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Message]
  | ChooseOneAtATime [Message]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

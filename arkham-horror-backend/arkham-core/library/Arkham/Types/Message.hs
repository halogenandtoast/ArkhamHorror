module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  )
where

import Arkham.Types.Ability
import Arkham.Types.ActId
import Arkham.Types.AgendaId
import Arkham.Types.AssetId
import Arkham.Types.Card
import Arkham.Types.EnemyId
import Arkham.Types.FastWindow
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationSymbol
import Arkham.Types.Modifier
import Arkham.Types.SkillTestResult
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude
import Data.Aeson

data EncounterCardSource = FromDiscard | FromEncounterDeck
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = Setup
  | RunEvent InvestigatorId CardCode
  | LoadDeck InvestigatorId [PlayerCard]
  | BeginRound
  | EndRoundWindow
  | EndRound
  | BeginMythos
  | EndMythos
  | BeginInvestigation
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
  | MoveAction InvestigatorId LocationId
  | MoveTo InvestigatorId LocationId
  | PrePlayerWindow
  | PostPlayerWindow
  | PlayerWindow InvestigatorId
  | Ask Question
  | ChooseTakeResourceAction InvestigatorId
  | ChooseDrawCardAction InvestigatorId
  | ChoosePlayCardAction InvestigatorId
  | ChooseActivateCardAbilityAction InvestigatorId
  | ActivateCardAbilityAction InvestigatorId Ability
  | UseCardAbility InvestigatorId Ability
  | ResolveToken Token InvestigatorId Int
  | ChooseMoveAction InvestigatorId
  | ChooseInvestigateAction InvestigatorId SkillType [Modifier]
  | Investigate SkillType InvestigatorId LocationId [Modifier]
  | ChooseFightEnemyAction InvestigatorId SkillType [Modifier]
  | ChooseEvadeEnemyAction InvestigatorId
  | ChooseEngageEnemyAction InvestigatorId
  | ChooseEndTurn InvestigatorId
  | CheckAttackOfOpportunity InvestigatorId
  | TakeResources InvestigatorId Int
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
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId CardCode Source
  | InvestigatorPlayCard InvestigatorId CardCode Int
  | InvestigatorAssignDamage InvestigatorId EnemyId Int Int
  | AssetDamage AssetId EnemyId Int Int
  | AssetDefeated AssetId
  | DiscardAsset AssetId
  | AssetDiscarded AssetId CardCode
  | InvestigatorDamage InvestigatorId Source Int Int
  | InvestigatorPlayAsset InvestigatorId AssetId
  | DiscoverCluesAtLocation InvestigatorId LocationId Int
  | InvestigatorDiscoverClues InvestigatorId LocationId Int
  | DiscoverClues InvestigatorId LocationId Int
  | AfterDiscoverClues InvestigatorId LocationId Int
  | BeginSkillTest InvestigatorId SkillType Int [Message]
                [Message]
  | StartSkillTest
  | InvestigatorStartSkillTest InvestigatorId SkillType [Modifier]
  | BeforeSkillTest InvestigatorId SkillType
  | TriggerSkillTest InvestigatorId SkillType Int
  | RunSkillTest Int
  | SkillTestResults
  | SkillTestApplyResults
  | RunSkill InvestigatorId CardCode SkillTestResult
  | SkillTestCommitCard InvestigatorId (Int, Card)
  | AddOnFailure Message
  | AddOnSuccess Message
  | FailSkillTest
  | FindAndDrawEncounterCard InvestigatorId
                         (EncounterCardType, Trait)
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource (Int, EncounterCard)
  | DrawAnotherToken InvestigatorId Int Token
  | SkillTestEnds
  | ReturnTokens [Token]
  | DrawToken Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int
  | DrewPlayerTreachery InvestigatorId CardCode
  | RemoveCardFromHand InvestigatorId CardCode
  | DrewTreachery InvestigatorId CardCode
  | PayCardCost InvestigatorId CardCode Int
  | AddAct ActId
  | AddAgenda AgendaId
  | AllRandomDiscard
  | NextAgenda AgendaId AgendaId
  | NextAct ActId ActId
  | WhenEnterLocation InvestigatorId LocationId
  | AfterEnterLocation InvestigatorId LocationId
  | EnemyMove EnemyId LocationId LocationId
  | CreateEnemyAt CardCode LocationId
  | RunTreachery InvestigatorId TreacheryId
  | RevelationSkillTest InvestigatorId SkillType Int [Message] [Message]
  | DamagePerPointOfFailure InvestigatorId
  | HorrorPerPointOfFailure InvestigatorId
  | DiscardTreachery TreacheryId
  | SetEncounterDeck [EncounterCard]
  | TreacheryFailure InvestigatorId TreacheryId -- TODO: better name
  | ChooseAndDiscardAsset InvestigatorId
  | FightEnemy InvestigatorId EnemyId SkillType [Modifier]
  | WhenAttackEnemy InvestigatorId EnemyId
  | AttackEnemy InvestigatorId EnemyId SkillType Int
  | AfterAttackEnemy InvestigatorId EnemyId
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId SkillType
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation LocationId
  | AttachTreacheryToLocation TreacheryId LocationId
  | LocationIncreaseShroud LocationId Int
  | LocationDecreaseShroud LocationId Int
  | AttachTreacheryToInvestigator TreacheryId InvestigatorId
  | InvestigatorAddModifier InvestigatorId Modifier
  | SkillTestAddModifier Modifier
  | EnemyAddModifier EnemyId Modifier
  | InvestigatorRemoveAllModifiersFromSource InvestigatorId Source
  | EnemyRemoveAllModifiersFromSource EnemyId Source
  | RequestedEncounterCard Source (Maybe EncounterCard)
  | ShuffleEncounterDiscardBackIn
  | ShuffleDiscardBackIn InvestigatorId
  | DiscardEncounterUntilFirst Source (EncounterCardType, Trait)
  | SpendClues Int [InvestigatorId]
  | InvestigatorSpendClues InvestigatorId Int
  | CreateStoryAssetAt CardCode LocationId
  | AddAssetAt AssetId LocationId
  | Resolution Int
  | NoResolution
  | Resign InvestigatorId
  | InvestigatorWhenDefeated InvestigatorId
  | InvestigatorDefeated InvestigatorId
  | AddAbility Source Ability
  | RemoveAbilitiesFrom Source
  | TakeControlOfAsset InvestigatorId AssetId
  | InvestigatorResigned InvestigatorId
  | CheckFastWindow InvestigatorId [FastWindow]
  | CancelNextAttack
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Question]
  | ChooseOneAtATime [Question]
  | ChoiceResult Message
  | ChoiceResults [Message]
  | ChooseTo Message
  | ChooseToDoAll [Message]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

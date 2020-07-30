module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  )
where

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
import Arkham.Types.Source
import Arkham.Types.Target
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
  | PlayerWindow InvestigatorId
  | Ask Question
  | TakeAction InvestigatorId Int Action
  | ChoosePlayCardAction InvestigatorId
  | ChooseActivateCardAbilityAction InvestigatorId
  | ActivateCardAbilityAction InvestigatorId Ability
  | UseCardAbility InvestigatorId Ability
  | ResolveToken Token InvestigatorId Int
  | Investigate InvestigatorId LocationId SkillType Bool
  | ChooseFightEnemy InvestigatorId SkillType [Modifier] Bool
  | ChooseEvadeEnemy InvestigatorId SkillType Bool
  | ChooseEngageEnemyAction InvestigatorId
  | ChooseEndTurn InvestigatorId
  | CheckAttackOfOpportunity InvestigatorId
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
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId CardCode Source
  | PlayCard InvestigatorId CardId Bool
  | PlayedCard InvestigatorId CardId
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
  | BeginSkillTest InvestigatorId Source SkillType Int [Message]
                [Message] [Modifier]
  | StartSkillTest
  | InvestigatorStartSkillTest InvestigatorId SkillType [Modifier]
  | BeforeSkillTest InvestigatorId SkillType
  | TriggerSkillTest InvestigatorId SkillType Int
  | RunSkillTest Int
  | SkillTestResults
  | SkillTestApplyResults
  | RunSkill InvestigatorId CardCode SkillTestResult
  | SkillTestCommitCard InvestigatorId CardId
  | SkillTestUncommitCard InvestigatorId CardId
  | AddOnFailure Message
  | AddOnSuccess Message
  | FailSkillTest
  | FindAndDrawEncounterCard InvestigatorId
                         (EncounterCardType, Trait)
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | DrawAnotherToken InvestigatorId Int Token
  | SkillTestEnds
  | ReturnTokens [Token]
  | DrawToken Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | HealHorror Target Int
  | DrewPlayerTreachery InvestigatorId CardCode
  | RemoveCardFromHand InvestigatorId CardCode
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
  | RunTreachery InvestigatorId TreacheryId
  | RevelationSkillTest InvestigatorId Source SkillType Int [Message] [Message]
  | DamagePerPointOfFailure InvestigatorId
  | HorrorPerPointOfFailure InvestigatorId
  | Discard Target
  | SetEncounterDeck [EncounterCard]
  | TreacheryFailure InvestigatorId TreacheryId -- TODO: better name
  | ChooseAndDiscardAsset InvestigatorId
  | FightEnemy InvestigatorId EnemyId SkillType [Modifier] Bool
  | WhenAttackEnemy InvestigatorId EnemyId
  | AttackEnemy InvestigatorId EnemyId SkillType [Modifier]
  | AfterAttackEnemy InvestigatorId EnemyId
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId SkillType Bool
  | TryEvadeEnemy InvestigatorId EnemyId SkillType
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation LocationId
  | AttachTreacheryToLocation TreacheryId LocationId
  | AttachTreacheryToInvestigator TreacheryId InvestigatorId
  | AddModifier Target Modifier
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
  | Run [Message]
  | Continue Text
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question
  = ChooseOne [Message]
  | ChooseOneFromSource Source [Message]
  | ChooseOneAtATime [Message]
  | ChooseTo Message
  | ChooseToDoAll [Message]
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

module Arkham.Types.Message
  ( Message(..)
  , Question(..)
  , EncounterCardSource(..)
  , ChooseOneFromSource(..)
  , LeftoverCardStrategy(..)
  , Labeled(..)
  , label
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
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import ClassyPrelude

data EncounterCardSource = FromDiscard | FromEncounterDeck
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeftoverCardStrategy = ShuffleBackIn | PutBackInAnyOrder
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = Setup
  | SearchDeckForTraits InvestigatorId [Trait]
  | SearchTopOfDeck InvestigatorId Int [Trait] LeftoverCardStrategy
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
  | PlayerWindow InvestigatorId [Message]
  | Ask Question
  | TakeAction InvestigatorId Int (Maybe Action)
  | LoseAction InvestigatorId Source
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
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemyDefeated EnemyId InvestigatorId CardCode Source
  | PlayCard InvestigatorId CardId Bool
  | PlayedCard InvestigatorId CardId Bool
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
  | BeginSkillTest InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier]
  | BeginSkillTestAfterFast InvestigatorId Source (Maybe Action) SkillType Int [Message]
                [Message] [Modifier]
  | StartSkillTest
  | InvestigatorStartSkillTest InvestigatorId (Maybe Action) SkillType [Modifier]
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
  | SkillTestEnded SkillTestResult [Token]
  | ReturnTokens [Token]
  | DrawToken Token
  | EmptyDeck InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | WhenDrawCard InvestigatorId CardCoad
  | HealHorror Target Int
  | HealDamage Target Int
  | DrewRevelation InvestigatorId CardCode CardId
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
  | AfterEnemyAttacks EnemyId Target
  | WhenEvadeEnemy InvestigatorId EnemyId
  | EvadeEnemy InvestigatorId EnemyId SkillType Bool
  | TryEvadeEnemy InvestigatorId EnemyId SkillType
  | EnemyEvaded InvestigatorId EnemyId
  | AfterEvadeEnemy InvestigatorId EnemyId
  | SuccessfulInvestigation InvestigatorId LocationId
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
  | AddToHandFromDeck InvestigatorId CardId
  | FocusCards [Card]
  | AddFocusedToHand InvestigatorId CardId
  | AddFocusedToTopOfDeck InvestigatorId CardId
  | ShuffleAllFocusedIntoDeck InvestigatorId
  | ShuffleCardsIntoDeck InvestigatorId [PlayerCard]
  | PutOnTopOfDeck InvestigatorId PlayerCard
  | AddToHand InvestigatorId Card
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Labeled a = Labeled
  { labelFor :: Text
  , unlabel :: a
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

label :: Text -> a -> Labeled a
label t a = Labeled { labelFor = t, unlabel = a }

data ChooseOneFromSource = MkChooseOneFromSource { chooseOneSource :: Source, chooseOneChoices :: [Labeled Message] }
  deriving stock (Show, Generic)

instance ToJSON ChooseOneFromSource where
  toJSON = genericToJSON $ aesonOptions $ Just "chooseOne"
  toEncoding = genericToEncoding $ aesonOptions $ Just "chooseOne"

instance FromJSON ChooseOneFromSource where
  parseJSON = genericParseJSON $ aesonOptions $ Just "chooseOne"

data Question
  = ChooseOne [Message]
  | ChooseOneFromSource ChooseOneFromSource
  | ChooseOneAtATime [Message]
  | ChooseTo Message
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

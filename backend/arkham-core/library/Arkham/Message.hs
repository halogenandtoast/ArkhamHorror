{-# LANGUAGE TemplateHaskell #-}
module Arkham.Message
  ( module Arkham.Message
  , module X
  ) where

import Arkham.Prelude

import Arkham.Message.Type as X
import Arkham.Question as X
import Arkham.Strategy as X

import Arkham.Ability
import Arkham.Act.Sequence
import Arkham.Action
import Arkham.Asset.Uses
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Card
import Arkham.Card.Id
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck
import Arkham.Direction
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard.Source
import Arkham.Exception
import Arkham.Helpers
import Arkham.Id
import Arkham.Location.Base
import Arkham.Matcher hiding ( EnemyDefeated, InvestigatorDefeated )
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.RequestedTokenStrategy
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Arkham.Window ( Window )
import Arkham.Zone
import Control.Exception
import Data.Aeson.TH

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType EnemyDefeated{} = Just EnemyDefeatedMessage
messageType RevealToken{} = Just RevealTokenMessage
messageType InvestigatorDamage{} = Just DamageMessage
messageType InvestigatorDoAssignDamage{} = Just DamageMessage
messageType InvestigatorDrewEncounterCard{} = Just DrawEncounterCardMessage
messageType InvestigatorDefeated{} = Just InvestigatorDefeatedMessage
messageType RunWindow{} = Just RunWindowMessage
messageType _ = Nothing

isBlanked :: Message -> Bool
isBlanked Blanked{} = True
isBlanked _ = False

resolve :: Message -> [Message]
resolve msg = [When msg, msg, After msg]

story :: [InvestigatorId] -> Message -> Message
story iids msg = AskMap
  (mapFromList
    [ (iid, ChooseOne [Run [Continue "Continue", msg]]) | iid <- iids ]
  )

-- TODO: Better handle in play and out of play
-- Out of play refers to player's hand, in any deck,
-- in any discard pile, the victory display, and
-- cards that have been set aside

data AdvancementMethod = AdvancedWithClues | AdvancedWithOther
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

{- | Masking rules
UseCardAbility: Because some abilities have a discard self cost, the card of
the ability will have already been discarded when we go to resolve this. While
we could use InDiscard in the RunMessage instance for that card's entity, there
may be cases where we can trigger abilities without paying the cost, so we want
it to be accessible from both.
-}
doNotMask :: Message -> Bool
doNotMask UseCardAbility{} = True
doNotMask _ = False

data Message
  = UseAbility InvestigatorId Ability [Window]
  | -- Story Card Messages
    ReadStory InvestigatorId CardDef
  | ResolveStory InvestigatorId CardDef
  | Do Message
  | -- Handle discard costs
    DiscardedCost Target
  | -- Act Deck Messages
    SetActDeck
  | AddAct CardDef
  | AdvanceAct ActId Source AdvancementMethod
  | NextAdvanceActStep ActId Int
  | ReplaceAct ActId ActId
  | RevertAct ActId
  | AdvanceActDeck Int Source
  | AdvanceToAct Int CardDef ActSide Source
  | -- Agenda Deck Messages
    SetAgendaDeck
  | AddAgenda Int CardDef
  | AdvanceAgenda AgendaId
  | NextAdvanceAgendaStep AgendaId Int
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgendaDeck Int Source
  | AdvanceCurrentAgenda
  | ReplaceLocation LocationId Card
  | ReplaceAgenda AgendaId AgendaId
  | RevertAgenda AgendaId
  | ResetAgendaDeckToStage Int
  | -- No Remaining Investigator Messages
    SetNoRemainingInvestigatorsHandler Target
  | HandleNoRemainingInvestigators Target
  | CheckForRemainingInvestigators
  | AddDirectConnection LocationId LocationId
  | AddCampaignCardToDeck InvestigatorId CardDef
  | -- Adding Cards to Hand
    AddFocusedToHand InvestigatorId Target Zone CardId
  | AddToHand InvestigatorId Card
  | AddTreacheryToHand InvestigatorId TreacheryId
  | ReturnToHand InvestigatorId Target
  | -- Adding Cards to Deck
    AddFocusedToTopOfDeck InvestigatorId Target CardId
  | -- Adding Cards to Player Discard
    AddToDiscard InvestigatorId PlayerCard
  | AddToEncounterDiscard EncounterCard
  | -- Slot Messages
    AddSlot InvestigatorId SlotType Slot
  | -- Adding Cards to Encounter Deck
    AddToEncounterDeck EncounterCard
  | AddToTopOfEncounterDeck EncounterCard
  | -- Scenario Deck Messages
    AddToScenarioDeck ScenarioDeckKey Target
  | AddCardToScenarioDeck ScenarioDeckKey Card
  | ShuffleScenarioDeckIntoEncounterDeck ScenarioDeckKey
  | DrawFromScenarioDeck InvestigatorId ScenarioDeckKey Target Int
  | DrawRandomFromScenarioDeck InvestigatorId ScenarioDeckKey Target Int
  | DrewFromScenarioDeck InvestigatorId ScenarioDeckKey Target [Card]
  | -- Victory
    AddToVictory Target
  | DefeatedAddToVictory Target
  | -- Tokens
    AddToken TokenFace
  | RemoveAllTokens TokenFace
  | -- Asset Uses
    AddUses Target UseType Int
  | -- Asks
    AskPlayer Message
  | Ask InvestigatorId (Question Message)
  | AskMap (HashMap InvestigatorId (Question Message))
  | After Message -- TODO: REMOVE
  | AfterEvadeEnemy InvestigatorId EnemyId
  | AfterRevelation InvestigatorId TreacheryId
  | AllCheckHandSize
  | AllDrawCardAndResource
  | AllDrawEncounterCard
  | AllInvestigatorsResigned
  | AllRandomDiscard
  | AssetDamage AssetId Source Int Int
  | AssetDefeated AssetId
  | -- Attach
    AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AttachStoryTreacheryTo Card Target
  | AttachTreachery TreacheryId Target
  | AttackEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType
  | BeforeRevealTokens
  | BeforeSkillTest InvestigatorId SkillType Int
  | -- Game State Control
    Begin Phase
  | BeginRound
  | BeginSkillTest InvestigatorId Source Target (Maybe Action) SkillType Int
  | BeginSkillTestAfterFast InvestigatorId Source Target (Maybe Action) SkillType Int
  | BeginTrade InvestigatorId Target [InvestigatorId]
  | BeginTurn InvestigatorId
  | Blanked Message
  | CampaignStep (Maybe CampaignStep)
  | CancelNext MessageType
  | CancelSkillEffects
  | CancelHorror InvestigatorId Int
  | CancelDamage InvestigatorId Int
  | CancelFailedByModifierEffects
  | CheckAttackOfOpportunity InvestigatorId Bool
  | CheckDefeated Source
  | CheckHandSize InvestigatorId
  | CheckWindow [InvestigatorId] [Window]
  | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
  | RunWindow InvestigatorId [Window]
  | ChooseAndDiscardAsset InvestigatorId AssetMatcher
  | ChooseAndDiscardCard InvestigatorId
  | ChooseEndTurn InvestigatorId
  | ChooseEvadeEnemy InvestigatorId Source (Maybe Target) SkillType EnemyMatcher Bool
  | ChooseFightEnemy InvestigatorId Source (Maybe Target) SkillType EnemyMatcher Bool
  | ChooseLeadInvestigator
  | StandaloneSetup
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
  | ChooseRandomLocation Target (HashSet LocationId)
  | ChosenRandomLocation Target LocationId
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | CommitCard InvestigatorId CardId
  | Continue Text
  | CreateEffect CardCode (Maybe (EffectMetadata Window Message)) Source Target
  | CreateEnemy Card
  | CreateEnemyAt Card LocationId (Maybe Target)
  | CreatedEnemyAt EnemyId LocationId Target
  | CreateEnemyAtLocationMatching Card LocationMatcher
  | CreateEnemyEngagedWithPrey Card
  -- new payment bs
  | PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | PayCost ActiveCostId InvestigatorId Bool Cost
  | PayCostFinished ActiveCostId
  | PaidCost ActiveCostId InvestigatorId (Maybe Action) Payment
  -- end  new payment bs
  | CreateWindowModifierEffect EffectWindow (EffectMetadata Window Message) Source Target
  | CreateTokenEffect (EffectMetadata Window Message) Source Token
  | CreateStoryAssetAt Card LocationId
  | PlaceAsset AssetId Placement
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateTokenValueEffect Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Window Message)) Source Target
  | CrossOutRecord CampaignLogKey
  | Damage Target Source Int
  | DeckHasNoCards InvestigatorId (Maybe Target)
  | DisableEffect EffectId
  | Discard Target
  | DiscardCard InvestigatorId CardId
  | DiscardHand InvestigatorId
  | DiscardEncounterUntilFirst Source CardMatcher
  | DiscardTopOfDeck InvestigatorId Int (Maybe Target)
  | DiscardTopOfEncounterDeck InvestigatorId Int (Maybe Target)
  | DiscardTopOfEncounterDeckWithDiscardedCards InvestigatorId Int (Maybe Target) [EncounterCard]
  | Discarded Target Card
  | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Target
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Target
  | DiscoverClues InvestigatorId LocationId Int (Maybe Action)
  | DiscoverCluesAtLocation InvestigatorId LocationId Int (Maybe Action)
  | DisengageEnemy InvestigatorId EnemyId
  | DisengageEnemyFromAll EnemyId
  | Done Text
  | DrawAnotherToken InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
  | DrawToken InvestigatorId Token
  | DrewPlayerEnemy InvestigatorId Card
  | DrewTreachery InvestigatorId Card
  | ResolveTreachery InvestigatorId TreacheryId
  | DrivenInsane InvestigatorId
  | EmptyDeck InvestigatorId
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
  | EndRoundWindow
  | EndSearch InvestigatorId Source Target [(Zone, ZoneReturnStrategy)]
  | SearchEnded InvestigatorId
  | EndTurn InvestigatorId
  | EndUpkeep
  | EnemiesAttack
  | EnemyAttack InvestigatorId EnemyId DamageStrategy EnemyAttackType
  | InitiateEnemyAttack InvestigatorId EnemyId EnemyAttackType
  | EnemyAttackFromDiscard InvestigatorId Card
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | EnemyAttacks [Message]
  | CheckEnemyEngagement InvestigatorId
  | EnemyCheckEngagement EnemyId
  | EnemyDamage EnemyId InvestigatorId Source DamageEffect Int
  | -- Bool is for direct damage
    EnemyDamaged EnemyId InvestigatorId Source DamageEffect Int Bool -- INTERNAL ONLY
  | -- Used after modified amount has been determined
    DirectEnemyDamage EnemyId InvestigatorId Source DamageEffect Int
  | EnemySetDamage EnemyId Source Int
  | DefeatEnemy EnemyId InvestigatorId Source
  | EnemyDefeated EnemyId InvestigatorId CardCode Source [Trait]
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyMove EnemyId LocationId
  | EngagedEnemyMove EnemyId LocationId
  | EnemyEntered EnemyId LocationId
  | SetBearer Target InvestigatorId
  | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
  | EnemySpawnEngagedWithPrey EnemyId
  | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnedAt LocationId EnemyId
  | EnemyWillAttack InvestigatorId EnemyId DamageStrategy EnemyAttackType
  | EngageEnemy InvestigatorId EnemyId Bool
  | EvadeEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | Exhaust Target
  | FailSkillTest
  | FailedAttackEnemy InvestigatorId EnemyId
  | FailedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
  | FightEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | FindAndDrawEncounterCard InvestigatorId CardMatcher
  | FindEncounterCard InvestigatorId Target CardMatcher
  | FinishedWithMulligan InvestigatorId
  | FlavorText (Maybe Text) [Text]
  | FocusCards [Card]
  | FocusTargets [Target]
  | FocusTokens [Token]
  | Force Message
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | FoundEncounterCard InvestigatorId Target EncounterCard
  | FoundEncounterCardFrom InvestigatorId Target EncounterCardSource EncounterCard
  | FoundEnemyInVoid InvestigatorId Target EnemyId
  | GainActions InvestigatorId Source Int
  | GainClues InvestigatorId Int
  | GainXP InvestigatorId Int
  | GameOver
  | HandlePointOfFailure InvestigatorId Target Int -- Really do x n times, does not have to be failure
  | HealAllDamage Target
  | HealDamage Target Int
  | HealHorror Target Int
  | MovedHorror Source Target Int
  | HealHorrorWithAdditional Target Int
  | AdditionalHealHorror Target Int
  | HuntersMove
  | HunterMove EnemyId
  | InDiscard InvestigatorId Message
  | InSearch Message
  | InHand InvestigatorId Message
  | InitDeck InvestigatorId (Deck PlayerCard) -- used to initialize the deck for the campaign
  | UpgradeDeck InvestigatorId (Deck PlayerCard) -- used to upgrade deck during campaign
  | FinishedUpgradingDecks
  | Flip InvestigatorId Source Target
  | Flipped Source Card
  | InitiatePlayCardAsChoose InvestigatorId CardId [Card] [Message] ChosenCardStrategy Bool
  | InitiatePlayCardAs InvestigatorId CardId Card [Message] ChosenCardStrategy Bool
  | InitiatePlayCard InvestigatorId CardId (Maybe Target) Bool
  -- | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
  | CheckAdditionalActionCosts InvestigatorId Target Source Action [Message]
  | -- Maybe Target is handler for success
    Investigate InvestigatorId LocationId Source (Maybe Target) SkillType Bool
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
  | InvestigatorDiscardAllClues InvestigatorId
  | InvestigatorDiscoverClues InvestigatorId LocationId Int (Maybe Action)
  | InvestigatorDiscoverCluesAtTheirLocation InvestigatorId Int (Maybe Action)
  | -- | meant to be used internally by investigators                  ^ damage ^ horror
    InvestigatorDoAssignDamage InvestigatorId Source DamageStrategy Int Int [Target] [Target]
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDoDrawEncounterCard InvestigatorId
  | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
  | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
  | InvestigatorDrewPlayerCard InvestigatorId PlayerCard
  | InvestigatorEliminated InvestigatorId
  | InvestigatorKilled Source InvestigatorId
  | InvestigatorMulligan InvestigatorId
  | InvestigatorsMulligan
  | -- | This message exists in case the number of clues will change
    InvestigatorPlaceAllCluesOnLocation InvestigatorId
  | InvestigatorPlaceCluesOnLocation InvestigatorId Int
  | InvestigatorPlayAsset InvestigatorId AssetId
  | InvestigatorPlayedAsset InvestigatorId AssetId
  | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target) [Window] Zone
  | InvestigatorResigned InvestigatorId
  | InvestigatorSpendClues InvestigatorId Int
  | InvestigatorTakeDamage InvestigatorId Source Int Int
  | InvestigatorWhenDefeated Source InvestigatorId
  | InvestigatorWhenEliminated Source InvestigatorId
  | Label Text [Message]
  | CardLabel CardCode [Message]
  | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
  | LookAtRevealed InvestigatorId Source Target
  | LookAtTopOfDeck InvestigatorId Target Int
  | LoseActions InvestigatorId Source Int
  | LoseResources InvestigatorId Int
  | LoseAllResources InvestigatorId
  | SpendActions InvestigatorId Source (Maybe Action) Int
  | Move Source InvestigatorId LocationId LocationId
  | MoveAction InvestigatorId LocationId Cost Bool
  | MoveAllCluesTo Target
  | MoveAllTo Source LocationId
  | MoveFrom Source InvestigatorId LocationId
  | MoveTo Source InvestigatorId LocationId
  | MoveToward Target LocationMatcher
  | MoveTopOfDeckToBottom Source DeckSignifier Int
  | MoveUntil LocationId Target
  | NextCampaignStep (Maybe CampaignStep)
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
  | Noop
  | PassSkillTest
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
  | -- | Bool is to check if we should ignore additional costs
    PayAbilityCost Source InvestigatorId (Maybe Action) Bool Cost
  | PayAbilityCostFinished EffectId Source InvestigatorId
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId Card [Window]
  | PaidForCardCost InvestigatorId Card Payment
  | PayForCardAbility InvestigatorId Source [Window] Int Payment
  | PerformEnemyAttack InvestigatorId EnemyId DamageStrategy EnemyAttackType
  | PlaceClues Target Int
  | PlaceCluesUpToClueValue LocationId Int
  | FlipClues Target Int
  | PlaceDoom Target Int
  | PlaceHorror Target Int
  | PlaceDoomOnAgenda
  | PlaceEnemyInVoid EnemyId
  | PlaceLocation Card
  | PlaceLocationMatching CardMatcher
  | PlaceResources Target Int
  | PlaceUnderneath Target [Card]
  | PlacedUnderneath Target Card
  | PlaceNextTo Target [Card]
  | PlacedLocation Name CardCode LocationId
  | PlacedLocationDirection LocationId Direction LocationId
  | PlayCard InvestigatorId Card (Maybe Target) [Window] Bool
  -- | PlayFastEvent InvestigatorId CardId (Maybe Target) [Window]
  | PlayedCard InvestigatorId Card
  | ResolvedCard InvestigatorId Card
  | PlayerWindow InvestigatorId [Message] Bool
  | PutCardIntoPlay InvestigatorId Card (Maybe Target) [Window]
  | PutOnTopOfDeck InvestigatorId PlayerCard
  | PutOnTopOfEncounterDeck InvestigatorId EncounterCard
  | RandomDiscard InvestigatorId
  | Ready Target
  | ReadyAlternative Source Target
  | ReadyExhausted
  | Record CampaignLogKey
  | RecordCount CampaignLogKey Int
  | RecordSet CampaignLogKey [CardCode]
  | RecordSetInsert CampaignLogKey [CardCode]
  | CrossOutRecordSetEntries CampaignLogKey [CardCode]
  | RefillSlots InvestigatorId SlotType [AssetId]
  | Remember ScenarioLogKey
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemovePlayerCardFromGame Card
  | RemoveAllClues Target
  | RemoveAllDoom Source
  | RemoveCampaignCardFromDeck InvestigatorId CardCode
  | RemoveCardFromHand InvestigatorId CardId
  | RemoveCardFromSearch InvestigatorId CardId
  | RemoveClues Target Int
  | RemoveDiscardFromGame InvestigatorId
  | RemoveDoom Target Int
  | RemoveEnemy EnemyId
  | RemoveFromDiscard InvestigatorId CardId
  | RemoveFromEncounterDiscard EncounterCard
  | RemoveFromGame Target
  | RemovedFromGame Card
  | RemoveLocation LocationId
  | RemovedLocation LocationId
  | RemoveTraits Target [Trait]
  | RemoveFromPlay Source
  | RemovedFromPlay Source
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
  | RequestSetAsideCard Source CardCode
  | RequestTokens Source (Maybe InvestigatorId) RevealStrategy RequestedTokenStrategy
  | RequestedEncounterCard Source (Maybe EncounterCard)
  | RequestedEncounterCards Target [EncounterCard]
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard)
  | RequestedSetAsideCard Source Card
  | RequestedTokens Source (Maybe InvestigatorId) [Token]
  | RerunSkillTest
  | ResetGame
  | ResetTokens Source
  | Resign InvestigatorId
  | ResignWith Target
  | ResolveAmounts InvestigatorId [(Text, Int)] Target
  | ResolveEvent InvestigatorId EventId (Maybe Target) [Window]
  | ResolveToken Token TokenFace InvestigatorId -- since tokens can have their face changed we use this to represent that; TODO: use a real modifier
  | ReturnSkillTestRevealedTokens
  | ReturnTokens [Token]
  | RevealInHand CardId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | UnrevealLocation LocationId
  | RevealSkillTestTokens InvestigatorId
  | RevealToken Source InvestigatorId Token
  | Revelation InvestigatorId Source
  | RevelationChoice InvestigatorId Source Int
  | RevelationSkillTest InvestigatorId Source SkillType Int
  | Run [Message]
  | RunBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RunSkillTest InvestigatorId
  | RemoveFromBearersDeckOrDiscard PlayerCard
  | SearchCollectionForRandom InvestigatorId Source CardMatcher
  | Search InvestigatorId Source Target [(Zone, ZoneReturnStrategy)] CardMatcher FoundCardsStrategy
  | SearchFound InvestigatorId Target DeckSignifier [Card]
  | FoundCards (HashMap Zone [Card])
  | SearchNoneFound InvestigatorId Target
  | SetActions InvestigatorId Source Int
  | SetEncounterDeck (Deck EncounterCard)
  | SetLocationLabel LocationId Text
  | SetRole InvestigatorId ClassSymbol
  | ForceTokenDraw TokenFace
  | SetActiveInvestigator InvestigatorId
  | SetTokens [TokenFace]
  | SetTokensForScenario
  | Setup
  | EndSetup
  | SetupInvestigators
  | SetupStep Int
  | ShuffleAllFocusedIntoDeck InvestigatorId Target
  | PutAllFocusedIntoDiscard InvestigatorId Target
  | ShuffleAllInEncounterDiscardBackIn CardCode
  | ShuffleBackIntoEncounterDeck Target
  | ShuffleCardsIntoDeck InvestigatorId [PlayerCard]
  | PlaceOnBottomOfDeck InvestigatorId PlayerCard
  | ShuffleDiscardBackIn InvestigatorId
  | ShuffleEncounterDiscardBackIn
  | ShuffleIntoDeck InvestigatorId Target
  | ShuffleIntoEncounterDeck [EncounterCard]
  | SkillTestApplyResults
  | SkillTestApplyResultsAfter
  | SkillTestAsk Message
  | SkillTestCommitCard InvestigatorId Card
  | SkillTestEnds Source
  | AfterSkillTestEnds Source Target Int
  | SkillTestResults Int Int Int Int
  | SkillTestUncommitCard InvestigatorId Card
  | SpawnEnemyAt Card LocationId
  | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
  | SpendClues Int [InvestigatorId]
  | SpendResources InvestigatorId Int
  | SpendUses Target UseType Int
  | StartCampaign
  | StartScenario ScenarioId
  | StartSkillTest InvestigatorId
  | -- There are two targets, one associated to the action and one
    -- to handle the result
    Successful (Action, Target) InvestigatorId Source Target Int
  | SufferTrauma InvestigatorId Int Int
  | Surge InvestigatorId Source
  | TakeAction InvestigatorId (Maybe Action) Cost
  | TakeControlOfAsset InvestigatorId AssetId
  | ReplaceInvestigatorAsset InvestigatorId Card
  | ReplacedInvestigatorAsset InvestigatorId AssetId
  | TakeControlOfSetAsideAsset InvestigatorId Card
  | TakeResources InvestigatorId Int Bool
  | DrawStartingHand InvestigatorId
  | TakeStartingResources InvestigatorId
  | TakenAction InvestigatorId Action
  | TargetLabel Target [Message]
  | SkillLabel SkillType [Message]
  | EvadeLabel EnemyId [Message]
  | ChosenEvadeEnemy Source EnemyId
  | TriggerSkillTest InvestigatorId
  | TryEvadeEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType
  | UnfocusCards
  | UnfocusTargets
  | UnfocusTokens
  | SealToken Token
  | SealedToken Token Card
  | UnsealToken Token
  | UnsetActiveCard
  | UseCardAbility InvestigatorId Source [Window] Int Payment
  | UseCardAbilityChoice InvestigatorId Source [Window] Int Payment AbilityMetadata
  | UseCardAbilityChoiceTarget InvestigatorId Source [Window] Int Payment Target
  | ResetMetadata Target
  | When Message
  | WhenWillEnterLocation InvestigatorId LocationId
  | EnterLocation InvestigatorId LocationId
  | SetLocationAsIf InvestigatorId LocationId
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
  | -- Fields
    UpdateLocation LocationAttrs LocationId
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''Message)

targetLabel :: IdToTarget entityId => entityId -> [Message] -> Message
targetLabel entityId = TargetLabel (idToTarget entityId)

chooseOrRunOne :: InvestigatorId -> [Message] -> Message
chooseOrRunOne _ [x] = x
chooseOrRunOne iid msgs = chooseOne iid msgs

chooseOne :: InvestigatorId -> [Message] -> Message
chooseOne _ [] = throw $ InvalidState "No messages for chooseOne"
chooseOne iid msgs = Ask iid (ChooseOne msgs)

chooseOneAtATime :: InvestigatorId -> [Message] -> Message
chooseOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOneAtATime iid msgs = Ask iid (ChooseOneAtATime msgs)

chooseSome :: InvestigatorId -> Text -> [Message] -> Message
chooseSome _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseSome iid doneText msgs = Ask iid (ChooseSome $ Done doneText : msgs)

chooseUpToN :: InvestigatorId -> Int -> Text -> [Message] -> Message
chooseUpToN _ _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseUpToN iid n doneText msgs =
  Ask iid (ChooseUpToN n $ Done doneText : msgs)

chooseN :: InvestigatorId -> Int -> [Message] -> Message
chooseN _ _ [] = throw $ InvalidState "No messages for chooseN"
chooseN iid n msgs = Ask iid (ChooseN n msgs)

chooseAmounts
  :: InvestigatorId -> Text -> Int -> [(Text, (Int, Int))] -> Target -> Message
chooseAmounts iid label total choiceMap target =
  Ask iid (ChooseAmounts label total choiceMap target)

chooseUpgradeDeck :: InvestigatorId -> Message
chooseUpgradeDeck iid = Ask iid ChooseUpgradeDeck

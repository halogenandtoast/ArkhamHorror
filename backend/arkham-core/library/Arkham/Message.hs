{-# LANGUAGE TemplateHaskell #-}
module Arkham.Message
  ( module Arkham.Message
  , module X
  ) where

import Arkham.Prelude

import Arkham.Message.Type as X
import Arkham.Question as X
import Arkham.Strategy as X
import Arkham.Text as X

import Arkham.Ability
import Arkham.Act.Sequence
import Arkham.Action hiding ( Explore )
import Arkham.Action.Additional
import Arkham.Agenda.Sequence
import Arkham.Asset.Uses
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.CampaignStep
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck
import {-# SOURCE #-} Arkham.Discard
import Arkham.Direction
import Arkham.Draw.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard.Source
import Arkham.Exception
import Arkham.Helpers
import Arkham.History
import Arkham.Id
import Arkham.Matcher hiding ( EnemyDefeated, InvestigatorDefeated )
import Arkham.Movement
import Arkham.Name
import Arkham.Phase
import Arkham.Placement
import Arkham.RequestedTokenStrategy
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.ScenarioLogKey
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult qualified as SkillTest
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Trait
import Arkham.Window ( Window, WindowType )
import Control.Exception
import Data.Aeson.TH

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType InvestigatorDrawEnemy{} = Just DrawEnemyMessage
messageType EnemyDefeated{} = Just EnemyDefeatedMessage
messageType (Discard GameSource (EnemyTarget _)) = Just EnemyDefeatedMessage
messageType RevealToken{} = Just RevealTokenMessage
messageType InvestigatorDamage{} = Just DamageMessage
messageType InvestigatorDoAssignDamage{} = Just DamageMessage
messageType InvestigatorDrewEncounterCard{} = Just DrawEncounterCardMessage
messageType InvestigatorDefeated{} = Just InvestigatorDefeatedMessage
messageType RunWindow{} = Just RunWindowMessage
messageType Explore{} = Just ExploreMessage
messageType (Do msg) = messageType msg
messageType _ = Nothing

isBlanked :: Message -> Bool
isBlanked Blanked{} = True
isBlanked _ = False

resolve :: Message -> [Message]
resolve msg = [When msg, msg, After msg]

story :: [InvestigatorId] -> FlavorText -> Message
story iids flavor = AskMap
  (mapFromList [ (iid, Read flavor [Label "Continue" []]) | iid <- iids ])

storyWithChooseOne :: InvestigatorId -> [InvestigatorId] -> FlavorText -> [UI Message] -> Message
storyWithChooseOne lead iids flavor choices = AskMap
  (mapFromList [ (iid, Read flavor $ if iid == lead then choices else []) | iid <- iids ])

data AdvancementMethod = AdvancedWithClues | AdvancedWithOther
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

pattern CancelNext :: Source -> MessageType -> Message
pattern CancelNext source msgType = CancelEachNext source [msgType]

pattern AttachTreachery :: TreacheryId -> Target -> Message
pattern AttachTreachery tid target =
  PlaceTreachery tid (TreacheryAttachedTo target)

createCardEffect
  :: CardDef
  -> (Maybe (EffectMetadata Window Message))
  -> Source
  -> Target
  -> Message
createCardEffect def = CreateEffect (toCardCode def)

data AbilityRef = AbilityRef Source Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getChoiceAmount :: Text -> [(Text, Int)] -> Int
getChoiceAmount key choices =
 let choicesMap = mapFromList @(HashMap Text Int) choices
 in findWithDefault 0 key choicesMap

class IsMessage msg where
  toMessage :: msg -> Message

instance IsMessage Message where
  toMessage = id
  {-# INLINE toMessage #-}

instance IsMessage EnemyAttackDetails where
  toMessage = InitiateEnemyAttack
  {-# INLINE toMessage #-}

data Message
  = UseAbility InvestigatorId Ability [Window]
  | ResolvedAbility Ability -- INTERNAL
  | -- Story Card Messages
    ReadStory InvestigatorId CardDef
  | ResolveStory InvestigatorId CardDef
  | ResolveStoryStep InvestigatorId CardDef Int
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
  | AddAgenda Int Card
  | SetCurrentAgendaDeck Int [Card]
  | AdvanceAgenda AgendaId
  | AdvanceToAgenda Int CardDef AgendaSide Source
  | NextAdvanceAgendaStep AgendaId Int
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgendaDeck Int Source
  | AdvanceCurrentAgenda
  | ReplaceLocation LocationId Card
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
  | AddCampaignCardToDeck InvestigatorId CardDef
  | RemoveCardFromDeckForCampaign InvestigatorId PlayerCard
  | AddCardToDeckForCampaign InvestigatorId PlayerCard
  | -- Adding Cards to Hand
    AddFocusedToHand InvestigatorId Target Zone CardId
  | AddToHand InvestigatorId Card
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
  | -- Scenario Deck Messages
    AddToScenarioDeck ScenarioDeckKey Target
  | AddCardToScenarioDeck ScenarioDeckKey Card
  | ShuffleScenarioDeckIntoEncounterDeck ScenarioDeckKey
  | DrawFromScenarioDeck InvestigatorId ScenarioDeckKey Target Int
  | DrawRandomFromScenarioDeck InvestigatorId ScenarioDeckKey Target Int
  | DrewFromScenarioDeck InvestigatorId ScenarioDeckKey Target [Card]
  | SetScenarioDeck ScenarioDeckKey [Card]
  | RemoveCardFromScenarioDeck ScenarioDeckKey Card
  | -- Victory
    AddToVictory Target
  | DefeatedAddToVictory Target
  | -- Tokens
    AddToken TokenFace
  | RemoveAllTokens TokenFace
  | -- Asset Uses
    AddUses AssetId UseType Int
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
  | AllRandomDiscard Source CardMatcher
  | AssetDamage AssetId Source Int Int
  | AssetDefeated AssetId
  | -- Attach
    AttachAsset AssetId Target
  | AttachStoryTreacheryTo Card Target
  | AttackEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType
  | BeforeRevealTokens
  | BeforeSkillTest SkillTest
  | ChangeSkillTestType SkillTestType SkillTestBaseValue
  | -- Game State Control
    BeginGame
  | Begin Phase
  | BeginRound
  | BeginSkillTest SkillTest
  | BeginSkillTestAfterFast SkillTest
  | BeginTrade InvestigatorId Source Target [InvestigatorId]
  | BeginTurn InvestigatorId
  | Blanked Message
  | CampaignStep (Maybe CampaignStep)
  | CancelEachNext Source [MessageType]
  | CancelSkillEffects
  | CancelHorror InvestigatorId Int
  | CancelDamage InvestigatorId Int
  | CancelFailedByModifierEffects
  | CheckAttackOfOpportunity InvestigatorId Bool
  | CheckDefeated Source
  | AssignDamage Target
  | CancelAssignedDamage Target Int Int
  | CheckHandSize InvestigatorId
  | CheckWindow [InvestigatorId] [Window]
  | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
  | RunWindow InvestigatorId [Window]
  | ChooseAndDiscardAsset InvestigatorId Source AssetMatcher
  | DiscardFromHand HandDiscard
  | DoneDiscarding InvestigatorId
  | DiscardCard InvestigatorId Source CardId
  | ChooseEndTurn InvestigatorId
  | ChooseEvadeEnemy InvestigatorId Source (Maybe Target) SkillType EnemyMatcher Bool
  | ChooseFightEnemy InvestigatorId Source (Maybe Target) SkillType EnemyMatcher Bool
  | ChooseLeadInvestigator
  | PreScenarioSetup
  | StandaloneSetup
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
  | ChooseRandomLocation Target (HashSet LocationId)
  | ChosenRandomLocation Target LocationId
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | CommitCard InvestigatorId Card
  | Continue Text
  | CreateEffect CardCode (Maybe (EffectMetadata Window Message)) Source Target
  | ObtainCard Card
  | CreateEnemy EnemyId Card
  | CreateEnemyWithPlacement EnemyId Card Placement
  | CreateEnemyAt EnemyId Card LocationId (Maybe Target)
  | CreatedEnemyAt EnemyId LocationId Target
  | CreateEnemyAtLocationMatching EnemyId Card LocationMatcher
  | CreateEnemyEngagedWithPrey EnemyId Card
  -- new payment bs
  | PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | PayCost ActiveCostId InvestigatorId Bool Cost
  | PayCostFinished ActiveCostId
  | PaidCost ActiveCostId InvestigatorId (Maybe Action) Payment
  -- end  new payment bs
  | CreateWindowModifierEffect EffectWindow (EffectMetadata Window Message) Source Target
  | CreateTokenEffect (EffectMetadata Window Message) Source Token
  | CreateAssetAt AssetId Card Placement
  | CreateEventAt InvestigatorId Card Placement
  | PlaceAsset AssetId Placement
  | PlaceEvent InvestigatorId EventId Placement
  | PlaceTreachery TreacheryId TreacheryPlacement
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateTokenValueEffect Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Window Message)) Source Target
  | CrossOutRecord CampaignLogKey
  | Damage Target Source Int
  | DeckHasNoCards InvestigatorId (Maybe Target)
  | DisableEffect EffectId
  | Discard Source Target
  | DiscardHand InvestigatorId Source
  | DiscardEncounterUntilFirst Source (Maybe InvestigatorId) CardMatcher
  | RevealUntilFirst InvestigatorId Source DeckSignifier CardMatcher
  | RevealedCards InvestigatorId Source DeckSignifier (Maybe Card) [Card]
  | DiscardUntilFirst InvestigatorId Source CardMatcher
  | DiscardTopOfDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeckWithDiscardedCards InvestigatorId Int Source (Maybe Target) [EncounterCard]
  | Discarded Target Source Card
  | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Source Target
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Source Target
  | DiscoverClues InvestigatorId LocationId Int (Maybe Action)
  | DiscoverCluesAtLocation InvestigatorId LocationId Int (Maybe Action)
  | DisengageEnemy InvestigatorId EnemyId
  | DisengageEnemyFromAll EnemyId
  | DrawAnotherToken InvestigatorId
  | DrawCards CardDraw -- use drawCards
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
  | DrawToken InvestigatorId Token
  | DrewPlayerEnemy InvestigatorId Card
  | DrewTreachery InvestigatorId (Maybe DeckSignifier) Card
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
  | EnemyWillAttack EnemyAttackDetails
  | EnemyAttack EnemyAttackDetails
  | InitiateEnemyAttack EnemyAttackDetails
  | PerformEnemyAttack EnemyAttackDetails -- Internal
  | EnemyAttackFromDiscard InvestigatorId Card
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | EnemyAttacks [Message]
  | CheckEnemyEngagement InvestigatorId
  | EnemyCheckEngagement EnemyId
  | EnemyDamage EnemyId DamageAssignment
  | EnemyDamaged EnemyId DamageAssignment -- INTERNAL ONLY
  | DefeatEnemy EnemyId InvestigatorId Source
  | EnemyDefeated EnemyId CardCode Source [Trait]
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyMove EnemyId LocationId
  | EnemyEntered EnemyId LocationId
  | SetBearer Target InvestigatorId
  | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
  | EnemySpawnEngagedWithPrey EnemyId
  | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnedAt LocationId EnemyId
  | EngageEnemy InvestigatorId EnemyId Bool
  | EvadeEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | Exhaust Target
  | ExhaustThen Target [Message]
  | FailSkillTest
  | FailedAttackEnemy InvestigatorId EnemyId
  | FailedSkillTest InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | FightEnemy InvestigatorId EnemyId Source (Maybe Target) SkillType Bool
  | FindAndDrawEncounterCard InvestigatorId CardMatcher Bool
  | FindEncounterCard InvestigatorId Target [ScenarioZone] CardMatcher
  | FinishedWithMulligan InvestigatorId
  | FocusCards [Card]
  | FocusTokens [Token]
  | Force Message
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | FoundEncounterCard InvestigatorId Target EncounterCard
  | FoundEncounterCardFrom InvestigatorId Target EncounterCardSource EncounterCard
  | FoundEnemyInVoid InvestigatorId Target EnemyId
  | GainActions InvestigatorId Source Int
  | GainAdditionalAction InvestigatorId Source AdditionalAction
  | UseEffectAction InvestigatorId EffectId [Window]
  | GainClues InvestigatorId Int
  | GainXP InvestigatorId Int
  | SpendXP InvestigatorId Int
  | GameOver
  | HandlePointOfFailure InvestigatorId Target Int -- Really do x n times, does not have to be failure
  | HealAllDamage Target Source
  | HealDamage Target Source Int
  | HealHorror Target Source Int
  | MovedHorror Source Target Int
  | HealHorrorWithAdditional Target Source Int
  | AdditionalHealHorror Target Source Int
  | HealDamageDirectly Target Source Int
  | HealHorrorDirectly Target Source Int
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
  | InitiatePlayCardAsChoose InvestigatorId Card [Card] [Message] ChosenCardStrategy [Window] Bool
  | InitiatePlayCardAs InvestigatorId Card Card [Message] ChosenCardStrategy [Window] Bool
  | InitiatePlayCard InvestigatorId Card (Maybe Target) [Window] Bool
  -- | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
  | CheckAdditionalActionCosts InvestigatorId Target Action [Message]
  | CheckAdditionalCommitCosts InvestigatorId [Card]
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
    InvestigatorDoAssignDamage InvestigatorId Source DamageStrategy AssetMatcher Int Int [Target] [Target]
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDoDrawEncounterCard InvestigatorId
  | InvestigatorDrawEnemy InvestigatorId EnemyId
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
  | InvestigatorClearUnusedAssetSlots InvestigatorId
  | InvestigatorPlayedAsset InvestigatorId AssetId
  | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target) [Window] Zone
  | FinishedEvent EventId
  | InvestigatorResigned InvestigatorId
  | InvestigatorSpendClues InvestigatorId Int
  | InvestigatorWhenDefeated Source InvestigatorId
  | InvestigatorWhenEliminated Source InvestigatorId
  | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
  | LookAtRevealed InvestigatorId Source Target
  | LookAtTopOfDeck InvestigatorId Target Int
  | LoseActions InvestigatorId Source Int
  | LoseResources InvestigatorId Source Int
  | LoseAllResources InvestigatorId
  | SpendActions InvestigatorId Source (Maybe Action) Int
  | Move Movement
  | MoveAction InvestigatorId LocationId Cost Bool
  | MoveAllCluesTo Target
  | MoveAllTo Source LocationId
  | MoveFrom Source InvestigatorId LocationId
  | MoveTo Movement
  | MoveToward Target LocationMatcher
  | MoveTopOfDeckToBottom Source DeckSignifier Int
  | MoveUntil LocationId Target
  | NextCampaignStep (Maybe CampaignStep)
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
  | Noop
  | PassSkillTest
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | -- | Bool is to check if we should ignore additional costs
    PayAbilityCost Source InvestigatorId (Maybe Action) Bool Cost
  | PayAbilityCostFinished EffectId Source InvestigatorId
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId Card [Window]
  | PaidForCardCost InvestigatorId Card Payment
  | PayForCardAbility InvestigatorId Source [Window] Int Payment
  | PlaceClues Target Int
  | PlaceCluesUpToClueValue LocationId Int
  | FlipClues Target Int
  | FlipDoom Target Int
  | PlaceDoom Target Int
  | PlaceDamage Target Int
  | PlaceAdditionalDamage Target Source Int Int
  | PlaceHorror Target Int
  | PlaceDoomOnAgenda
  | PlaceEnemyInVoid EnemyId
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Card
  | PlaceLocationMatching CardMatcher
  | PlaceResources Target Int
  | RemoveResources Target Int
  | PlaceUnderneath Target [Card]
  | PlacedUnderneath Target Card
  | PlaceNextTo Target [Card]
  | PlacedLocation Name CardCode LocationId
  | PlacedLocationDirection LocationId Direction LocationId
  | PlayCard InvestigatorId Card (Maybe Target) [Window] Bool
  | CardEnteredPlay InvestigatorId Card
  | ResolvedCard InvestigatorId Card
  | PlayerWindow InvestigatorId [UI Message] Bool
  | PutCampaignCardIntoPlay InvestigatorId CardDef
  | PutCardIntoPlay InvestigatorId Card (Maybe Target) [Window]
  | PutCardOnTopOfDeck InvestigatorId DeckSignifier Card
  | PutOnTopOfDeck InvestigatorId DeckSignifier Target
  | PutCardOnBottomOfDeck InvestigatorId DeckSignifier Card
  | PutOnBottomOfDeck InvestigatorId DeckSignifier Target
  | Ready Target
  | ReadyAlternative Source Target
  | ReadyExhausted
  | Record CampaignLogKey
  | RecordCount CampaignLogKey Int
  | RecordSetInsert CampaignLogKey [SomeRecorded]
  | CrossOutRecordSetEntries CampaignLogKey [SomeRecorded]
  | RefillSlots InvestigatorId SlotType [AssetId]
  | Remember ScenarioLogKey
  | ScenarioCountIncrementBy ScenarioCountKey Int
  | ScenarioCountDecrementBy ScenarioCountKey Int
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemovePlayerCardFromGame Bool Card
  | RemoveAllClues Target
  | RemoveAllDoomFromPlay RemoveDoomMatchers
  | RemoveAllDoom Target
  | RemoveCampaignCard CardDef
  | RemoveCampaignCardFromDeck InvestigatorId CardDef
  | RemoveCardFromHand InvestigatorId CardId
  | RemoveCardFromSearch InvestigatorId CardId
  | RemoveClues Target Int
  | RemoveDiscardFromGame InvestigatorId
  | RemoveDoom Target Int
  | RemoveAsset AssetId
  | RemoveEnemy EnemyId
  | RemoveEvent EventId
  | RemoveSkill SkillId
  | RemoveTreachery TreacheryId
  | RemoveFromDiscard InvestigatorId CardId
  | RemoveFromEncounterDiscard EncounterCard
  | RemoveFromGame Target
  | RemoveCompletedActFromGame Int ActId
  | RemovedFromGame Card
  | RemoveLocation LocationId
  | RemovedLocation LocationId
  | RemoveTraits Target [Trait]
  | SetCardAside Card
  | SetOutOfPlay Target
  | DoSetOutOfPlay Target
  | RemoveFromPlay Source
  | RemovedFromPlay Source
  | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
  | RequestSetAsideCard Source CardCode
  | RequestTokens Source (Maybe InvestigatorId) RevealStrategy RequestedTokenStrategy
  | RequestedEncounterCard Source (Maybe InvestigatorId) (Maybe EncounterCard)
  | RequestedEncounterCards Target [EncounterCard]
  | RequestedPlayerCard InvestigatorId Source (Maybe PlayerCard)
  | RequestedSetAsideCard Source Card
  | RequestedTokens Source (Maybe InvestigatorId) [Token]
  | RerunSkillTest
  | ResetInvestigators
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
  | RecalculateSkillTestResults
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
  | SkillTestApplyResults
  | SkillTestApplyResultsAfter
  | SkillTestAsk Message
  | SkillTestCommitCard InvestigatorId Card
  | SkillTestEnds InvestigatorId Source
  | AfterSkillTestEnds Source Target SkillTest.SkillTestResult
  | EndSkillTestWindow
  | SkillTestResults SkillTestResultsData
  | SkillTestUncommitCard InvestigatorId Card
  | SpawnEnemyAt Card LocationId
  | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
  | SpendClues Int [InvestigatorId]
  | SpendResources InvestigatorId Int
  | SpendUses Target UseType Int
  | StartCampaign
  | StartScenario ScenarioId
  | RestartScenario
  | StartSkillTest InvestigatorId
  | -- There are two targets, one associated to the action and one
    -- to handle the result
    Successful (Action, Target) InvestigatorId Source Target Int
  | SufferTrauma InvestigatorId Int Int
  | CheckTrauma InvestigatorId
  | HealTrauma InvestigatorId Int Int
  | GainSurge Source Target
  | Surge InvestigatorId Source
  | TakeAction InvestigatorId (Maybe Action) Cost
  | TakeControlOfAsset InvestigatorId AssetId
  | ReplaceInvestigatorAsset InvestigatorId Card
  | ReplacedInvestigatorAsset InvestigatorId AssetId
  | TakeControlOfSetAsideAsset InvestigatorId Card
  | TakeResources InvestigatorId Int Source Bool
  | DrawStartingHand InvestigatorId
  | TakeStartingResources InvestigatorId
  | TakenAction InvestigatorId Action
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
  | UseCardAbility InvestigatorId Source Int [Window] Payment
  | UseCardAbilityStep InvestigatorId Source Int [Window] Payment Int
  | UseCardAbilityChoice InvestigatorId Source Int AbilityMetadata [Window] Payment
  | UseCardAbilityChoiceTarget InvestigatorId Source Int Target [Window] Payment
  | HandleTargetChoice InvestigatorId Source Target
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
  | UpdateHistory InvestigatorId History
  | -- The Forgotten Age
    PickSupply InvestigatorId Supply
  | UseSupply InvestigatorId Supply
  | Explore InvestigatorId Source CardMatcher
  | BecomeYithian InvestigatorId
  | SetScenarioMeta Value
  | -- The Circle Undone
    BecomePrologueInvestigator InvestigatorId InvestigatorId
  | PutLocationInFrontOf InvestigatorId LocationId
  | If WindowType [Message]
  | -- Commit
    Do Message
  deriving stock (Show, Eq)

$(deriveJSON defaultOptions ''Message)

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
  SkillLabel _ msgs -> Run msgs
  EvadeLabel _ msgs -> Run msgs
  FightLabel _ msgs -> Run msgs
  AbilityLabel iid ab windows msgs -> Run $ UseAbility iid ab windows : msgs
  ComponentLabel _ msgs -> Run msgs
  EndTurnButton _ msgs -> Run msgs
  StartSkillTestButton iid -> Run [StartSkillTest iid]
  SkillTestApplyResultsButton -> Run [SkillTestApplyResults]
  TokenGroupChoice source iid step -> Run [ChooseTokenGroups source iid step]
  EffectActionButton _ _ msgs -> Run msgs
  Done _ -> Run []

chooseOrRunOne :: InvestigatorId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne iid msgs = chooseOne iid msgs

questionLabel :: Text -> InvestigatorId -> Question Message -> Message
questionLabel lbl iid q = Ask iid (QuestionLabel lbl q)

chooseOne :: InvestigatorId -> [UI Message] -> Message
chooseOne _ [] = throw $ InvalidState "No messages for chooseOne"
chooseOne iid msgs = Ask iid (ChooseOne msgs)

chooseOneDropDown :: InvestigatorId -> [(Text, Message)] -> Message
chooseOneDropDown _ [] = throw $ InvalidState "No messages for chooseOne"
chooseOneDropDown iid msgs = Ask iid (DropDown msgs)

chooseOneAtATime :: InvestigatorId -> [UI Message] -> Message
chooseOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOneAtATime iid msgs = Ask iid (ChooseOneAtATime msgs)

chooseOrRunOneAtATime :: InvestigatorId -> [UI Message] -> Message
chooseOrRunOneAtATime _ [] = throw $ InvalidState "No messages for chooseOneAtATime"
chooseOrRunOneAtATime _ [x] = uiToRun x
chooseOrRunOneAtATime iid msgs = Ask iid (ChooseOneAtATime msgs)

chooseSome :: InvestigatorId -> Text -> [UI Message] -> Message
chooseSome _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseSome iid doneText msgs = Ask iid (ChooseSome $ Done doneText : msgs)

chooseUpToN :: InvestigatorId -> Int -> Text -> [UI Message] -> Message
chooseUpToN _ _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseUpToN iid n doneText msgs =
  Ask iid (ChooseUpToN n $ Label doneText [] : msgs)

chooseN :: InvestigatorId -> Int -> [UI Message] -> Message
chooseN _ _ [] = throw $ InvalidState "No messages for chooseN"
chooseN iid n msgs = Ask iid (ChooseN n msgs)

chooseOrRunN :: InvestigatorId -> Int -> [UI Message] -> Message
chooseOrRunN _ _ [] = throw $ InvalidState "No messages for chooseN"
chooseOrRunN _ n msgs | length msgs == n = Run $ map uiToRun msgs
chooseOrRunN iid n msgs = Ask iid (ChooseN n msgs)

chooseAmounts
  :: Targetable target => InvestigatorId -> Text -> AmountTarget -> [(Text, (Int, Int))] -> target -> Message
chooseAmounts iid label total choiceMap (toTarget -> target) = Ask
  iid
  (ChooseAmounts label total amountChoices target)
 where
  amountChoices = map toAmountChoice choiceMap
  toAmountChoice (l, (m, n)) = AmountChoice l m n

chooseUpgradeDeck :: InvestigatorId -> Message
chooseUpgradeDeck iid = Ask iid ChooseUpgradeDeck

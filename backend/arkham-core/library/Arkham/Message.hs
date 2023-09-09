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
import Arkham.Attack
import Arkham.Campaign.Option
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.CampaignStep
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.ChaosBag.RevealStrategy
import Arkham.ChaosBagStepState
import Arkham.ChaosToken
import Arkham.ClassSymbol
import Arkham.Cost
import Arkham.DamageEffect
import Arkham.Deck
import Arkham.Direction
import Arkham.Discard
import Arkham.Draw.Types
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterCard.Source
import Arkham.Enemy.Creation
import Arkham.Exception
import Arkham.Field
import Arkham.Helpers
import Arkham.History
import Arkham.Id
import Arkham.Key
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
import Arkham.SkillTest.Base
import Arkham.SkillTest.Type
import Arkham.SkillTestResult qualified as SkillTest
import Arkham.SkillType
import Arkham.Slot
import Arkham.Source
import Arkham.Target
import Arkham.Token
import Arkham.Token qualified as Token
import Arkham.Trait
import Arkham.Window (Window, WindowType)
import Control.Exception
import Data.Aeson.TH

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack {} = Just AttackMessage
messageType Revelation {} = Just RevelationMessage
messageType DrawChaosToken {} = Just DrawChaosTokenMessage
messageType ResolveChaosToken {} = Just ResolveChaosTokenMessage
messageType EnemySpawn {} = Just EnemySpawnMessage
messageType InvestigatorDrawEnemy {} = Just DrawEnemyMessage
messageType EnemyDefeated {} = Just EnemyDefeatedMessage
messageType (Discard GameSource (EnemyTarget _)) = Just EnemyDefeatedMessage
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

story :: [InvestigatorId] -> FlavorText -> Message
story iids flavor =
  AskMap
    (mapFromList [(iid, Read flavor [Label "Continue" []]) | iid <- iids])

storyWithChooseOne :: InvestigatorId -> [InvestigatorId] -> FlavorText -> [UI Message] -> Message
storyWithChooseOne lead iids flavor choices =
  AskMap
    (mapFromList [(iid, Read flavor $ if iid == lead then choices else []) | iid <- iids])

data AdvancementMethod = AdvancedWithClues | AdvancedWithOther
  deriving stock (Generic, Eq, Show)
  deriving anyclass (FromJSON, ToJSON)

pattern PlaceClues :: Source -> Target -> Int -> Message
pattern PlaceClues source target n = PlaceTokens source target Clue n

pattern PlaceDoom :: Source -> Target -> Int -> Message
pattern PlaceDoom source target n = PlaceTokens source target Doom n

pattern PlaceResources :: Source -> Target -> Int -> Message
pattern PlaceResources source target n = PlaceTokens source target Token.Resource n

pattern PlaceDamage :: Source -> Target -> Int -> Message
pattern PlaceDamage source target n = PlaceTokens source target Token.Damage n

pattern PlaceHorror :: Source -> Target -> Int -> Message
pattern PlaceHorror source target n = PlaceTokens source target Horror n

pattern RemoveClues :: Source -> Target -> Int -> Message
pattern RemoveClues source target n = RemoveTokens source target Clue n

pattern RemoveDoom :: Source -> Target -> Int -> Message
pattern RemoveDoom source target n = RemoveTokens source target Doom n

pattern RemoveResources :: Source -> Target -> Int -> Message
pattern RemoveResources source target n = RemoveTokens source target Token.Resource n

pattern CancelNext :: Source -> MessageType -> Message
pattern CancelNext source msgType = CancelEachNext source [msgType]

pattern AttachTreachery :: TreacheryId -> Target -> Message
pattern AttachTreachery tid target =
  PlaceTreachery tid (TreacheryAttachedTo target)

createCardEffect
  :: (Sourceable source, Targetable target)
  => CardDef
  -> (Maybe (EffectMetadata Window Message))
  -> source
  -> target
  -> Message
createCardEffect def mMeta (toSource -> source) (toTarget -> target) = CreateEffect (toCardCode def) mMeta source target

data AbilityRef = AbilityRef Source Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getChoiceAmount :: Text -> [(Text, Int)] -> Int
getChoiceAmount key choices =
  let choicesMap = mapFromList @(Map Text Int) choices
   in findWithDefault 0 key choicesMap

class IsMessage msg where
  toMessage :: msg -> Message

instance IsMessage Message where
  toMessage = id
  {-# INLINE toMessage #-}

instance IsMessage EnemyAttackDetails where
  toMessage = InitiateEnemyAttack
  {-# INLINE toMessage #-}

instance IsMessage (HandDiscard Message) where
  toMessage = DiscardFromHand
  {-# INLINE toMessage #-}

instance IsMessage (EnemyCreation Message) where
  toMessage = CreateEnemy
  {-# INLINE toMessage #-}

data ReplaceStrategy = DefaultReplace | Swap
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data StoryMode = ResolveIt | DoNotResolveIt
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Message
  = UseAbility InvestigatorId Ability [Window]
  | ResolvedAbility Ability -- INTERNAL, Set Arbiter of Fates
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
  | AddAgenda Int Card
  | SetCurrentAgendaDeck Int [Card]
  | AdvanceAgenda AgendaId
  | AdvanceToAgenda Int CardDef AgendaSide Source
  | NextAdvanceAgendaStep AgendaId Int
  | AdvanceAgendaIfThresholdSatisfied
  | AdvanceAgendaDeck Int Source
  | AdvanceCurrentAgenda
  | ReplaceLocation LocationId Card ReplaceStrategy
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
    AddChaosToken ChaosTokenFace
  | RemoveAllChaosTokens ChaosTokenFace
  | -- Asset Uses
    AddUses AssetId UseType Int
  | -- Asks
    AskPlayer Message
  | Ask InvestigatorId (Question Message)
  | AskMap (Map InvestigatorId (Question Message))
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
  | BeforeRevealChaosTokens
  | BeforeSkillTest SkillTest
  | ChangeSkillTestType SkillTestType SkillTestBaseValue
  | -- Game State Control
    BeginGame
  | Begin Phase
  | BeginRound
  | BeginSkillTest SkillTest
  | BeginSkillTestAfterFast SkillTest
  | SetSkillTestTarget Target
  | BeginTrade InvestigatorId Source Target [InvestigatorId]
  | BeginTurn InvestigatorId
  | Blanked Message
  | HandleOption CampaignOption
  | CampaignStep CampaignStep
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
  | DiscardFromHand (HandDiscard Message)
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
  | ChooseRandomLocation Target (Set LocationId)
  | ChosenRandomLocation Target LocationId
  | ChooseChaosTokenGroups Source InvestigatorId ChaosBagStep
  | CommitCard InvestigatorId Card
  | CommitToSkillTest SkillTest (UI Message)
  | Continue Text
  | CreateEffect CardCode (Maybe (EffectMetadata Window Message)) Source Target
  | ObtainCard Card
  | CreateEnemy (EnemyCreation Message)
  | CreatedEnemyAt EnemyId LocationId Target
  | -- new payment bs
    PayForAbility Ability [Window]
  | CreatedCost ActiveCostId
  | PayCost ActiveCostId InvestigatorId Bool Cost
  | PayCostFinished ActiveCostId
  | PaidCost ActiveCostId InvestigatorId (Maybe Action) Payment
  | -- end  new payment bs
    CreateWindowModifierEffect EffectWindow (EffectMetadata Window Message) Source Target
  | CreateChaosTokenEffect (EffectMetadata Window Message) Source ChaosToken
  | CreateAssetAt AssetId Card Placement
  | CreateEventAt InvestigatorId Card Placement
  | PlaceAsset AssetId Placement
  | PlaceEvent InvestigatorId EventId Placement
  | PlaceTreachery TreacheryId TreacheryPlacement
  | PlaceKey Target ArkhamKey
  | CreateStoryAssetAtLocationMatching Card LocationMatcher
  | CreateChaosTokenValueEffect Int Source Target
  | CreateWeaknessInThreatArea Card InvestigatorId
  | CreatedEffect EffectId (Maybe (EffectMetadata Window Message)) Source Target
  | CrossOutRecord CampaignLogKey
  | SetCampaignLog CampaignLog
  | Damage Target Source Int
  | DeckHasNoCards InvestigatorId (Maybe Target)
  | DisableEffect EffectId
  | Discard Source Target
  | DiscardHand InvestigatorId Source
  | RevealUntilFirst InvestigatorId Source DeckSignifier CardMatcher
  | RevealedCards InvestigatorId Source DeckSignifier (Maybe Card) [Card]
  | DiscardUntilFirst InvestigatorId Source DeckSignifier ExtendedCardMatcher
  | DiscardTopOfDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeck InvestigatorId Int Source (Maybe Target)
  | DiscardTopOfEncounterDeckWithDiscardedCards InvestigatorId Int Source (Maybe Target) [EncounterCard]
  | Discarded Target Source Card
  | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Source Target
  | DiscardedTopOfDeck InvestigatorId [PlayerCard] Source Target
  | DiscoverClues InvestigatorId LocationId Source Int (Maybe Action)
  | DiscoverCluesAtLocation InvestigatorId LocationId Source Int (Maybe Action)
  | DisengageEnemy InvestigatorId EnemyId
  | DisengageEnemyFromAll EnemyId
  | DrawAnotherChaosToken InvestigatorId
  | DrawCards CardDraw -- use drawCards
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
  | DrawChaosToken InvestigatorId ChaosToken
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
  | EnemyAttackFromDiscard InvestigatorId Source Card
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | EnemyAttacks [Message]
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
  | FocusChaosTokens [ChaosToken]
  | Force Message
  | FoundAndDrewEncounterCard InvestigatorId EncounterCardSource EncounterCard
  | FoundEncounterCard InvestigatorId Target EncounterCard
  | FoundEncounterCardFrom InvestigatorId Target EncounterCardSource EncounterCard
  | FoundEnemyInVoid InvestigatorId Target EnemyId
  | GainActions InvestigatorId Source Int
  | GainAdditionalAction InvestigatorId Source AdditionalAction
  | LoseAdditionalAction InvestigatorId Source AdditionalAction
  | UseEffectAction InvestigatorId EffectId [Window]
  | GainClues InvestigatorId Source Int
  | GainXP InvestigatorId Source Int
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
  | InOutOfPlay Message
  | InitDeck InvestigatorId (Deck PlayerCard) -- used to initialize the deck for the campaign
  | UpgradeDeck InvestigatorId (Deck PlayerCard) -- used to upgrade deck during campaign
  | FinishedUpgradingDecks
  | Flip InvestigatorId Source Target
  | Flipped Source Card
  | InitiatePlayCardAsChoose InvestigatorId Card [Card] [Message] ChosenCardStrategy [Window] Bool
  | InitiatePlayCardAs InvestigatorId Card Card [Message] ChosenCardStrategy [Window] Bool
  | InitiatePlayCard InvestigatorId Card (Maybe Target) [Window] Bool
  | -- | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
    CheckAdditionalActionCosts InvestigatorId Target Action [Message]
  | CheckAllAdditionalCommitCosts
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
  | InvestigatorDiscardAllClues Source InvestigatorId
  | InvestigatorDiscoverClues InvestigatorId LocationId Source Int (Maybe Action)
  | InvestigatorDiscoverCluesAtTheirLocation InvestigatorId Source Int (Maybe Action)
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
    InvestigatorPlaceAllCluesOnLocation InvestigatorId Source
  | InvestigatorPlaceCluesOnLocation InvestigatorId Source Int
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
  | MoveAllCluesTo Source Target
  | MoveTopOfDeckToBottom Source DeckSignifier Int
  | NextCampaignStep (Maybe CampaignStep)
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | Noop
  | PassSkillTest
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillTestType Int
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId Card [Window]
  | PaidForCardCost InvestigatorId Card Payment
  | PayForCardAbility InvestigatorId Source [Window] Int Payment
  | PlaceCluesUpToClueValue LocationId Source Int
  | FlipClues Target Int
  | FlipDoom Target Int
  | PlaceAdditionalDamage Target Source Int Int
  | PlaceDoomOnAgenda
  | PlaceEnemyInVoid EnemyId
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Card
  | PlaceLocationMatching CardMatcher
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
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
  | RemoveFromGame Target
  | RemoveCompletedActFromGame Int ActId
  | RemovedFromGame Card
  | RemoveLocation LocationId
  | RemovedLocation LocationId
  | RemoveTraits Target [Trait]
  | SetCardAside Card
  | SetOutOfPlay OutOfPlayZone Target
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
  | Resign InvestigatorId
  | ResignWith Target
  | ResolveAmounts InvestigatorId [(Text, Int)] Target
  | ResolveEvent InvestigatorId EventId (Maybe Target) [Window]
  | ResolveEventChoice InvestigatorId EventId Int (Maybe Target) [Window]
  | ResolveSkill SkillId
  | ResolveChaosToken ChaosToken ChaosTokenFace InvestigatorId -- since tokens can have their face changed we use this to represent that; TODO: use a real modifier
  | ReturnSkillTestRevealedChaosTokens
  | ReturnChaosTokens [ChaosToken]
  | RevealInHand CardId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | UnrevealLocation LocationId
  | RevealSkillTestChaosTokens InvestigatorId
  | RevealChaosToken Source InvestigatorId ChaosToken
  | Revelation InvestigatorId Source
  | RevelationChoice InvestigatorId Source Int
  | RevelationSkillTest InvestigatorId Source SkillType Int
  | Run [Message]
  | RunBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedChaosTokenStrategy
  | RunSkillTest InvestigatorId
  | RecalculateSkillTestResults
  | RemoveFromBearersDeckOrDiscard PlayerCard
  | SearchCollectionForRandom InvestigatorId Source CardMatcher
  | Search InvestigatorId Source Target [(Zone, ZoneReturnStrategy)] CardMatcher FoundCardsStrategy
  | SearchFound InvestigatorId Target DeckSignifier [Card]
  | FoundCards (Map Zone [Card])
  | SearchNoneFound InvestigatorId Target
  | SetActions InvestigatorId Source Int
  | SetEncounterDeck (Deck EncounterCard)
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
  | CancelSurge Source
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
  | UnfocusChaosTokens
  | SealChaosToken ChaosToken
  | SealedChaosToken ChaosToken Card
  | UnsealChaosToken ChaosToken
  | ChaosTokenIgnored InvestigatorId Source ChaosToken
  | ChaosTokenCanceled InvestigatorId Source ChaosToken
  | UnsetActiveCard
  | AddCardEntity Card
  | RemoveCardEntity Card
  | UseCardAbility InvestigatorId Source Int [Window] Payment
  | UseCardAbilityStep InvestigatorId Source Int [Window] Payment Int
  | UseCardAbilityChoice InvestigatorId Source Int AbilityMetadata [Window] Payment
  | UseCardAbilityChoiceTarget InvestigatorId Source Int Target [Window] Payment
  | HandleTargetChoice InvestigatorId Source Target
  | ResetMetadata Target
  | DoNotCountUseTowardsAbilityLimit InvestigatorId Ability
  | When Message
  | Would BatchId [Message]
  | IgnoreBatch BatchId
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
  | ReplaceCard CardId Card
  | UpdateHistory InvestigatorId History
  | -- The Forgotten Age
    PickSupply InvestigatorId Supply
  | UseSupply InvestigatorId Supply
  | Explore InvestigatorId Source CardMatcher
  | BecomeYithian InvestigatorId
  | SetScenarioMeta Value
  | DoStep Int Message
  | -- The Circle Undone
    BecomePrologueInvestigator InvestigatorId InvestigatorId
  | PutLocationInFrontOf InvestigatorId LocationId
  | PutLocationInCenter LocationId
  | PlaceBreaches Target Int
  | RemoveBreaches Target Int
  | RunCosmos InvestigatorId LocationId [Message]
  | PlaceCosmos InvestigatorId LocationId (CosmosLocation Card LocationId)
  | Incursion LocationId
  | UpdateLocation LocationId (Update Location)
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
  GridLabel _ msgs -> Run msgs
  SkillLabel _ msgs -> Run msgs
  EvadeLabel _ msgs -> Run msgs
  FightLabel _ msgs -> Run msgs
  AbilityLabel iid ab windows msgs -> Run $ UseAbility iid ab windows : msgs
  ComponentLabel _ msgs -> Run msgs
  EndTurnButton _ msgs -> Run msgs
  StartSkillTestButton iid -> Run [StartSkillTest iid]
  SkillTestApplyResultsButton -> Run [SkillTestApplyResults]
  ChaosTokenGroupChoice source iid step -> Run [ChooseChaosTokenGroups source iid step]
  EffectActionButton _ _ msgs -> Run msgs
  Done _ -> Run []

chooseOrRunOne :: InvestigatorId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne iid msgs = chooseOne iid msgs

questionLabel :: Text -> InvestigatorId -> Question Message -> Message
questionLabel lbl iid q = Ask iid (QuestionLabel lbl Nothing q)

questionLabelWithCard :: Text -> CardCode -> InvestigatorId -> Question Message -> Message
questionLabelWithCard lbl cCode iid q = Ask iid (QuestionLabel lbl (Just cCode) q)

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

chooseSome1 :: InvestigatorId -> Text -> [UI Message] -> Message
chooseSome1 _ _ [] = throw $ InvalidState "No messages for chooseSome"
chooseSome1 iid doneText msgs = Ask iid (ChooseSome1 doneText msgs)

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
  :: Targetable target
  => InvestigatorId
  -> Text
  -> AmountTarget
  -> [(Text, (Int, Int))]
  -> target
  -> Message
chooseAmounts iid label total choiceMap (toTarget -> target) =
  Ask
    iid
    (ChooseAmounts label total amountChoices target)
 where
  amountChoices = map toAmountChoice choiceMap
  toAmountChoice (l, (m, n)) = AmountChoice l m n

chooseUpgradeDeck :: InvestigatorId -> Message
chooseUpgradeDeck iid = Ask iid ChooseUpgradeDeck

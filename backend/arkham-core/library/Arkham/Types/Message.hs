module Arkham.Types.Message
  ( module Arkham.Types.Message
  ) where

import Arkham.Prelude

import Arkham.Types.Ability
import Arkham.Types.Action
import Arkham.Types.Asset.Uses
import Arkham.Types.CampaignLogKey
import Arkham.Types.CampaignStep
import Arkham.Types.Card
import Arkham.Types.Card.Id
import Arkham.Types.ChaosBagStepState
import Arkham.Types.ClassSymbol
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.Exception
import Arkham.Types.Helpers
import Arkham.Types.Id
import Arkham.Types.LocationSymbol
import Arkham.Types.Matcher
import Arkham.Types.Name
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Resolution
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.Window
import Control.Exception

data MessageType
    = RevelationMessage
    | AttackMessage
    | DrawTokenMessage
    | RevealTokenMessage
    | ResolveTokenMessage
    | RunWindowMessage
    | EnemySpawnMessage
    | DamageMessage
    | DrawEncounterCardMessage
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType RevealToken{} = Just RevealTokenMessage
messageType InvestigatorDamage{} = Just DamageMessage
messageType InvestigatorDoAssignDamage{} = Just DamageMessage
messageType InvestigatorDrewEncounterCard{} = Just DrawEncounterCardMessage
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

data DamageStrategy = DamageAny | DamageAssetsFirst | DamageFirst CardDef
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data EncounterCardSource = FromDiscard | FromEncounterDeck | FromTheVoid
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data SearchedCardsStrategy = ShuffleBackIn FoundCardStrategy | PutBackInAnyOrder | DeferSearchedToTarget Target
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

data FoundCardStrategy = DrawFound InvestigatorId | NotifyTargetOfFound Target
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
    deriving stock (Bounded, Enum, Show)

data DeckSignifier = InvestigatorDeck InvestigatorId | EncounterDeck
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- TODO: Better handle in play and out of play
-- Out of play refers to player's hand, in any deck,
-- in any discard pile, the victory display, and
-- cards that have been set aside

data Message
    = UseAbility InvestigatorId Ability
    | AddAct ActId
    | AddAgenda AgendaId
    | AddCampaignCardToDeck InvestigatorId CardDef
    | AddCampaignCardToEncounterDeck CardDef
    | AddConnection LocationId LocationSymbol
    | AddDirectConnection LocationId LocationId
    | AddFocusedToHand InvestigatorId Target CardId
    | AddFocusedToTopOfDeck InvestigatorId Target CardId
    | AddSkillTestSubscriber Target
    | AddSlot InvestigatorId SlotType Slot
    | AddToDiscard InvestigatorId PlayerCard
    | AddToEncounterDeck EncounterCard
    | AddToHand InvestigatorId Card
    | AddToHandFromDeck InvestigatorId CardId
    | AddToScenarioDeck Target
    | AddCardToScenarioDeck Card
    | AddToVictory Target
    | AddToken TokenFace
    | AddTraits Target [Trait]
    | AddUses Target UseType Int
    | AddedConnection LocationId LocationId
    | AdvanceAct ActId Source
    | AdvanceAgenda AgendaId
    | AdvanceAgendaIfThresholdSatisfied
    | AdvanceCurrentAgenda
    | After Message
    | AfterAttackEnemy InvestigatorId EnemyId
    | AfterDiscoverClues InvestigatorId LocationId Int
    | AfterEnterLocation InvestigatorId LocationId
    | AfterEvadeEnemy InvestigatorId EnemyId
    | AfterRevelation InvestigatorId TreacheryId
    | AllCheckHandSize
    | AllDrawCardAndResource
    | AllDrawEncounterCard
    | AllInvestigatorsResigned
    | AllRandomDiscard
    | AskPlayer Message
    | Ask InvestigatorId Question
    | AskMap (Map InvestigatorId Question)
    | AssetDamage AssetId Source Int Int
    | AssetDefeated AssetId
    | AttachAsset AssetId Target
    | AttachEvent EventId Target
    | AttachStoryTreacheryTo Card Target
    | AttachTreachery TreacheryId Target
    | AttackEnemy InvestigatorId EnemyId Source SkillType
    | BeforeRevealTokens
    | BeforeSkillTest InvestigatorId SkillType Int
    | BeginEnemy
    | BeginInvestigation
    | BeginMythos
    | BeginRound
    | BeginSkillTest InvestigatorId Source Target (Maybe Action) SkillType Int
    | BeginSkillTestAfterFast InvestigatorId Source Target (Maybe Action) SkillType Int
    | BeginTrade InvestigatorId Target [InvestigatorId]
    | BeginTurn InvestigatorId
    | BeginUpkeep
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
    | CheckWindow InvestigatorId [Window]
    | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
    | RunWindow InvestigatorId [Window]
    | ChooseAndDiscardAsset InvestigatorId AssetMatcher
    | ChooseAndDiscardCard InvestigatorId
    | ChooseEndTurn InvestigatorId
    | ChooseInvestigate InvestigatorId Source Bool
    | ChooseEvadeEnemy InvestigatorId Source SkillType Bool
    | ChooseFightEnemy InvestigatorId Source SkillType (Set Trait) Bool
    | ChooseFightEnemyNotEngagedWithInvestigator InvestigatorId Source SkillType Bool
    | ChooseLeadInvestigator
    | ChoosePlayer InvestigatorId ChoosePlayerChoice
    | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
    | ChooseRandomLocation Target (Set LocationId)
    | ChosenRandomLocation Target LocationId
    | ChooseTokenGroups Source InvestigatorId ChaosBagStep
    | CommitCard InvestigatorId CardId
    | Continue Text
    | CreateEffect CardCode (Maybe (EffectMetadata Message)) Source Target
    | CreateEnemy Card
    | CreateEnemyAt Card LocationId (Maybe Target)
    | CreatedEnemyAt EnemyId LocationId Target
    | CreateEnemyAtLocationMatching Card LocationMatcher
    | CreateEnemyEngagedWithPrey Card
    | CreatePayAbilityCostEffect Ability Source Target
    | CreateWindowModifierEffect EffectWindow (EffectMetadata Message) Source Target
    | CreateTokenEffect (EffectMetadata Message) Source Token
    | CreateStoryAssetAt Card LocationId
    | CreateStoryAssetAtLocationMatching Card LocationMatcher
    | CreateTokenValueEffect Int Source Target
    | CreateWeaknessInThreatArea Card InvestigatorId
    | CreatedEffect EffectId (Maybe (EffectMetadata Message)) Source Target
    | CrossOutRecord CampaignLogKey
    | Damage Target Source Int
    | DeckHasNoCards InvestigatorId (Maybe Target)
    | DisableEffect EffectId
    | Discard Target
    | DiscardCard InvestigatorId CardId
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
    | Done Text
    | DrawAnotherToken InvestigatorId
    | DrawCards InvestigatorId Int Bool
    | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
    | DrawToken InvestigatorId Token
    | DrewPlayerEnemy InvestigatorId Card
    | DrewTreachery InvestigatorId Card
    | DrivenInsane InvestigatorId
    | EmptyDeck InvestigatorId
    | EndPhase
    | EndCheckWindow
    | EndEnemy
    | EndInvestigation
    | EndMythos
    | EndOfGame
    | Exile Target
    | Exiled Target Card
    | ScenarioResolution Resolution
    | EndOfScenario
    | EndRound
    | EndRoundWindow
    | EndSearch InvestigatorId Source
    | EndTurn InvestigatorId
    | EndUpkeep
    | EnemiesAttack
    | EnemyAttack InvestigatorId EnemyId DamageStrategy
    | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
    | EnemyAttacks [Message]
    | EnemyCheckEngagement EnemyId
    | EnemyDamage EnemyId InvestigatorId Source Int
    | EnemySetDamage EnemyId Source Int
    | DefeatEnemy EnemyId InvestigatorId Source
    | EnemyDefeated EnemyId InvestigatorId LocationId CardCode Source [Trait]
    | EnemyEngageInvestigator EnemyId InvestigatorId
    | EnemyEvaded InvestigatorId EnemyId
    | EnemyMove EnemyId LocationId LocationId
    | EngagedEnemyMove EnemyId LocationId LocationId
    | EnemyEntered EnemyId LocationId
    | EnemySetBearer EnemyId BearerId
    | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
    | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
    | EnemySpawnEngagedWithPrey EnemyId
    | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
    | EnemySpawnedAt LocationId EnemyId
    | EnemyWillAttack InvestigatorId EnemyId DamageStrategy
    | EngageEnemy InvestigatorId EnemyId Bool
    | EvadeEnemy InvestigatorId EnemyId Source SkillType Bool
    | Exhaust Target
    | FailSkillTest
    | FailedAttackEnemy InvestigatorId EnemyId
    | FailedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
    | FightEnemy InvestigatorId EnemyId Source SkillType Bool
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
    | HuntersMove
    | HunterMove EnemyId
    | InDiscard InvestigatorId Message
    | InHand InvestigatorId Message
    | InitDeck InvestigatorId (Deck PlayerCard) -- used to initialize the deck for the campaign
    | UpgradeDeck InvestigatorId (Deck PlayerCard) -- used to upgrade deck during campaign
    | FinishedUpgradingDecks
    | Flip Source Target
    | InitiatePlayCardAsChoose InvestigatorId CardId [Card] [Message] Bool
    | InitiatePlayCardAs InvestigatorId CardId Card [Message] Bool
    | InitiatePlayCard InvestigatorId CardId (Maybe Target) Bool
    | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
    | InitiatePlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
    | CheckAdditionalActionCosts InvestigatorId Target Source Action [Message]
    | Investigate InvestigatorId LocationId Source SkillType Bool
    | -- | uses the internal method and then checks defeat
      InvestigatorAssignDamage InvestigatorId Source DamageStrategy Int Int
    | InvestigatorCommittedCard InvestigatorId CardId
    | InvestigatorCommittedSkill InvestigatorId SkillId
    | InvestigatorDamage InvestigatorId Source Int Int
    | InvestigatorDamageEnemy InvestigatorId EnemyId
    | InvestigatorDamageInvestigator InvestigatorId InvestigatorId
    | InvestigatorDefeated InvestigatorId
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
    | InvestigatorKilled InvestigatorId
    | InvestigatorMulligan InvestigatorId
    | InvestigatorsMulligan
    | -- | This message exists in case the number of clues will change
      InvestigatorPlaceAllCluesOnLocation InvestigatorId
    | InvestigatorPlaceCluesOnLocation InvestigatorId Int
    | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
    | InvestigatorPlayedAsset InvestigatorId AssetId [SlotType] [Trait]
    | InvestigatorPlayDynamicAsset InvestigatorId AssetId [SlotType] [Trait] Int
    | InvestigatorPlayDynamicEvent InvestigatorId EventId Int
    | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target) [Window]
    | InvestigatorResigned InvestigatorId
    | InvestigatorSpendClues InvestigatorId Int
    | InvestigatorTakeDamage InvestigatorId Source Int Int
    | InvestigatorWhenDefeated Source InvestigatorId
    | InvestigatorWhenEliminated Source InvestigatorId
    | Label Text [Message]
    | CardLabel CardCode [Message]
    | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
    | LookAtRevealed Source Target
    | LookAtTopOfDeck InvestigatorId Target Int
    | LoseActions InvestigatorId Source Int
    | LoseResources InvestigatorId Int
    | LoseAllResources InvestigatorId
    | SpendActions InvestigatorId Source Int
    | Move InvestigatorId LocationId LocationId
    | MoveAction InvestigatorId LocationId Cost Bool
    | MoveAllCluesTo Target
    | MoveAllTo LocationId
    | MoveFrom InvestigatorId LocationId
    | MoveTo InvestigatorId LocationId
    | MoveToward Target LocationMatcher
    | MoveTopOfDeckToBottom Source DeckSignifier Int
    | MoveUntil LocationId Target
    | MovedBy InvestigatorId Source
    | NextAct ActId ActId
    | NextAdvanceActStep ActId Int
    | NextAgenda AgendaId AgendaId
    | NextCampaignStep (Maybe CampaignStep)
    | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
    | PassSkillTest
    | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
    | PayAbilityCost Source InvestigatorId (Maybe Action) Cost
    | PayAbilityCostFinished EffectId Source InvestigatorId
    | PaidAbilityCost InvestigatorId (Maybe Action) Payment
    | PayCardCost InvestigatorId CardId
    | PayDynamicCardCost InvestigatorId CardId Int [Message]
    | PayForCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int
    | PayedForDynamicCard InvestigatorId CardId Int Bool
    | PerformEnemyAttack InvestigatorId EnemyId DamageStrategy
    | PlaceClues Target Int
    | PlaceDoom Target Int
    | PlaceDoomOnAgenda
    | PlaceEnemyInVoid EnemyId
    | PlaceLocation LocationId CardDef
    | PlaceLocationMatching LocationMatcher
    | PlaceResources Target Int
    | PlaceUnderneath Target [Card]
    | PlacedLocation Name CardCode LocationId
    | PlacedLocationDirection LocationId Direction LocationId
    | PlayCard InvestigatorId CardId (Maybe Target) Bool
    | PlayFastEvent InvestigatorId CardId (Maybe Target) [Window]
    | PlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
    | PlayedCard InvestigatorId Card
    | PlayerWindow InvestigatorId [Message] Bool
    | PutCardIntoPlay InvestigatorId Card (Maybe Target)
    | PutOnTopOfDeck InvestigatorId PlayerCard
    | PutOnTopOfEncounterDeck InvestigatorId EncounterCard
    | PutSetAsideIntoPlay Target
    | RandomDiscard InvestigatorId
    | Ready Target
    | ReadyAlternative Source Target
    | ReadyExhausted
    | Record CampaignLogKey
    | RecordCount CampaignLogKey Int
    | RecordSet CampaignLogKey [CardCode]
    | RefillSlots InvestigatorId SlotType [AssetId]
    | Remember ScenarioLogKey
    | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
    | RemoveAllClues Target
    | RemoveAllDoom
    | RemoveCampaignCardFromDeck InvestigatorId CardCode
    | RemoveCardFromHand InvestigatorId CardId
    | RemoveClues Target Int
    | RemoveDiscardFromGame InvestigatorId
    | RemoveDoom Target Int
    | RemoveEnemy EnemyId
    | RemoveFromDiscard InvestigatorId CardId
    | RemoveFromEncounterDiscard EncounterCard
    | RemoveFromGame Target
    | RemovedFromGame Card
    | RemoveLocation LocationId
    | RemoveTraits Target [Trait]
    | RemovedFromPlay Source
    | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
    | RequestSetAsideCard Source CardCode
    | RequestTokens Source (Maybe InvestigatorId) Int RequestedTokenStrategy
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
    | ResolveEvent InvestigatorId EventId (Maybe Target)
    | ResolveToken Token TokenFace InvestigatorId -- since tokens can have their face changed we use this to represent that; TODO: use a real modifier
    | ReturnSkillTestRevealedTokens
    | ReturnToHand InvestigatorId Target
    | ReturnTokens [Token]
    | RevealInHand CardId
    | RevealLocation (Maybe InvestigatorId) LocationId
    | RevealSkillTestTokens InvestigatorId
    | RevealToken Source InvestigatorId Token
    | Revelation InvestigatorId Source
    | RevelationChoice InvestigatorId Source Int
    | RevelationSkillTest InvestigatorId Source SkillType Int
    | RevertAct ActId
    | RevertAgenda AgendaId
    | Run [Message]
    | RunBag Source (Maybe InvestigatorId) RequestedTokenStrategy
    | RunDrawFromBag Source (Maybe InvestigatorId) RequestedTokenStrategy
    | RunSkillTest InvestigatorId
    | RunSkillTestSourceNotification InvestigatorId Source
    | SearchCollectionForRandom InvestigatorId Source CardMatcher
    | SearchDeckForTraits InvestigatorId Target [Trait]
    | SearchDiscard InvestigatorId Target [Trait]
    | SearchTopOfDeck InvestigatorId Source Target Int [Trait] SearchedCardsStrategy
    | SearchTopOfDeckFound InvestigatorId Target DeckSignifier Card
    | SearchTopOfDeckNoneFound InvestigatorId Target
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
    | SkillTestCommitCard InvestigatorId CardId
    | SkillTestEnds Source
    | AfterSkillTestEnds
    | SkillTestResults Int Int Int Int
    | SkillTestUncommitCard InvestigatorId CardId
    | SpawnEnemyAt Card LocationId
    | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
    | SpendClues Int [InvestigatorId]
    | SpendResources InvestigatorId Int
    | SpendUses Target UseType Int
    | StartCampaign
    | StartScenario Name ScenarioId
    | StartSkillTest InvestigatorId
    | SuccessfulInvestigation InvestigatorId LocationId Source
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
    | TriggerSkillTest InvestigatorId
    | TryEvadeEnemy InvestigatorId EnemyId Source SkillType
    | UnengageNonMatching InvestigatorId [Trait]
    | UnfocusCards
    | UnfocusTargets
    | UnfocusTokens
    | UnsetActiveCard
    | UseCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int Payment
    | UseLimitedAbility InvestigatorId Ability
    | UseScenarioSpecificAbility InvestigatorId (Maybe Target) Int
    | When Message
    | WhenAttackEnemy InvestigatorId EnemyId
    | WhenWillEnterLocation InvestigatorId LocationId
    | WhenEnterLocation InvestigatorId LocationId
    | WhenEvadeEnemy InvestigatorId EnemyId
    | Will Message
    -- must be called on instance directly
    | SetOriginalCardCode CardCode
    deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

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

chooseUpgradeDeck :: InvestigatorId -> Message
chooseUpgradeDeck iid = Ask iid ChooseUpgradeDeck

data Question
    = ChooseOne [Message]
    | ChooseN Int [Message]
    | ChooseSome [Message]
    | ChooseUpToN Int [Message]
    | ChooseOneAtATime [Message]
    | ChooseUpgradeDeck
    deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice = SetLeadInvestigator | SetTurnPlayer
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

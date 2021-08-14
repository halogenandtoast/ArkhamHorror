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
import Arkham.Types.Window (Window)
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
    = AddAct ActId
    | AddAgenda AgendaId
    | AddCampaignCardToDeck InvestigatorId CardDef
    | AddCampaignCardToEncounterDeck CardDef
    | AddCardToScenarioDeck Card
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
    | AfterSkillTestEnds Source Target Int
    | AllCheckHandSize
    | AllDrawCardAndResource
    | AllDrawEncounterCard
    | AllInvestigatorsResigned
    | AllRandomDiscard
    | Ask InvestigatorId Question
    | AskMap (HashMap InvestigatorId Question)
    | AskPlayer Message
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
    | CancelDamage InvestigatorId Int
    | CancelFailedByModifierEffects
    | CancelHorror InvestigatorId Int
    | CancelNext MessageType
    | CancelSkillEffects
    | CardLabel CardCode [Message]
    | CheckAdditionalActionCosts InvestigatorId Target Source Action [Message]
    | CheckAttackOfOpportunity InvestigatorId Bool
    | CheckDefeated Source
    | CheckHandSize InvestigatorId
    | CheckWindow InvestigatorId [Window]
    | ChooseAndDiscardAsset InvestigatorId AssetMatcher
    | ChooseAndDiscardCard InvestigatorId
    | ChooseEndTurn InvestigatorId
    | ChooseEvadeEnemy InvestigatorId Source SkillType Bool
    | ChooseFightEnemy InvestigatorId Source SkillType (HashSet Trait) Bool
    | ChooseFightEnemyNotEngagedWithInvestigator InvestigatorId Source SkillType Bool
    | ChooseInvestigate InvestigatorId Source Bool
    | ChooseLeadInvestigator
    | ChooseOneRewardByEachPlayer [CardDef] [InvestigatorId]
    | ChoosePlayer InvestigatorId ChoosePlayerChoice
    | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
    | ChooseRandomLocation Target (HashSet LocationId)
    | ChooseTokenGroups Source InvestigatorId ChaosBagStep
    | ChosenEvadeEnemy Source EnemyId
    | ChosenRandomLocation Target LocationId
    | CommitCard InvestigatorId CardId
    | Continue Text
    | CreateEffect CardCode (Maybe (EffectMetadata Message)) Source Target
    | CreateEnemy Card
    | CreateEnemyAt Card LocationId (Maybe Target)
    | CreateEnemyAtLocationMatching Card LocationMatcher
    | CreateEnemyEngagedWithPrey Card
    | CreatePayAbilityCostEffect Ability Source Target
    | CreateStoryAssetAt Card LocationId
    | CreateStoryAssetAtLocationMatching Card LocationMatcher
    | CreateTokenEffect (EffectMetadata Message) Source Token
    | CreateTokenValueEffect Int Source Target
    | CreateWeaknessInThreatArea Card InvestigatorId
    | CreateWindowModifierEffect EffectWindow (EffectMetadata Message) Source Target
    | CreatedEffect EffectId (Maybe (EffectMetadata Message)) Source Target Window
    | CreatedEnemyAt EnemyId LocationId Target
    | CrossOutRecord CampaignLogKey
    | Damage Target Source Int
    | DeckHasNoCards InvestigatorId (Maybe Target)
    | DefeatEnemy EnemyId InvestigatorId Source
    | DisableEffect EffectId
    | Discard Target
    | DiscardCard InvestigatorId CardId
    | DiscardEncounterUntilFirst Source CardMatcher
    | DiscardTopOfDeck InvestigatorId Int (Maybe Target)
    | DiscardTopOfEncounterDeck InvestigatorId Int (Maybe Target)
    | DiscardTopOfEncounterDeckWithDiscardedCards InvestigatorId Int (Maybe Target) [EncounterCard]
    | Discarded Target Card
    | DiscardedTopOfDeck InvestigatorId [PlayerCard] Target
    | DiscardedTopOfEncounterDeck InvestigatorId [EncounterCard] Target
    | DiscoverClues InvestigatorId LocationId Int (Maybe Action)
    | DiscoverCluesAtLocation InvestigatorId LocationId Int (Maybe Action)
    | DisengageEnemy InvestigatorId EnemyId
    | Done Text
    | DrawAnotherToken InvestigatorId
    | DrawCards InvestigatorId Int Bool
    | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
    | DrawStartingHand InvestigatorId
    | DrawToken InvestigatorId Token
    | DrewPlayerEnemy InvestigatorId Card
    | DrewTreachery InvestigatorId Card
    | DrivenInsane InvestigatorId
    | EmptyDeck InvestigatorId
    | EndCheckWindow
    | EndEnemy
    | EndInvestigation
    | EndMythos
    | EndOfGame
    | EndOfScenario
    | EndPhase
    | EndRound
    | EndRoundWindow
    | EndSearch InvestigatorId Source
    | EndSetup
    | EndTurn InvestigatorId
    | EndUpkeep
    | EnemiesAttack
    | EnemyAttack InvestigatorId EnemyId DamageStrategy
    | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
    | EnemyAttacks [Message]
    | EnemyCheckEngagement EnemyId
    | EnemyDamage EnemyId InvestigatorId Source Int
    | EnemyDefeated EnemyId InvestigatorId LocationId CardCode Source [Trait]
    | EnemyEngageInvestigator EnemyId InvestigatorId
    | EnemyEntered EnemyId LocationId
    | EnemyEvaded InvestigatorId EnemyId
    | EnemyMove EnemyId LocationId LocationId
    | EnemySetBearer EnemyId BearerId
    | EnemySetDamage EnemyId Source Int
    | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
    | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
    | EnemySpawnEngagedWithPrey EnemyId
    | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
    | EnemySpawnedAt LocationId EnemyId
    | EnemyWillAttack InvestigatorId EnemyId DamageStrategy
    | EngageEnemy InvestigatorId EnemyId Bool
    | EngagedEnemyMove EnemyId LocationId LocationId
    | EvadeEnemy InvestigatorId EnemyId Source SkillType Bool
    | EvadeLabel EnemyId [Message]
    | Exhaust Target
    | Exile Target
    | Exiled Target Card
    | FailSkillTest
    | FailedAttackEnemy InvestigatorId EnemyId
    | FailedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
    | FightEnemy InvestigatorId EnemyId Source SkillType Bool
    | FindAndDrawEncounterCard InvestigatorId CardMatcher
    | FindEncounterCard InvestigatorId Target CardMatcher
    | FinishedUpgradingDecks
    | FinishedWithMulligan InvestigatorId
    | FlavorText (Maybe Text) [Text]
    | Flip Source Target
    | FocusCards [Card]
    | FocusTargets [Target]
    | FocusTokens [Token]
    | Force Message
    | ForceTokenDraw TokenFace
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
    | HunterMove EnemyId
    | HuntersMove
    | InDiscard InvestigatorId Message
    | InHand InvestigatorId Message
    | InitDeck InvestigatorId (Deck PlayerCard) -- used to initialize the deck for the campaign
    | InitiatePlayCard InvestigatorId CardId (Maybe Target) Bool
    | InitiatePlayCardAs InvestigatorId CardId Card [Message] Bool
    | InitiatePlayCardAsChoose InvestigatorId CardId [Card] [Message] Bool
    | InitiatePlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
    | InitiatePlayFastEvent InvestigatorId CardId (Maybe Target) Bool
    | Investigate InvestigatorId LocationId Source SkillType Bool
    | InvestigatorAssignDamage InvestigatorId Source DamageStrategy Int Int -- | uses the internal method and then checks defeat
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
    | InvestigatorDoAssignDamage InvestigatorId Source DamageStrategy Int Int [Target] [Target] -- | meant to be used internally by investigators                  ^ damage ^ horror
    | InvestigatorDoDrawEncounterCard InvestigatorId
    | InvestigatorDrawEncounterCard InvestigatorId
    | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
    | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
    | InvestigatorDrewPlayerCard InvestigatorId PlayerCard
    | InvestigatorEliminated InvestigatorId
    | InvestigatorKilled InvestigatorId
    | InvestigatorMulligan InvestigatorId
    | InvestigatorPlaceAllCluesOnLocation InvestigatorId -- | This message exists in case the number of clues will change
    | InvestigatorPlaceCluesOnLocation InvestigatorId Int
    | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
    | InvestigatorPlayDynamicAsset InvestigatorId AssetId [SlotType] [Trait] Int
    | InvestigatorPlayDynamicEvent InvestigatorId EventId Int
    | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target) [Window]
    | InvestigatorPlayedAsset InvestigatorId AssetId [SlotType] [Trait]
    | InvestigatorResigned InvestigatorId
    | InvestigatorSpendClues InvestigatorId Int
    | InvestigatorTakeDamage InvestigatorId Source Int Int
    | InvestigatorWhenDefeated Source InvestigatorId
    | InvestigatorWhenEliminated Source InvestigatorId
    | InvestigatorsMulligan
    | Label Text [Message]
    | LoadDeck InvestigatorId (Deck PlayerCard) -- used to reset the deck of the investigator
    | LookAtRevealed Source Target
    | LookAtTopOfDeck InvestigatorId Target Int
    | LoseActions InvestigatorId Source Int
    | LoseAllResources InvestigatorId
    | LoseResources InvestigatorId Int
    | Move InvestigatorId LocationId LocationId
    | MoveAction InvestigatorId LocationId Cost Bool
    | MoveAllCluesTo Target
    | MoveAllTo LocationId
    | MoveFrom InvestigatorId LocationId
    | MoveTo InvestigatorId LocationId
    | MoveTopOfDeckToBottom Source DeckSignifier Int
    | MoveToward Target LocationMatcher
    | MoveUntil LocationId Target
    | MovedBy InvestigatorId Source
    | NextAct ActId ActId
    | NextAdvanceActStep ActId Int
    | NextAgenda AgendaId AgendaId
    | NextCampaignStep (Maybe CampaignStep)
    | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
    | PaidAbilityCost InvestigatorId (Maybe Action) Payment
    | PassSkillTest
    | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
    | PayAbilityCost Source InvestigatorId (Maybe Action) Window Cost
    | PayAbilityCostFinished EffectId Source InvestigatorId
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
    | PlaceOnBottomOfDeck InvestigatorId PlayerCard
    | PlaceResources Target Int
    | PlaceUnderneath Target [Card]
    | PlacedLocation Name CardCode LocationId
    | PlacedLocationDirection LocationId Direction LocationId
    | PlayCard InvestigatorId CardId (Maybe Target) Bool
    | PlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
    | PlayFastEvent InvestigatorId CardId (Maybe Target) [Window]
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
    | RemoveAllClues Target
    | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
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
    | RemoveLocation LocationId
    | RemoveTraits Target [Trait]
    | RemovedFromGame Card
    | RemovedFromPlay Source
    | ReplaceCurrentDraw Source InvestigatorId ChaosBagStep
    | ReplaceInvestigatorAsset InvestigatorId Card
    | ReplacedInvestigatorAsset InvestigatorId AssetId
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
    | RunWindow InvestigatorId [Window]
    | ScenarioResolution Resolution
    | SearchCollectionForRandom InvestigatorId Source CardMatcher
    | SearchDeckForTraits InvestigatorId Target [Trait]
    | SearchDiscard InvestigatorId Target [Trait]
    | SearchTopOfDeck InvestigatorId Source Target Int [Trait] SearchedCardsStrategy
    | SearchTopOfDeckFound InvestigatorId Target DeckSignifier Card
    | SearchTopOfDeckNoneFound InvestigatorId Target
    | SetActions InvestigatorId Source Int
    | SetActiveInvestigator InvestigatorId
    | SetEncounterDeck (Deck EncounterCard)
    | SetLocationAsIf InvestigatorId LocationId
    | SetLocationLabel LocationId Text
    | SetRole InvestigatorId ClassSymbol
    | SetTokens [TokenFace]
    | SetTokensForScenario
    | Setup
    | SetupInvestigators
    | SetupStep Int
    | ShuffleAllFocusedIntoDeck InvestigatorId Target
    | ShuffleAllInEncounterDiscardBackIn CardCode
    | ShuffleBackIntoEncounterDeck Target
    | ShuffleCardsIntoDeck InvestigatorId [PlayerCard]
    | ShuffleDiscardBackIn InvestigatorId
    | ShuffleEncounterDiscardBackIn
    | ShuffleIntoDeck InvestigatorId Target
    | ShuffleIntoEncounterDeck [EncounterCard]
    | SkillTestApplyResults
    | SkillTestApplyResultsAfter
    | SkillTestAsk Message
    | SkillTestCommitCard InvestigatorId CardId
    | SkillTestEnds Source
    | SkillTestResults Int Int Int Int
    | SkillTestUncommitCard InvestigatorId CardId
    | SpawnEnemyAt Card LocationId
    | SpawnEnemyAtEngagedWith Card LocationId InvestigatorId
    | SpendActions InvestigatorId Source Int
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
    | TakeControlOfSetAsideAsset InvestigatorId Card
    | TakeResources InvestigatorId Int Bool
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
    | UpgradeDeck InvestigatorId (Deck PlayerCard) -- used to upgrade deck during campaign
    | UseAbility InvestigatorId Ability
    | UseCardAbility InvestigatorId Source [Window] Int Payment
    | UseCardAbilityChoice InvestigatorId Source [Window] Int Payment AbilityMetadata
    | UseLimitedAbility InvestigatorId Ability
    | UseScenarioSpecificAbility InvestigatorId (Maybe Target) Int
    | When Message
    | WhenAttackEnemy InvestigatorId EnemyId
    | WhenEnterLocation InvestigatorId LocationId
    | WhenEvadeEnemy InvestigatorId EnemyId
    | WhenWillEnterLocation InvestigatorId LocationId
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

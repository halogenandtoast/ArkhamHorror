module Arkham.Types.Message
  ( module Arkham.Types.Message
  )
where

import Arkham.Prelude

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
import Arkham.Types.Cost
import Arkham.Types.Direction
import Arkham.Types.Effect.Window
import Arkham.Types.EffectId
import Arkham.Types.EffectMetadata
import Arkham.Types.EnemyId
import Arkham.Types.EventId
import Arkham.Types.Exception
import Arkham.Types.InvestigatorId
import Arkham.Types.LocationId
import Arkham.Types.LocationMatcher
import Arkham.Types.LocationSymbol
import Arkham.Types.Name
import Arkham.Types.RequestedTokenStrategy
import Arkham.Types.Resolution
import Arkham.Types.ScenarioId
import Arkham.Types.ScenarioLogKey
import Arkham.Types.SkillId
import Arkham.Types.SkillType
import Arkham.Types.Slot
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Token
import Arkham.Types.Trait
import Arkham.Types.TreacheryId
import Arkham.Types.Window
import Control.Exception

data MessageType
  = RevelationMessage
  | AttackMessage
  | DrawTokenMessage
  | RevealTokenMessage
  | ResolveTokenMessage
  | EnemySpawnMessage
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

messageType :: Message -> Maybe MessageType
messageType PerformEnemyAttack{} = Just AttackMessage
messageType Revelation{} = Just RevelationMessage
messageType DrawToken{} = Just DrawTokenMessage
messageType ResolveToken{} = Just ResolveTokenMessage
messageType EnemySpawn{} = Just EnemySpawnMessage
messageType RevealToken{} = Just RevealTokenMessage
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

data DamageStrategy = DamageAny | DamageAssetsFirst
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncounterCardSource = FromDiscard | FromEncounterDeck | FromTheVoid
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LeftoverCardStrategy = ShuffleBackIn | PutBackInAnyOrder
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

-- TODO: Better handle in play and out of play
-- Out of play refers to player's hand, in any deck,
-- in any discard pile, the victory display, and
-- cards that have been set aside

data Message
  = ActivateCardAbilityAction InvestigatorId Ability
  | AddAbility Source Ability
  | AddAct ActId
  | AddAgenda AgendaId
  | AddCampaignCardToDeck InvestigatorId CardCode
  | AddCampaignCardToEncounterDeck CardCode
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
  | AddToken Token
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
  | ApplyModifiers Target
  | Ask InvestigatorId Question
  | AskMap (HashMap InvestigatorId Question)
  | AssetDamage AssetId Source Int Int
  | AssetDefeated AssetId
  | AttachAsset AssetId Target
  | AttachEvent EventId Target
  | AttachStoryTreacheryTo Card Target
  | AttachTreachery TreacheryId Target
  | AttackEnemy InvestigatorId EnemyId Source SkillType
  | BeforeSkillTest InvestigatorId SkillType
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
  | ChangeCardToFast InvestigatorId CardId
  | CheckAttackOfOpportunity InvestigatorId Bool
  | CheckDefeated Source
  | CheckHandSize InvestigatorId
  | CheckWindow InvestigatorId [Window]
  | ChooseActivateCardAbilityAction InvestigatorId
  | ChooseAndDiscardAsset InvestigatorId
  | ChooseAndDiscardCard InvestigatorId
  | ChooseEndTurn InvestigatorId
  | ChooseEvadeEnemy InvestigatorId Source SkillType Bool
  | ChooseFightEnemy InvestigatorId Source SkillType (HashSet Trait) Bool
  | ChooseFightEnemyNotEngagedWithInvestigator InvestigatorId Source SkillType Bool
  | ChooseLeadInvestigator
  | ChoosePlayer InvestigatorId ChoosePlayerChoice
  | ChoosePlayerOrder [InvestigatorId] [InvestigatorId]
  | ChooseRandomLocation Target (HashSet LocationId)
  | ChosenRandomLocation Target LocationId
  | ChooseTokenGroups Source InvestigatorId ChaosBagStep
  | CommitCard InvestigatorId CardId
  | CompleteObjective
  | Continue Text
  | CreateEffect CardCode (Maybe (EffectMetadata Message)) Source Target
  | CreateEnemy Card
  | CreateEnemyAt Card LocationId (Maybe Target)
  | CreatedEnemyAt EnemyId LocationId Target
  | CreateEnemyAtLocationMatching Card LocationMatcher
  | CreateEnemyEngagedWithPrey Card
  | CreatePayAbilityCostEffect (Maybe Ability) Source Target
  | CreateWindowModifierEffect EffectWindow (EffectMetadata Message) Source Target
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
  | Done
  | DrawAnotherToken InvestigatorId
  | DrawCards InvestigatorId Int Bool
  | DrawEncounterCards Target Int -- Meant to allow events to handle (e.g. first watch)
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
  | ScenarioResolution Resolution
  | EndOfScenario
  | EndPhase
  | EndRound
  | EndRoundWindow
  | EndSearch InvestigatorId
  | EndTurn InvestigatorId
  | EndUpkeep
  | EnemiesAttack
  | EnemyAttack InvestigatorId EnemyId
  | EnemyAttackIfEngaged EnemyId (Maybe InvestigatorId)
  | EnemyAttacks [Message]
  | EnemyCheckEngagement EnemyId
  | EnemyDamage EnemyId InvestigatorId Source Int
  | EnemySetDamage EnemyId Source Int
  | EnemyDefeated EnemyId InvestigatorId LocationId CardCode Source [Trait]
  | EnemyEngageInvestigator EnemyId InvestigatorId
  | EnemyEvaded InvestigatorId EnemyId
  | EnemyMove EnemyId LocationId LocationId
  | EnemyEntered EnemyId LocationId
  | EnemySetBearer EnemyId BearerId
  | EnemySpawn (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnAtLocationMatching (Maybe InvestigatorId) LocationMatcher EnemyId
  | EnemySpawnEngagedWithPrey EnemyId
  | EnemySpawnFromVoid (Maybe InvestigatorId) LocationId EnemyId
  | EnemySpawnedAt LocationId EnemyId
  | EnemyWillAttack InvestigatorId EnemyId
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
  | InDiscard InvestigatorId Message
  | InHand InvestigatorId Message
  | InitDeck InvestigatorId [PlayerCard] -- used to initialize the deck for the campaign
  | UpgradeDeck InvestigatorId [PlayerCard] -- used to upgrade deck during campaign
  | FinishedUpgradingDecks
  | InitiatePlayCard InvestigatorId CardId (Maybe Target) Bool
  | InitiatePlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
  | Investigate InvestigatorId LocationId Source SkillType Bool
  | InvestigatorAssignDamage InvestigatorId Source DamageStrategy Int Int -- ^ uses the internal method and then checks defeat
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
  | InvestigatorDoAssignDamage InvestigatorId Source DamageStrategy Int Int [Target] [Target] -- ^ meant to be used internally by investigators                  ^ damage ^ horror
  | InvestigatorDrawEncounterCard InvestigatorId
  | InvestigatorDrawEnemy InvestigatorId LocationId EnemyId
  | InvestigatorDrewEncounterCard InvestigatorId EncounterCard
  | InvestigatorDrewPlayerCard InvestigatorId PlayerCard
  | InvestigatorEliminated InvestigatorId
  | InvestigatorKilled InvestigatorId
  | InvestigatorMulligan InvestigatorId
  | InvestigatorPlaceAllCluesOnLocation InvestigatorId -- ^ This message exists in case the number of clues will change
  | InvestigatorPlaceCluesOnLocation InvestigatorId Int
  | InvestigatorPlayAsset InvestigatorId AssetId [SlotType] [Trait]
  | InvestigatorPlayDynamicAsset InvestigatorId AssetId [SlotType] [Trait] Int
  | InvestigatorPlayDynamicEvent InvestigatorId EventId Int
  | InvestigatorPlayEvent InvestigatorId EventId (Maybe Target)
  | InvestigatorResigned InvestigatorId
  | InvestigatorSpendClues InvestigatorId Int
  | InvestigatorTakeDamage InvestigatorId Source Int Int
  | InvestigatorWhenDefeated Source InvestigatorId
  | InvestigatorWhenEliminated Source InvestigatorId
  | Label Text [Message]
  | LoadDeck InvestigatorId [PlayerCard] -- used to reset the deck of the investigator
  | LookAtRevealed LocationId
  | LookAtTopOfDeck InvestigatorId Target Int
  | LoseActions InvestigatorId Source Int
  | LoseResources InvestigatorId Int
  | SpendActions InvestigatorId Source Int
  | Move InvestigatorId LocationId LocationId
  | MoveAction InvestigatorId LocationId Cost Bool
  | MoveAllCluesTo Target
  | MoveAllTo LocationId
  | MoveFrom InvestigatorId LocationId
  | MoveTo InvestigatorId LocationId
  | MoveToward Target LocationMatcher
  | MoveUntil LocationId Target
  | NextAct ActId ActId
  | NextAdvanceActStep ActId Int
  | NextAgenda AgendaId AgendaId
  | NextCampaignStep (Maybe CampaignStep)
  | NextChaosBagStep Source (Maybe InvestigatorId) RequestedTokenStrategy
  | PassSkillTest
  | PassedSkillTest InvestigatorId (Maybe Action) Source Target SkillType Int
  | PayAbilityCost Source InvestigatorId (Maybe Action) Cost
  | PayAbilityCostFinished Source InvestigatorId
  | PaidAbilityCost InvestigatorId (Maybe Action) Payment
  | PayCardCost InvestigatorId CardId
  | PayDynamicCardCost InvestigatorId CardId Int [Message]
  | PayForCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int
  | PayedForDynamicCard InvestigatorId CardId Int Bool
  | PerformEnemyAttack InvestigatorId EnemyId
  | PlaceClues Target Int
  | PlaceDoom Target Int
  | PlaceDoomOnAgenda
  | PlaceEnemyInVoid EnemyId
  | PlaceLocation CardCode LocationId
  | PlaceLocationMatching LocationMatcher
  | PlaceResources Target Int
  | PlaceUnderneath Target [Card]
  | PlacedLocation Name CardCode LocationId
  | PlacedLocationDirection LocationId Direction LocationId
  | PlayCard InvestigatorId CardId (Maybe Target) Bool
  | PlayDynamicCard InvestigatorId CardId Int (Maybe Target) Bool -- Int is unused for Bool True
  | PlayedCard InvestigatorId CardId Name CardCode
  | PlayerWindow InvestigatorId [Message]
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
  | RemoveAbilitiesFrom Source
  | RemoveAllCopiesOfCardFromGame InvestigatorId CardCode
  | RemoveAllClues Target
  | RemoveAllDoom
  | RemoveCampaignCardFromDeck InvestigatorId CardCode
  | RemoveCardFromHand InvestigatorId CardCode
  | RemoveClues Target Int
  | RemoveDiscardFromGame InvestigatorId
  | RemoveDoom Target Int
  | RemoveEnemy EnemyId
  | RemoveFromDiscard InvestigatorId CardId
  | RemoveFromEncounterDiscard EncounterCard
  | RemoveFromGame Target
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
  | ResolveToken DrawnToken Token InvestigatorId
  | ReturnSkillTestRevealedTokens
  | ReturnToHand InvestigatorId Target
  | ReturnTokens [Token]
  | RevealInHand CardId
  | RevealLocation (Maybe InvestigatorId) LocationId
  | RevealSkillTestTokens InvestigatorId
  | RevealToken Source InvestigatorId Token
  | Revelation InvestigatorId Source
  | RevelationSkillTest InvestigatorId Source SkillType Int
  | RevertAct ActId
  | RevertAgenda AgendaId
  | Run [Message]
  | RunBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RunDrawFromBag Source (Maybe InvestigatorId) RequestedTokenStrategy
  | RunSkillTest InvestigatorId
  | RunSkillTestSourceNotification InvestigatorId Source
  | SearchCollectionForRandom InvestigatorId Source (CardType, HashSet Trait)
  | SearchDeckForTraits InvestigatorId Target [Trait]
  | SearchDiscard InvestigatorId Target [Trait]
  | SearchTopOfDeck InvestigatorId Target Int [Trait] LeftoverCardStrategy
  | SetActions InvestigatorId Source Int
  | SetEncounterDeck [EncounterCard]
  | SetLocationLabel LocationId Text
  | SetTokens [Token]
  | SetTokensForScenario
  | Setup
  | EndSetup
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
  | SkillTestResults
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
  | UseCardAbility InvestigatorId Source (Maybe AbilityMetadata) Int Payment
  | UseLimitedAbility InvestigatorId Ability
  | UseScenarioSpecificAbility InvestigatorId (Maybe Target) Int
  | When Message
  | WhenAttackEnemy InvestigatorId EnemyId
  | WhenEnterLocation InvestigatorId LocationId
  | WhenEvadeEnemy InvestigatorId EnemyId
  | Will Message
  | WithCount Int Message
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

chooseOne :: HasCallStack => InvestigatorId -> [Message] -> Message
chooseOne _ [] = throw $ InvalidState $ "No messages for chooseOne\n" <> pack
  (prettyCallStack callStack)
chooseOne iid msgs = Ask iid (ChooseOne msgs)

chooseOneAtATime :: HasCallStack => InvestigatorId -> [Message] -> Message
chooseOneAtATime _ [] =
  throw $ InvalidState $ "No messages for chooseOneAtATime\n" <> pack
    (prettyCallStack callStack)
chooseOneAtATime iid msgs = Ask iid (ChooseOneAtATime msgs)

chooseSome :: HasCallStack => InvestigatorId -> [Message] -> Message
chooseSome _ [] = throw $ InvalidState $ "No messages for chooseSome\n" <> pack
  (prettyCallStack callStack)
chooseSome iid msgs = Ask iid (ChooseSome $ Done : msgs)

chooseN :: HasCallStack => InvestigatorId -> Int -> [Message] -> Message
chooseN _ _ [] = throw $ InvalidState $ "No messages for chooseN\n" <> pack
  (prettyCallStack callStack)
chooseN iid n msgs = Ask iid (ChooseN n msgs)

chooseUpgradeDeck :: InvestigatorId -> Message
chooseUpgradeDeck iid = Ask iid ChooseUpgradeDeck

data Question
  = ChooseOne [Message]
  | ChooseN Int [Message]
  | ChooseSome [Message]
  | ChooseOneAtATime [Message]
  | ChooseUpgradeDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ChoosePlayerChoice = SetLeadInvestigator
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

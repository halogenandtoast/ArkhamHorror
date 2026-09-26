module AH3e.Message where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State

data Message
  = ChooseScenario
  | SelectScenario ScenarioCode
  | RandomScenario
  | AskInvestigatorChoice
  | SelectInvestigator PlayerId InvestigatorId
  | GainStartingPossessions InvestigatorId [StartingPossession]
  | GainNamedStarting InvestigatorId CardCode
  | SetupEncounterDecks
  | PlaceAtStartingSpace InvestigatorId
  | FinalPreparations
  | BeginRound
  | BeginActionPhase
  | NextActionTurn
  | StartActionTurn InvestigatorId
  | ActionTurn InvestigatorId
  | PerformAction InvestigatorId ActionKind
  | AfterAction InvestigatorId ActionKind
  | StandUp InvestigatorId
  | EndActionTurn InvestigatorId
  | BeginMonsterPhase
  | MonsterActivationStep
  | ActivateMonster CardId
  | MonsterAttackStep [InvestigatorId]
  | MonstersAttack InvestigatorId [CardId]
  | MonsterAttacks CardId InvestigatorId
  | MonsterReadyStep
  | BeginEncounterPhase
  | NextEncounterTurn
  | StartEncounterTurn InvestigatorId
  | ResolveEncounterFrom InvestigatorId EncounterDeck
  | ResolveTerrorEncounter InvestigatorId
  | AcknowledgeEncounter
  | FinishEncounter
  | EndEncounterTurn InvestigatorId
  | BeginMythosPhase
  | MythosTurn [PlayerId]
  | DrawMythosToken PlayerId
  | ResolveMythosToken PlayerId MythosToken
  | EndRound
  | ReplaceInvestigator PlayerId
  | ResolveEffect EffectCtx Effect
  | ChooseInvestigatorsFor EffectCtx Int [InvestigatorId] Effect
  | CheckReactions Trigger [Text]
  | ContinueTest
  | MarkAssetUsed InvestigatorId CardId
  | PayCost EffectCtx Cost
  | BeginTest TestState
  | ToggleTestAsset CardId
  | RollDice
  | SpendForReroll RerollCost
  | RerollDie RerollCost Int
  | -- | reroll dice one at a time, at most this many, stopping whenever they like
    RerollUpTo Source Int
  | RerollOneOf Source Int Int
  | RerollAll Source
  | -- | add one to the result of a die of their choice
    AddToDie Source
  | RaiseDie Int
  | MarkUsedInTest CardId
  | FinishTest
  | MoveStep MoveState
  | MoveInvestigator MoveState SpaceId
  | UseTravelRoute MoveState SpaceId
  | MoveDirectly InvestigatorId SpaceId
  | EnterSpace InvestigatorId SpaceId
  | EngageMonster InvestigatorId CardId
  | DisengageMonster InvestigatorId CardId
  | ExhaustMonster CardId
  | ReadyMonster CardId
  | CheckEngagement CardId
  | MonsterStep CardId Int MonsterTarget
  | MoveMonsterTo CardId SpaceId
  | MonsterEngagesIn CardId SpaceId
  | AttackMonster InvestigatorId CardId
  | AttackDamage InvestigatorId CardId Int
  | AttackResolved InvestigatorId CardId Int
  | EvadeMonsters InvestigatorId Int
  | SufferHarm InvestigatorId Source HarmKind Int Int
  | CastSpell InvestigatorId CardId [Message]
  | PayCastCost InvestigatorId CardId Int Bool [Message]
  | ResumeCast InvestigatorId CardId [Message]
  | -- | offer everyone holding a card that prevents damage, then assign what is left
    PreventDamage HarmPlan [Text]
  | HarmDamageStage HarmPlan
  | HarmHorrorStage HarmPlan
  | HarmChooseAmount HarmStat CardId Int HarmPlan
  | ResolveHarm HarmPlan
  | HarmAsset CardId Int Int
  | -- | the cards of whoever suffered it answer a harm plan that has landed
    HarmResolved HarmPlan
  | {- | add successes to the test in progress, or to the next one to begin when a
    card promises them while paying for a spell
    -}
    AddTestSuccesses Int
  | ApplyHarm InvestigatorId Source Int Int
  | CheckDefeat InvestigatorId
  | DefeatInvestigator InvestigatorId
  | DevourInvestigator InvestigatorId
  | RetireInvestigator InvestigatorId
  | DealMonsterDamage CardId Source Int
  | DefeatMonster CardId Source
  | DiscardMonster CardId
  | SpawnMonsterAt (Maybe SpaceId) Bool
  | PlaceMonster CardId SpaceId MonsterState
  | PlaceDoom Source SpaceId
  | PlaceDoomInStreet Source SpaceId
  | PlaceDoomOnSheet Int
  | PlaceDoomInOrder Source [SpaceId]
  | RemoveDoom SpaceId Int
  | SpreadDoom
  | SpawnClue
  | GateBurst
  | SpreadTerror NeighborhoodId
  | CheckDoomThresholds SpaceId
  | ChooseGateBurstSpaces [SpaceId] [SpaceId]
  | ClearSpaceDoom SpaceId
  | WardRemove InvestigatorId SpaceId Int
  | CheckStateTriggers
  | GainAsset InvestigatorId CardId
  | DiscardAsset CardId
  | GainNamedCard InvestigatorId Text
  | GainConditionMsg InvestigatorId ConditionName
  | FocusSkill InvestigatorId Skill Bool
  | FocusSkillAgain InvestigatorId Skill
  | DiscardFocus InvestigatorId Skill
  | {- | If a headline resolved without asking anything, give the players a
    moment to read it before it is discarded. Carries the question count from
    before the headline resolved.
    -}
    AcknowledgeHeadline InvestigatorId Int
  | {- | Take a card off the active card area once it has finished resolving,
    unless something newer has taken its place.
    -}
    ClearActiveCard CardId
  | {- | Hold a freshly drawn mythos token in front of the players until they
    continue, so it is read before it resolves.
    -}
    AcknowledgeMythosToken PlayerId MythosToken
  | -- | Take the drawn mythos token away once it has finished resolving.
    ClearActiveToken
  | -- | Discard one clue; 'Nothing' takes it from the scenario sheet.
    DiscardClue (Maybe InvestigatorId)
  | ResearchClues InvestigatorId Int
  | ResearchCluesExact InvestigatorId Int
  | PayMoney InvestigatorId Int
  | TradeWith InvestigatorId InvestigatorId
  | TradeTransfer InvestigatorId InvestigatorId TradeItem
  | BuyFromDisplayMsg EffectCtx (Maybe Trait) Bool (Maybe Int) Effect
  | BuyCard InvestigatorId CardId Int
  | BuyFromDisplayMore EffectCtx (Maybe Trait) Bool (Maybe Int) Effect Int
  | CycleDisplay InvestigatorId Int
  | DiscardFromDisplay CardId
  | RefillDisplay
  | AddArchiveToCodex ArchiveNumber
  | AddArchiveToCodexFlipped ArchiveNumber
  | FlipCodexCard ArchiveNumber
  | RemoveCodexCard ArchiveNumber
  | DrawHeadline InvestigatorId
  | DiscardHeadline CardId
  | DiscardRumor
  | AddRumorDoom
  | OfferRumorDiscard EffectCtx Effect
  | BuyFromDisplayChecked EffectCtx (Maybe Trait) Bool (Maybe Int) Effect
  | IgnoreRumor InvestigatorId
  | GainItemFromDeck InvestigatorId AssetDeckKind (Maybe Trait) (Maybe ValueBound)
  | BuyRevealed EffectCtx AssetDeckKind [CardId] (Maybe Int) Pricing Int
  | -- | take clues off the scenario sheet, for a card that spends them
    SpendSheetClues Int
  | -- | put this many of the pool's markers on the scenario sheet
    MarkSheet Int
  | ReturnToBottom AssetDeckKind [CardId]
  | GainFromDisplay InvestigatorId CardId
  | RecoverInvestigator InvestigatorId Int Int
  | RecoverAsset CardId Int Int
  | EncounterOptions InvestigatorId
  | ResolveReckonings [Source]
  | ResolveReckoning Source
  | WinTheGame
  | LoseTheGame Text
  | Debug DebugAction
  | -- | runs the card's "after you gain this card from the deck" ability
    AfterGainedFromDeck InvestigatorId CardId
  | -- | asks only while the investigator still has that asset
    AskAboutAsset InvestigatorId CardId Text [Choice]
  | -- puts back the questions a debug action set aside
    RestoreQuestions (Map PlayerId Question)
  | LogText Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MonsterTarget = TowardSpaces SpaceRule | TowardPrey InvestigatorRule
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TradeItem = TradeMoney Int | TradeClues Int | TradeRemnants Int | TradeCard CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Label
  = TextLabel Text
  | ActionLabel ActionKind
  | SpaceLabel SpaceId
  | MonsterLabel CardId
  | CardLabel CardId
  | SkillLabel Skill
  | DieLabel Int Int
  | InvestigatorLabel InvestigatorId
  | ScenarioLabel ScenarioCode
  | AmountLabel Int
  | TokenLabel MythosToken
  | SourceLabel Source
  | DoneLabel Text
  | -- | text for the choice, with the cards it would give so they can be shown
    CardsLabel Text [CardId]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Choice = Choice {label :: Label, messages :: [Message]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question = Question {prompt :: Text, choices :: [Choice]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  | {- | an action someone else's card granted: it spends none of their own actions,
    and the flag lets it repeat one they have already taken
    -}
    PerformGrantedAction InvestigatorId ActionKind Bool
  | -- | ask who takes the granted action, if anyone
    OfferGrantedAction InvestigatorId ActionKind
  | -- | an ability a card gives freely during its owner's turn; costs no action
    PerformFreeAction InvestigatorId ComponentRef Int
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
  | -- | an investigator's own ability is spent for the round
    MarkAbilityUsed InvestigatorId Text
  | PayCost EffectCtx Cost
  | BeginTest TestState
  | -- | remembers one thing on a card while its own test resolves
    RememberOnCard CardId Text
  | -- | notes a number on a card, for a card whose own test resolves later
    NoteOnCard CardId Text Int
  | -- | gain remnants, offering first whatever a card takes in their place
    GainRemnants InvestigatorId Int
  | GainRemnantsNow InvestigatorId Int
  | -- | add one to a die instead of rerolling it, paying the reroll's cost
    RaiseInsteadOfReroll RerollCost Int CardId
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
  | -- | set a die of their choice to this value (Grave Dirt's six)
    ChooseDieToSet Int
  | SetDieValue Int Int
  | -- | hold this effect back until the test in progress has resolved
    AddTestRider EffectCtx Effect
  | RaiseDie Int
  | MarkUsedInTest CardId
  | FinishTest
  | MoveStep MoveState
  | MoveInvestigator MoveState SpaceId
  | UseTravelRoute MoveState SpaceId
  | MoveDirectly InvestigatorId SpaceId
  | EnterSpace InvestigatorId SpaceId
  | EngageMonster InvestigatorId CardId
  | -- | the engagement itself, once anyone who could step in has decided
    EngageMonsterNow InvestigatorId CardId
  | DisengageMonster InvestigatorId CardId
  | ExhaustMonster CardId
  | -- | the activation itself, once anything that could replace it has passed
    DoActivateMonster CardId
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
  | -- | offer everyone holding a card that prevents harm, then assign what is left
    PreventHarm HarmPlan [Text]
  | -- | harm a card has prevented, which the step it interrupted picks up
    PreventedHarm Int Int
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
  | -- | the monster, once anyone who could stop it has decided
    PlaceMonsterNow CardId SpaceId MonsterState
  | PlaceDoom Source SpaceId
  | -- | the doom, once anyone who could stop it has decided
    PlaceDoomNow Source SpaceId
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
  | -- | put a face-up marker of that colour on the space
    PlaceMarker SpaceId Text
  | -- | put a face-up marker of that colour on the monster, which travels with it
    PlaceMonsterMarker CardId Text
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
  | BuyFromDisplayMsg EffectCtx (Maybe Trait) Pricing (Maybe Int) Effect
  | BuyCard InvestigatorId CardId Int
  | BuyFromDisplayMore EffectCtx (Maybe Trait) Pricing (Maybe Int) Effect Int
  | CycleDisplay InvestigatorId Int
  | DiscardFromDisplay CardId
  | RefillDisplay
  | AddArchiveToCodex ArchiveNumber
  | AddArchiveToCodexFlipped ArchiveNumber
  | FlipCodexCard ArchiveNumber
  | RemoveCodexCard ArchiveNumber
  | -- | the removal itself, once they have read the side that sends it away
    DiscardCodexCard ArchiveNumber
  | DrawHeadline InvestigatorId
  | DiscardHeadline CardId
  | DiscardRumor
  | AddRumorDoom
  | OfferRumorDiscard EffectCtx Effect
  | BuyFromDisplayChecked EffectCtx (Maybe Trait) Pricing (Maybe Int) Effect
  | IgnoreRumor InvestigatorId
  | GainItemFromDeck InvestigatorId AssetDeckKind (Maybe Trait) (Maybe ValueBound)
  | BuyRevealed EffectCtx AssetDeckKind [CardId] (Maybe Int) Pricing Int
  | -- | take clues off the scenario sheet, for a card that spends them
    SpendSheetClues Int
  | -- | put this many of the pool's markers on the scenario sheet
    MarkSheet Int
  | {- | add to (or, negative, take from) one of the scenario sheet's named token
    piles; a scenario keeps its own state here too
    -}
    MarkSheetToken Text Int
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

data TradeItem
  = TradeMoney Int
  | TradeClues Int
  | TradeRemnants Int
  | TradeCard CardId
  | TradeFocus Skill
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

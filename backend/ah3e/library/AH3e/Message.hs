module AH3e.Message where

import AH3e.Prelude
import AH3e.Types.Board
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
  | {- | another action of their own choosing, which may repeat one they have
    already taken this round (the white markers of the Silver Twilight Lodge)
    -}
    GrantAnotherAction InvestigatorId
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
  | -- | the draw itself, once anything offered in its place has been declined
    DrawMythosTokenNow PlayerId
  | ResolveMythosToken PlayerId MythosToken
  | -- | the token's own effect, once the drawer's cards have had their say
    ResolveMythosTokenNow PlayerId MythosToken
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
  | -- | test this skill instead, the pool not yet being rolled (Voice of Authority)
    SetTestSkill Skill
  | RollDice
  | SpendForReroll RerollCost
  | RerollDie RerollCost Int
  | {- | roll this many more dice into the test in progress, after the pool has
    already been rolled (Just That Good, Reckless Resolve)
    -}
    RollAdditionalDice Source Int
  | {- | roll one more die for each die in the test that is not a success, which
    only the engine can count (Reckless Resolve)
    -}
    RollADiePerFailure Source
  | -- | take one die of their choice out of the test in progress (FATIGUED)
    RemoveADie Source
  | RemoveDieAt Int
  | -- | reroll dice one at a time, at most this many, stopping whenever they like
    RerollUpTo Source Int
  | -- | the staged rerolls themselves, once any surcharge on rerolling is paid
    RerollUpToNow Source Int
  | RerollOneOf Source Int Int
  | RerollAll Source
  | -- | add one to the result of a die of their choice
    AddToDie Source
  | -- | set a die of their choice to this value (Grave Dirt's six)
    ChooseDieToSet Int
  | -- | pick a result, then the die it replaces (Lucky Coin)
    ChooseDieResult
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
  | {- | offer each of these investigators, one at a time, a ride to that space
    (Delivery Truck)
    -}
    OfferRide [InvestigatorId] SpaceId
  | EnterSpace InvestigatorId SpaceId
  | EngageMonster InvestigatorId CardId
  | -- | the engagement itself, once anyone who could step in has decided
    EngageMonsterNow InvestigatorId CardId
  | DisengageMonster InvestigatorId CardId
  | -- | name this monster's prey for the monster phase, in place of its printed rule
    SetMonsterPrey CardId InvestigatorId
  | ExhaustMonster CardId
  | -- | the activation itself, once anything that could replace it has passed
    DoActivateMonster CardId
  | ReadyMonster CardId
  | CheckEngagement CardId
  | MonsterStep CardId Int MonsterTarget
  | MoveMonsterTo CardId SpaceId
  | MonsterEngagesIn CardId SpaceId
  | {- | the monster attacked, chosen once anything that hauls one in has moved it
    (Harpoon)
    -}
    ChooseAttackTarget InvestigatorId
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
  | -- | slipped past, so the monster has its say before it is left behind
    EvadedMonster InvestigatorId CardId
  | -- | closed with, so the monster has its say about whoever it caught
    MonsterEngaged InvestigatorId CardId
  | -- | whatever is holding this investigator has its say about what they just did
    MonstersWatchAction InvestigatorId ActionKind
  | DefeatMonster CardId Source
  | DiscardMonster CardId
  | SpawnMonsterAt (Maybe SpaceId) Bool
  | PlaceMonster CardId SpaceId MonsterState
  | -- | the monster, once anyone who could stop it has decided
    PlaceMonsterNow CardId SpaceId MonsterState
  | {- | put a piece of map into play, laid against a tile already on the board: the
    piece carries that tile at its own origin, so the board has only to shift it
    -}
    AddToBoard NeighborhoodId MapDef
  | PlaceDoom Source SpaceId
  | -- | the doom, once anyone who could stop it has decided
    PlaceDoomNow Source SpaceId
  | PlaceDoomInStreet Source SpaceId
  | PlaceDoomOnSheet Int
  | PlaceDoomInOrder Source [SpaceId]
  | RemoveDoom SpaceId Int
  | SpreadDoom
  | SpawnClue
  | {- | spawn a clue whose event card goes on top of its encounter deck rather
    than being shuffled in with the top two (Spirit Camera)
    -}
    SpawnClueOnTop
  | GateBurst
  | SpreadTerror NeighborhoodId
  | CheckDoomThresholds SpaceId
  | ChooseGateBurstSpaces [SpaceId] [SpaceId]
  | ClearSpaceDoom SpaceId
  | -- | put a face-up marker of that colour on the space
    PlaceMarker SpaceId Text
  | -- | clear every marker of one colour from the board
    DiscardMarkers Text
  | -- | a marker the whole neighborhood holds, rather than one of its spaces
    PlaceNeighborhoodMarker NeighborhoodId Text Bool
  | -- | a marker placed face down, for a card that hides what it put there
    PlaceMarkerFacedown SpaceId Text
  | -- | turn one of a space's facedown markers face up, whatever colour it proves
    RevealMarkerAt SpaceId
  | -- | lay the top of the ally deck facedown in that space as a bystander
    PlaceBystander SpaceId
  | {- | a condition taken although one of that name is already held, for a card
    printed "even if you already have one" (The Key and the Gate's dark pacts)
    -}
    GainAnotherCondition InvestigatorId ConditionName
  | -- | an investigator turns a bystander face up and keeps the ally card
    TakeBystander InvestigatorId CardId
  | -- | the monsters reached that bystander first
    DiscardBystander CardId
  | {- | walk a corner piece round a tile to the next corner of it, turning the piece
    as it is laid back down (Secrets of the Order card 135)
    -}
    MoveCornerTile ThresholdType NeighborhoodId
  | -- | clues handed straight to an investigator, off the scenario sheet
    TakeClues InvestigatorId Int
  | -- | tokens a codex card keeps on itself, which several of them count
    MarkCodexToken ArchiveNumber Text Int
  | {- | clues reaching the scenario sheet once every card has had its say about
    them, so a card answering that can still send some of them there
    -}
    AddSheetClues Int
  | -- | put a face-up marker of that colour on the monster, which travels with it
    PlaceMonsterMarker CardId Text
  | WardRemove InvestigatorId SpaceId Int
  | {- | spend a ward's successes one at a time, for a card that offers something
    else to do with them: successes left, and doom taken off so far
    -}
    WardStep InvestigatorId SpaceId Int Int
  | CheckStateTriggers
  | -- | the gain itself, once anything that answers it has had its say
    GainNow EffectCtx Gain
  | GainAsset InvestigatorId CardId
  | -- | put this card under another, which may be an asset or a monster
    AttachAsset CardId CardId
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
  | -- | the purchase itself, once anything that reshuffles the display has passed
    BuyFromDisplayNow EffectCtx (Maybe Trait) Pricing (Maybe Int) Effect
  | BuyCard InvestigatorId CardId Int
  | BuyFromDisplayMore EffectCtx (Maybe Trait) Pricing (Maybe Int) Effect Int
  | CycleDisplay InvestigatorId Int
  | DiscardFromDisplay CardId
  | RefillDisplay
  | {- | a card turned up from a face-down pile of archive cards: it is put in
    front of the table to be read before whatever turning it up does
    -}
    RevealArchiveCard ArchiveNumber [Message]
  | AddArchiveToCodex ArchiveNumber
  | AddArchiveToCodexFlipped ArchiveNumber
  | -- | flip once the table has read the side showing
    TurnCodexCard ArchiveNumber
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
  | {- | an "Encounter:" ability taken in place of the encounter its taker would
    have resolved (Under Dark Waves)
    -}
    UseEncounterAbility InvestigatorId ComponentRef Int
  | ResolveReckonings [Source]
  | ResolveReckoning Source
  | -- | the reckoning itself, once anything that could hold it back has passed
    ResolveReckoningNow Source
  | -- | that source's reckoning does not resolve again this mythos phase
    CancelReckoning Text
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
  | {- | likewise, but by card code, for cards that do not exist yet: a starting
    possession is minted when it is taken, and a box that is not on the table has
    dealt no copy to borrow a picture from
    -}
    CardCodesLabel Text [CardCode]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Choice = Choice {label :: Label, messages :: [Message]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question = Question {prompt :: Text, choices :: [Choice]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

module AH3e.Types.State where

import AH3e.Prelude
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill

data GameMode = StandardMode | StoryMode | ChallengeMode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Phase = SetupPhase | ActionPhase | MonsterPhase | EncounterPhase | MythosPhase | EndPhase
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ComponentRef
  = SheetRef InvestigatorId
  | CardRef CardId
  | CodexRef ArchiveNumber
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ActionKind
  = MoveAction
  | GatherResourcesAction
  | FocusAction
  | WardAction
  | AttackAction
  | EvadeAction
  | ResearchAction
  | TradeAction
  | ComponentAction ComponentRef Int
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Source
  = SourceEncounter CardId
  | SourceCard CardId
  | SourceMonster CardId
  | SourceInvestigator InvestigatorId
  | SourceScenario
  | SourceCodex ArchiveNumber
  | SourceHeadline CardId
  | SourceMythos
  | SourceRules
  | SourceDebug
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EffectCtx = EffectCtx
  { investigator :: InvestigatorId
  , source :: Source
  , testResult :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Trigger
  = -- | the owner of a card may act while someone else's test is resolving
    AnotherResolvesTest InvestigatorId InvestigatorId
  | AfterGatherResources InvestigatorId
  | AfterResearchAction InvestigatorId
  | -- | a research action has finished, and this was its test result
    AfterResearchResult InvestigatorId Int
  | -- | this many clues have just gone from an investigator onto the sheet
    AfterCluesResearched InvestigatorId Int
  | AfterMoveAction InvestigatorId
  | -- | they took this much doom off their own space, which some sheets answer
    AfterDoomRemoved InvestigatorId Int
  | AfterFailedTest InvestigatorId
  | AfterPassedTest InvestigatorId
  | -- | they and this monster have just come apart
    AfterDisengage InvestigatorId CardId
  | -- | they and this monster have just come together
    AfterEngaged InvestigatorId CardId
  | -- | this much doom has just gone onto the scenario sheet
    AfterDoomOnSheet InvestigatorId Int
  | -- | an action of theirs has finished, whichever it was
    AfterAnyAction InvestigatorId ActionKind
  | {- | somebody else's action has finished; the first is the card's owner, whose
    decision it is, and the second whoever took it
    -}
    AnotherPerformsAction InvestigatorId InvestigatorId ActionKind
  | AfterSpendRemnant InvestigatorId
  | -- | the encounter has finished resolving; its investigator is still standing where it happened
    AfterEncounter InvestigatorId
  | -- | the monster defeated is gone by now, so only the attacker is carried
    AfterDefeatMonsterInAttack InvestigatorId
  | -- | they have just dealt damage to this monster as part of an attack action
    AfterDamageMonsterInAttack InvestigatorId CardId
  | -- | a move action has ended, having carried them this many spaces
    AfterMoveDistance InvestigatorId Int
  | AtStartOfTurn InvestigatorId
  | {- | their turn is over bar anything that answers its ending, which may still
    hand them another action (War of Attrition, DRIVEN)
    -}
    AtEndOfTurn InvestigatorId
  | -- | a clue has just come to them off their neighborhood (Occult Principle)
    AfterGainNeighborhoodClue InvestigatorId
  | -- | a ward action of theirs has finished, and this was its test result
    AfterWardResult InvestigatorId Int
  | -- | they have just focused this skill as part of a focus action
    AfterFocusedSkill InvestigatorId Skill
  | {- | that monster has just arrived in their space, whether it moved there or
    spawned there (One Man Army)
    -}
    AfterMonsterArrives InvestigatorId CardId
  | {- | a monster has just been put on the board from off it, wherever it landed;
    everyone in play is asked, since a card may answer a spawn across town
    (Cryptic Sketches)
    -}
    AfterMonsterSpawned InvestigatorId CardId
  | -- | they have just become delayed, having not been a moment ago (The Red Clock)
    AfterBecomeDelayed InvestigatorId
  | -- | they have just slipped past that monster as part of an evade action
    AfterEvadeMonster InvestigatorId CardId
  | AtEndOfMonsterPhase InvestigatorId
  | -- | this many tokens have just gone into the mythos cup
    TokensReturnedToCup InvestigatorId Int
  | DrewBlankToken InvestigatorId
  | SpentFocusToReroll InvestigatorId
  | AfterCastSpell InvestigatorId CardId
  | -- | someone in this investigator's space has just recovered sanity
    AfterRecoverSanity InvestigatorId RecoverTarget
  | -- | a card has just joined the codex, or one already there has turned over
    AfterCodexChanged InvestigatorId
  | -- | the encounter just resolved came off the street deck (426.5)
    AfterStreetEncounter InvestigatorId
  | {- | the action is about to be performed and can still be prepared for; the
    action itself has not begun, so nothing about it has been chosen yet
    -}
    BeforePerformAction InvestigatorId ActionKind
  | {- | cards of this kind are about to be bought or gained, while the display can
    still be changed (Eye for Appraisal)
    -}
    BeforeAcquiring InvestigatorId (Maybe Trait)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Who a recovery reached, so a card can add to that same one.
data RecoverTarget = RecoveredInvestigator InvestigatorId | RecoveredAsset CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

triggerInvestigator :: Trigger -> InvestigatorId
triggerInvestigator = \case
  AnotherResolvesTest owner _ -> owner
  AfterGatherResources iid -> iid
  AfterResearchAction iid -> iid
  AfterResearchResult iid _ -> iid
  AfterCluesResearched iid _ -> iid
  AfterMoveAction iid -> iid
  AfterDoomRemoved iid _ -> iid
  AfterFailedTest iid -> iid
  AfterPassedTest iid -> iid
  AfterDisengage iid _ -> iid
  AfterEngaged iid _ -> iid
  AfterDoomOnSheet iid _ -> iid
  AfterAnyAction iid _ -> iid
  AnotherPerformsAction owner _ _ -> owner
  AfterSpendRemnant iid -> iid
  AfterEncounter iid -> iid
  AfterDefeatMonsterInAttack iid -> iid
  AfterDamageMonsterInAttack iid _ -> iid
  AfterMoveDistance iid _ -> iid
  AtStartOfTurn iid -> iid
  AtEndOfTurn iid -> iid
  AfterGainNeighborhoodClue iid -> iid
  AfterWardResult iid _ -> iid
  AfterFocusedSkill iid _ -> iid
  AfterMonsterArrives iid _ -> iid
  AfterMonsterSpawned iid _ -> iid
  AfterBecomeDelayed iid -> iid
  AfterEvadeMonster iid _ -> iid
  AtEndOfMonsterPhase iid -> iid
  TokensReturnedToCup iid _ -> iid
  DrewBlankToken iid -> iid
  SpentFocusToReroll iid -> iid
  AfterCastSpell iid _ -> iid
  AfterRecoverSanity iid _ -> iid
  AfterCodexChanged iid -> iid
  AfterStreetEncounter iid -> iid
  BeforePerformAction iid _ -> iid
  BeforeAcquiring iid _ -> iid

data HarmKind = NormalHarm | DirectHarm
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data HarmStat = DamageStat | HorrorStat
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Damage and horror being suffered at once (rules 416.7, 442.7): some or
all of each may go to a single asset, the same asset or a different one,
and whatever is left goes to the investigator.
-}
data HarmPlan = HarmPlan
  { investigator :: InvestigatorId
  , source :: Source
  , kind :: HarmKind
  , damage :: Int
  , horror :: Int
  , damageTo :: Maybe (CardId, Int)
  , horrorTo :: Maybe (CardId, Int)
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MonsterState = Ready | Exhausted | Engaged [InvestigatorId]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TestKind
  = EncounterTest
  | ActionTest ActionKind (Maybe CardId)
  | SpellTest CardId
  | OtherTest
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data AfterTest
  = AfterEffect EffectCtx Effect Effect
  | AfterAttack InvestigatorId CardId
  | AfterEvade InvestigatorId
  | AfterResearch InvestigatorId
  | AfterWard InvestigatorId SpaceId
  | AfterSpell InvestigatorId CardId
  | -- | the result is the damage a 'PreventDamage' step prevents
    AfterPreventDamage
  | -- | exhaust this monster if the test passed
    AfterExhaustMonster CardId
  | -- | move this many spaces beyond the result, for a spell taken as a move action
    AfterMoveSpell InvestigatorId Int
  | -- | add the result to the test this one interrupted
    AfterBoostTest
  | AfterCustom Source Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RerollCost = FocusCost Skill | ClueCost | FreeReroll Source
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TestStep = DeterminePool | ManipulateDice | TestResolved
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Die = Die {value :: Int, removed :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data TestState = TestState
  { investigator :: InvestigatorId
  , skill :: Skill
  , modifier :: Int
  , kind :: TestKind
  , step :: TestStep
  , bonusDice :: Int
  , fixedPool :: Maybe Int
  {- ^ a pool the card states outright, which the skill and its modifier do not
  contribute to ("resolve a test using that number of dice")
  -}
  , chosenAssets :: [CardId]
  , dice :: [Die]
  , addedSuccesses :: Int
  , after :: AfterTest
  , casting :: Maybe CardId
  -- ^ the spell being cast, when this test is part of casting one
  , usedInTest :: [CardId]
  -- ^ cards whose once-per-test ability has been spent on this test
  , riders :: [(EffectCtx, Effect)]
  {- ^ what a card printed "after resolving the test" left behind, resolved once
  the test's own result has been (Grave Dirt's CURSED).
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncounterDeck
  = NeighborhoodDeck NeighborhoodId
  | StreetDeck
  | TravelRouteDeck
  | ThresholdDeck
  | MysteryDeck SpaceId
  | AnomalyDeck
  | TerrorDeck NeighborhoodId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EncounterState = EncounterState
  { investigator :: InvestigatorId
  , card :: CardId
  , deck :: EncounterDeck
  , gainedNeighborhoodClue :: Bool
  , returnToArchive :: Bool
  -- ^ set by a card printed "return this card to the archive" rather than to its deck
  , returnToTop :: Maybe Bool
  {- ^ set by a card printed "place this card on top of" its own deck, which is not
  where an encounter otherwise leaves it. Optional, so a table saved before it loads.
  -}
  , section :: Maybe (Int, Int)
  {- ^ for cards whose section the engine picks (street type, or the doom range on
  anomaly and terror cards): the printed section in use, counted from the top, and
  how many sections the card has
  -}
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data MoveState = MoveState
  { investigator :: InvestigatorId
  , remaining :: Int
  , paidSteps :: Int
  , maxPaidSteps :: Int
  , voluntary :: Bool
  , ignoringMonsters :: Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data GameStatus = InProgress | Won | Lost Text
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A pile the debug panel can deal the top card from.
data DebugDeck
  = DeckItem
  | DeckAlly
  | DeckSpell
  | DeckSpecial
  | DeckStarting
  | DeckCondition
  | DeckMonster
  | DeckHeadline
  | DeckStreet
  | DeckThreshold
  | DeckTravelRoute
  | DeckAnomaly
  | DeckNeighborhood NeighborhoodId
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DebugAction
  = DebugSetMoney InvestigatorId Int
  | DebugSetClues InvestigatorId Int
  | DebugSetRemnants InvestigatorId Int
  | DebugSetDamage InvestigatorId Int
  | DebugSetHorror InvestigatorId Int
  | DebugMoveInvestigator InvestigatorId SpaceId
  | DebugSetSpaceDoom SpaceId Int
  | DebugSetSheetDoom Int
  | DebugSetSheetClues Int
  | DebugSetSheetMarkers Int
  | -- | take one of the display's cards, for nothing
    DebugGainFromDisplay InvestigatorId CardId
  | DebugDiscardCard CardId
  | DebugAddToCodex ArchiveNumber
  | DebugResolveEffect InvestigatorId Effect
  | DebugDrawMythos InvestigatorId MythosToken
  | DebugSetDelayed InvestigatorId Bool
  | -- | hand over a condition by name, the way a card would (BLESSED, CURSED, ...)
    DebugGainCondition InvestigatorId ConditionName
  | DebugSetFocus InvestigatorId Skill Int
  | DebugDrawDeck InvestigatorId DebugDeck
  | DebugDrawCard InvestigatorId DebugDeck CardId
  | DebugSetMonsterDamage CardId Int
  | -- | defeats it the way a killing blow would, so whatever answers a defeat runs
    DebugDefeatMonster CardId
  | DebugSetDice [Int]
  | -- | successes added to the test in progress, on top of what the dice say
    DebugSetAddedSuccesses Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

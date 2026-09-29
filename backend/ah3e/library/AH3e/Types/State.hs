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
  | -- | they and this monster have just come apart
    AfterDisengage InvestigatorId CardId
  | -- | an action of theirs has finished, whichever it was
    AfterAnyAction InvestigatorId ActionKind
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
  | AtEndOfMonsterPhase InvestigatorId
  | -- | this many tokens have just gone into the mythos cup
    TokensReturnedToCup InvestigatorId Int
  | DrewBlankToken InvestigatorId
  | SpentFocusToReroll InvestigatorId
  | AfterCastSpell InvestigatorId CardId
  | -- | someone in this investigator's space has just recovered sanity
    AfterRecoverSanity InvestigatorId RecoverTarget
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
  AfterDisengage iid _ -> iid
  AfterAnyAction iid _ -> iid
  AfterSpendRemnant iid -> iid
  AfterEncounter iid -> iid
  AfterDefeatMonsterInAttack iid -> iid
  AfterDamageMonsterInAttack iid _ -> iid
  AfterMoveDistance iid _ -> iid
  AtStartOfTurn iid -> iid
  AtEndOfMonsterPhase iid -> iid
  TokensReturnedToCup iid _ -> iid
  DrewBlankToken iid -> iid
  SpentFocusToReroll iid -> iid
  AfterCastSpell iid _ -> iid
  AfterRecoverSanity iid _ -> iid

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

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
  | -- | the monster defeated is gone by now, so only the attacker is carried
    AfterDefeatMonsterInAttack InvestigatorId
  | DrewBlankToken InvestigatorId
  | SpentFocusToReroll InvestigatorId
  | AfterCastSpell InvestigatorId CardId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

triggerInvestigator :: Trigger -> InvestigatorId
triggerInvestigator = \case
  AnotherResolvesTest owner _ -> owner
  AfterGatherResources iid -> iid
  AfterResearchAction iid -> iid
  AfterDefeatMonsterInAttack iid -> iid
  DrewBlankToken iid -> iid
  SpentFocusToReroll iid -> iid
  AfterCastSpell iid _ -> iid

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
  , chosenAssets :: [CardId]
  , dice :: [Die]
  , addedSuccesses :: Int
  , after :: AfterTest
  , casting :: Maybe CardId
  -- ^ the spell being cast, when this test is part of casting one
  , usedInTest :: [CardId]
  -- ^ cards whose once-per-test ability has been spent on this test
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
  | DebugGainCard InvestigatorId CardCode
  | DebugDiscardCard CardId
  | DebugAddToCodex ArchiveNumber
  | DebugResolveEffect InvestigatorId Effect
  | DebugDrawMythos InvestigatorId MythosToken
  | DebugSetDelayed InvestigatorId Bool
  | DebugSetFocus InvestigatorId Skill Int
  | DebugDrawDeck InvestigatorId DebugDeck
  | DebugDrawCard InvestigatorId DebugDeck CardId
  | DebugSetDice [Int]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

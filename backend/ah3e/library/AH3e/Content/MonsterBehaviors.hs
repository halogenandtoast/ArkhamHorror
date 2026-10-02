{- | What the monster deck's own cards do. The named and epic monsters belong to
the scenarios that bring them; these are the ones anybody can draw.
-}
module AH3e.Content.MonsterBehaviors (behaviors) where

import AH3e.Content.Vocabulary (curioItem, spell)
import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.Effect
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #monsters
    .~ Map.fromList
      [ -- "After this monster attacks, ..."
        ("altered-beast", onAttack \_ iid -> pure [ResolveEffect (ctxFor iid) DiscardAFocus])
      , ("entranced-hybrid", onAttack \mid _ -> doomToTheSheet mid)
      , ("pale-lord", onAttack \mid _ -> doomWhereItStands mid)
      , ("prowling-abductor", onAttack \_ iid -> pure [ResolveEffect (ctxFor iid) BecomeDelayed])
      , ("hulking-thrall", onAttack \_ iid -> pure [ResolveEffect (ctxFor iid) cursedUnlessRemnant])
      , ("masked-ones", onAttack maskedOnes)
      , ("swooping-scavenger", onAttack swoopingScavenger)
      , -- "After you disengage this monster, ..."
        ("lodge-enforcer", onDisengage \_ iid -> pure [SufferHarm iid rules NormalHarm 0 1])
      , ("lodge-loyalist", onDisengage \mid _ -> doomWhereItStands mid)
      , -- "Reward -- After you defeat this monster as part of an attack action, ..."
        ("ghoul-priest", reward curioItem)
      , ("high-priest", reward spell)
      , ("void-touched", reward spell)
      , ("icebound-captive", reward (GainE (AnAlly Nothing)))
      , ("swift-byakhee", onDefeatBy swiftByakhee)
      , -- what defeating it costs
        ("enraged-dreamer", onDefeatBy \iid -> pure [SufferHarm iid rules NormalHarm 0 1])
      , ("hybrid-thug", onDefeatBy \iid -> pure [SufferHarm iid rules NormalHarm 0 1])
      , ("accursed-somnambulist", onDefeatBy (testAfterwards Will 0 "somnambulist"))
      , -- "As part of an evade action, you may spend one remnant to add one to a die."
        ("lupine-thrall", defaultMonsterBehavior & #testOptions .~ lupineThrall)
      , -- the rest, each its own shape
        ("ghoul-acolyte", defaultMonsterBehavior & #afterEvaded .~ \mid _ -> crawlToward mid)
      , ("terrified-wanderer", defaultMonsterBehavior & #afterEngaged .~ terrifiedWanderer)
      , ("declan-pearce", defaultMonsterBehavior & #afterAttackAction .~ declanPearce)
      , -- "You cannot evade or disengage this monster, and it cannot engage anyone else."
        ("grim-spectre", defaultMonsterBehavior & #holdsItsQuarry .~ True)
      , -- Secrets of the Order
        ("lodge-guardian", onDisengage \_ iid -> pure [GainConditionMsg iid "CURSED"])
      , ("lodge-seer", onDisengage \_ iid -> pure [doomAt iid TheUnstableSpace 2])
      , ("twilight-sentry", onDisengage \_ iid -> pure [SufferHarm iid rules NormalHarm 0 1])
      ,
        ( "taloned-cannibal"
        , defaultMonsterBehavior & #afterEngaged .~ \_ iid -> pure [SufferHarm iid rules NormalHarm 0 1]
        )
      ,
        ( "gluttonous-giant"
        , defaultMonsterBehavior & #afterDamagedInAttack .~ \_ iid -> pure [SufferHarm iid rules NormalHarm 0 1]
        )
      ,
        ( "menacing-bulk"
        , onAttack \_ iid -> pure [ResolveEffect (ctxFor iid) (fatiguedOr (SufferDamage (N 1)))]
        )
      ,
        ( "sanguinous-wraith"
        , onAttack \_ iid -> pure [ResolveEffect (ctxFor iid) (fatiguedOr (PlaceDoomAt YourSpace (N 1)))]
        )
      , ("tunneling-dhole", onAttack fleeToUnstable)
      , ("confounding-specter", onAttack confoundingSpecter)
      ,
        ( "crashing-specter"
        , onAttack (\mid _ -> pure [bite mid]) & #afterExhausted .~ \mid -> pure [bite mid]
        )
      , ("screaming-haunt", defaultMonsterBehavior & #afterEngaged .~ screamingHaunt)
      , ("cacophonous-haunt", defaultMonsterBehavior & #afterEngaged .~ cacophonousHaunt)
      , -- the two Shrouded cards whose engaged face is not a monster at all
        ("weeping-haunt", becomes (`GainNamedCard` "Weeping Haunt"))
      , ("commanding-specter", becomes (`GainConditionMsg` "COMMANDING SPECTER"))
      ]
    & #assets
    .~ Map.fromList
      [ ("weeping-haunt-ally", weepingHauntAlly)
      , ("commanding-specter-condition", commandingSpecterCondition)
      ]
    & #customAfterTests
    .~ Map.fromList
      [ ("somnambulist", \src r -> whenFailed src r \iid -> [GainConditionMsg iid "CURSED"])
      , ("terrified-wanderer", terrifiedWandererResult)
      ]

rules :: Source
rules = SourceRules

ctxFor :: InvestigatorId -> EffectCtx
ctxFor iid = EffectCtx {investigator = iid, source = rules, testResult = Nothing}

onAttack :: (CardId -> InvestigatorId -> GameM [Message]) -> MonsterBehavior
onAttack f = defaultMonsterBehavior & #afterAttack .~ f

onDisengage :: (CardId -> InvestigatorId -> GameM [Message]) -> MonsterBehavior
onDisengage f = defaultMonsterBehavior & #afterDisengage .~ f

{- | "Reward -- After you defeat this monster as part of an attack action, you
gain X." An investigator as the source is what makes it an attack rather than a
card going off at it.
-}
reward :: Effect -> MonsterBehavior
reward gain = onDefeatBy \iid -> pure [ResolveEffect (ctxFor iid) gain]

onDefeatBy :: (InvestigatorId -> GameM [Message]) -> MonsterBehavior
onDefeatBy f =
  defaultMonsterBehavior
    & #afterDefeated
    .~ \_ src -> case src of
      SourceInvestigator iid -> f iid
      _ -> pure []

-- | "you become CURSED unless you spend one remnant"
cursedUnlessRemnant :: Effect
cursedUnlessRemnant = MayPay (SpendRemnants 1) NoEffect (GainE (Condition "CURSED"))

-- | "move one doom from its space to the scenario sheet"
doomToTheSheet :: CardId -> GameM [Message]
doomToTheSheet mid = do
  m <- getMonster mid
  board <- use #board
  let here = maybe 0 (.doom) (Map.lookup m.space board.spaces)
  pure [msg | here > 0, msg <- [RemoveDoom m.space 1, PlaceDoomOnSheet 1]]

-- | "place one doom in its space"
doomWhereItStands :: CardId -> GameM [Message]
doomWhereItStands mid = do
  m <- getMonster mid
  pure [PlaceDoom rules m.space]

-- | "you suffer two damage unless you place one doom in your space"
maskedOnes :: CardId -> InvestigatorId -> GameM [Message]
maskedOnes _ iid = do
  here <- investigatorSpace iid
  for_ here \sid ->
    chooseFor
      iid
      "The masked ones close in"
      [ label "Place one doom in your space" [PlaceDoom rules sid]
      , label "Suffer two damage" [SufferHarm iid rules NormalHarm 2 0]
      ]
  pure []

{- | "you disengage other monsters and both you and it move directly to the space
with the most doom"
-}
swoopingScavenger :: CardId -> InvestigatorId -> GameM [Message]
swoopingScavenger mid iid = do
  board <- use #board
  others <- filter ((/= mid) . (.card)) <$> engagedMonsters iid
  pure case sortOn (negate . (.doom)) (Map.elems board.spaces) of
    worst : _ ->
      [DisengageMonster iid o.card | o <- others]
        <> [MoveDirectly iid worst.id, MoveMonsterTo mid worst.id]
    [] -> []

-- | "you may disengage all monsters and move up to three spaces"
swiftByakhee :: InvestigatorId -> GameM [Message]
swiftByakhee iid = do
  ms <- engagedMonsters iid
  chooseFor
    iid
    "Break away on the byakhee's wings?"
    [ label
        "Disengage all monsters and move up to three spaces"
        ([DisengageMonster iid m.card | m <- ms] <> [ResolveEffect (ctxFor iid) (MoveUpTo 3)])
    , label "Stay where you are" []
    ]
  pure []

-- | "After you defeat it, test X. If you fail, ..."
testAfterwards :: Skill -> Int -> Text -> InvestigatorId -> GameM [Message]
testAfterwards skill modifier key iid =
  pure [BeginTest (newTest iid skill modifier OtherTest (AfterCustom (SourceInvestigator iid) key))]

whenFailed :: Source -> Int -> (InvestigatorId -> [Message]) -> GameM ()
whenFailed src result f = when (result <= 0) case src of
  SourceInvestigator iid -> pushAll (f iid)
  _ -> pure ()

-- | "it moves one space toward the unstable space"
crawlToward :: CardId -> GameM [Message]
crawlToward mid = pure [MonsterStep mid 1 (TowardSpaces UnstableSpace)]

{- | "After this monster becomes engaged with you, test influence -1. If you pass,
defeat it. If you fail, you suffer one damage."
-}
terrifiedWanderer :: CardId -> InvestigatorId -> GameM [Message]
terrifiedWanderer mid iid =
  pure
    [ BeginTest
        (newTest iid Influence (-1) OtherTest (AfterCustom (SourceMonster mid) "terrified-wanderer"))
    ]

terrifiedWandererResult :: Source -> Int -> GameM ()
terrifiedWandererResult src result = case src of
  SourceMonster mid -> do
    here <- uses #monsters (Map.member mid)
    when here
      $ if result > 0
        then push (DefeatMonster mid rules)
        else do
          m <- getMonster mid
          case m.state of
            Engaged (iid : _) -> push (SufferHarm iid rules NormalHarm 1 0)
            _ -> pure ()
  _ -> pure ()

-- | "After you perform an attack action, Declan disengages you unless you spend one focus."
declanPearce :: CardId -> InvestigatorId -> Bool -> GameM [Message]
declanPearce mid iid _ = do
  chooseFor
    iid
    "Declan slips away"
    [ label "Spend one focus to hold him" [ResolveEffect (ctxFor iid) (Pay (SpendFocus 1) NoEffect)]
    , label "Let him go" [DisengageMonster iid mid]
    ]
  pure []

{- | "As part of an evade action, you may spend one remnant to add one to the
result of one die." Its own ferocity is what the remnant buys you against.
-}
lupineThrall :: CardId -> InvestigatorId -> TestState -> GameM [Reaction]
lupineThrall mid iid ts = do
  i <- getInvestigator iid
  pure
    [ Reaction
        { key = "lupine-thrall"
        , label = "Spend a remnant to add one to a die"
        , messages = [PayCost (ctxFor iid) (SpendRemnants 1), AddToDie (SourceMonster mid)]
        }
    | isEvade ts.kind
    , i.remnants > 0
    , liveDiceCount ts > 0
    ]
 where
  isEvade = \case ActionTest EvadeAction _ -> True; _ -> False

-- Secrets of the Order --------------------------------------------------------

-- | "Place N doom at <where>", for a monster that puts it down as it lets go.
doomAt :: InvestigatorId -> Where -> Int -> Message
doomAt iid w n = ResolveEffect (ctxFor iid) (PlaceDoomAt w (N n))

-- | "Become FATIGUED. If you cannot, <this instead>."
fatiguedOr :: Effect -> Effect
fatiguedOr instead =
  If (CanGainCondition "FATIGUED") (GainE (Condition "FATIGUED")) instead

-- | "It suffers one damage", dealt by the rules rather than by anyone.
bite :: CardId -> Message
bite mid = DealMonsterDamage mid rules 1

{- | A Shrouded card whose engaged face is an ally or a condition: engaging it hands
that card over instead, and the monster card leaves the game rather than going back
to the monster deck, since the physical card is now in front of its new owner.
-}
becomes :: (InvestigatorId -> Message) -> MonsterBehavior
becomes handOver =
  defaultMonsterBehavior
    & #removedWhenDefeated
    .~ True
    & #insteadOfEngaging
    .~ \mid iid -> pure (Just [handOver iid, DiscardMonster mid])

{- | "It disengages all investigators and moves directly to the unstable space."
Which unstable space is only a choice when the event discard names more than one.
-}
fleeToUnstable :: CardId -> InvestigatorId -> GameM [Message]
fleeToUnstable mid iid = do
  m <- getMonster mid
  targets <- unstableSpaces
  let letGo = [DisengageMonster who mid | who <- holdersOf m]
  case targets of
    [sid] -> pure (letGo <> [MoveMonsterTo mid sid])
    _ -> do
      chooseFor iid "Choose the unstable space it flees to"
        $ [Choice (SpaceLabel sid) (letGo <> [MoveMonsterTo mid sid]) | sid <- targets]
      pure []

holdersOf :: Monster -> [InvestigatorId]
holdersOf m = case m.state of Engaged is -> is; _ -> []

{- | "Disengage all monsters and move directly to the unstable space. Then become
delayed."
-}
confoundingSpecter :: CardId -> InvestigatorId -> GameM [Message]
confoundingSpecter _ iid = do
  ms <- engagedMonsters iid
  pure
    $ [DisengageMonster iid m.card | m <- ms]
    <> [ ResolveEffect (ctxFor iid) (MoveDirectlyTo TheUnstableSpace)
       , ResolveEffect (ctxFor iid) BecomeDelayed
       ]

-- | "Place two doom in your space unless you become CURSED."
screamingHaunt :: CardId -> InvestigatorId -> GameM [Message]
screamingHaunt _ iid = do
  chooseFor
    iid
    "The haunt screams"
    [ label "Become CURSED" [GainConditionMsg iid "CURSED"]
    , label "Place two doom in your space" [doomAt iid YourSpace 2]
    ]
  pure []

-- | "You may place one doom in the unstable space to defeat this monster."
cacophonousHaunt :: CardId -> InvestigatorId -> GameM [Message]
cacophonousHaunt mid iid = do
  chooseFor
    iid
    "Send the haunt away?"
    [ label
        "Place one doom in the unstable space to be rid of it"
        [doomAt iid TheUnstableSpace 1, DefeatMonster mid rules]
    , label "Leave it" []
    ]
  pure []

{- | "After you assign one or more horror to this ally, remove one doom from your
space." Stated flatly, so it is not offered; it answers even when that horror was
its second and discarded it.
-}
weepingHauntAlly :: AssetBehavior
weepingHauntAlly =
  defaultAssetBehavior
    & #afterHarm
    .~ \cid iid plan ->
      pure
        [ ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (RemoveDoomFrom YourSpace (N 1))
        | Just (self, k) <- [plan.horrorTo]
        , self == cid
        , k > 0
        ]

{- | "At the end of your turn, place one doom in your space. Action: Place one doom
in your space and become FATIGUED to discard this card."
-}
commandingSpecterCondition :: AssetBehavior
commandingSpecterCondition =
  cardAction
    "Commanding Specter: place one doom and become FATIGUED to be rid of it"
    (Seq [PlaceDoomAt YourSpace (N 1), GainE (Condition "FATIGUED"), Custom "discard-source"])
    & #atEndOfOwnerTurn
    .~ \cid iid -> pure [ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (PlaceDoomAt YourSpace (N 1))]

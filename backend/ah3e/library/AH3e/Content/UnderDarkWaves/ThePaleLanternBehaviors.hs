-- | The Pale Lantern's own mechanics: the Club, the lantern, and the man behind it.
module AH3e.Content.UnderDarkWaves.ThePaleLanternBehaviors (behaviors) where

import AH3e.Content.Tiles (spaceIdFor)
import AH3e.Content.UnderDarkWaves.Codex
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
import Data.Text qualified as T

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (76, aMissingClient)
      , (77, infiltration)
      , (78, interrogation)
      , (79, theSociety)
      , (80, findTheTrail)
      , (81, findTheTruth)
      , (82, stealTheLantern)
      , (83, sunderTheLantern)
      , (84, theLostOne)
      , (85, creepingThreat)
      , (86, waningHope)
      , (87, aStormRages)
      , (88, theAidOfNodens)
      , (90, thePaleLantern)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("lantern-club-reckoning", const (everyoneTests Will 0 "lantern-doom"))
      , ("lantern-watch-reckoning", const (everyoneTests Observation 0 "lantern-doom"))
      , ("lantern-masters-reckoning", const (everyoneTests Lore 0 "lantern-doom"))
      , ("lantern-dues-reckoning", const (everyoneTests Will (-1) "lantern-dues"))
      , ("lantern-returns", bloodlessManReturns)
      , ("lantern-hunts", bloodlessManHunts)
      , ("lantern-storm-fades", stormFades)
      , ("lantern-nodens", aidOfNodens)
      , ("lantern-nodens-aid", nodensAid)
      , ("lantern-garner", garnerAnInvitation)
      , ("lantern-corner", cornerAnOfficer)
      , ("lantern-secrets", societySecrets)
      , ("lantern-turn-marker", turnAMarker)
      , ("lantern-storm", stormTest)
      , ("lantern-beacon", beaconTest)
      ]
    & #customPredicates
    .~ Map.fromList [("lantern-at-unstable", atUnstable)]
    & #customAfterTests
    .~ Map.fromList
      [ ("lantern-doom", doomWhereTheyStand)
      , ("lantern-dues", lanternDues)
      , ("infiltration", \_ r -> when (r >= 6) (push (FlipCodexCard 77)))
      , ("interrogation", interrogationResult)
      , ("steal-the-lantern", \_ r -> when (r >= 4) (push (FlipCodexCard 82)))
      , ("sunder", sunderResult)
      , ("storm-doom", stormDoom)
      , ("storm-beacon", stormBeacon)
      ]

bloodlessMan :: CardCode
bloodlessMan = "archive-89"

hallSchool :: SpaceId
hallSchool = spaceIdFor "Hall School"

{- | Card 76. Two clues in and the client's disappearance is worth chasing; the
investigators pick how, and the Society starts noticing either way.
-}
aMissingClient :: CodexBehavior
aMissingClient =
  defaultCodexBehavior
    { triggers =
        [ flipOnSheetClues "missing-client" 2 76
        , CodexTrigger
            { key = "missing-client-late"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 4)
            , action = \_ -> pushAll [AddArchiveToCodexFlipped 84, RemoveCodexCard 76]
            }
        ]
    , onFlip = \e -> when e.flipped do
        chooseGroup
          "How will you go after them?"
          [ Choice (TextLabel "Pose as a wealthy new member of the Club") [AddArchiveToCodex 77]
          , Choice (TextLabel "Find and interrogate one of the Society's officers") [AddArchiveToCodex 78]
          ]
        pushAll [AddArchiveToCodexFlipped 84, RemoveCodexCard 76]
    }

{- | Card 77. An invitation bought with enough money, and then the dues: the Club
asks something of its members every round.
-}
infiltration :: CodexBehavior
infiltration =
  defaultCodexBehavior
    { spaceEncounter = \e sid ->
        if e.flipped || sid /= hallSchool
          then Nothing
          else Just (Custom "lantern-garner")
    , componentActions =
        [ ComponentActionDef
            { label = "Spend the Club's secrets"
            , allowedWhileEngaged = False
            , canPerform = \_ -> do
                clues <- use #sheetClues
                flipped <- uses #codex (any (\e -> e.number == 77 && e.flipped))
                pure (flipped && clues >= 2)
            , perform = \_ ->
                pushAll
                  [ SpendSheetClues 2
                  , GateBurst
                  , AddArchiveToCodex 79
                  , AddArchiveToCodex 82
                  , RemoveCodexCard 77
                  ]
            }
        ]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "lantern-club-reckoning") else Nothing

{- | Card 78. The other way in: corner an officer at the unstable space, and keep
him alive long enough to talk.
-}
interrogation :: CodexBehavior
interrogation =
  defaultCodexBehavior
    { spaceEncounter = \e _ ->
        if e.flipped
          then Nothing
          else Just (If (CustomPredicate "lantern-at-unstable") (Custom "lantern-corner") NoEffect)
    , onFlip = \e -> when e.flipped do
        here <- unstableSpaces
        for_ (take 1 here) (spawnHeldBack "declan-pearce" >=> pushAll)
    , afterMonsterDefeated = \e mid _ ->
        if not e.flipped
          then pure []
          else do
            code <- cardCode mid
            pure
              [ msg
              | code == "declan-pearce"
              , msg <- [AddArchiveToCodexFlipped 79, AddArchiveToCodex 82, RemoveCodexCard 78]
              ]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "lantern-watch-reckoning") else Nothing

interrogationResult :: Source -> Int -> GameM ()
interrogationResult _ result = when (result > 0) do
  clues <- use #sheetClues
  when (clues >= 2) (pushAll [SpendSheetClues 2, FlipCodexCard 78])

{- | Card 79. Whichever way they got in, the Society is now something they can
read -- and something that reads them back every round.
-}
theSociety :: CodexBehavior
theSociety =
  defaultCodexBehavior
    { -- "Society Secrets" is the back: an encounter that buys a space back from the doom
      spaceEncounter = \e _ -> if e.flipped then Just (Custom "lantern-secrets") else Nothing
    , -- "Inner Workings" is the front: the Club's own way of reading a ward
      wardSkills = \e -> pure [Influence | not e.flipped]
    }
    & #reckoning
    .~ \e -> Just (Custom (if e.flipped then "lantern-watch-reckoning" else "lantern-dues-reckoning"))

-- | 79's front: "Each investigator that fails gains $1 and places one doom in their space."
lanternDues :: Source -> Int -> GameM ()
lanternDues src result = when (result <= 0) case src of
  SourceInvestigator iid -> do
    addMoney iid 1
    investigatorSpace iid >>= traverse_ (push . PlaceDoom src)
  _ -> pure ()

{- | Card 80. Four clues spent at the unstable space buys the trail: five markers
go down, one of them pointing at the truth.
-}
findTheTrail :: CodexBehavior
findTheTrail =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Spend four clues to find the trail"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                clues <- use #sheetClues
                here <- investigatorSpace iid
                unstable <- unstableSpaces
                pure (clues >= 4 && maybe False (`elem` unstable) here)
            , perform = \_ -> pushAll [SpendSheetClues 4, FlipCodexCard 80]
            }
        ]
    , onFlip = \e -> when e.flipped do
        board <- use #board
        colours <- shuffle (["blue", "green"] <> replicate 3 "white")
        hoods <- byDoom
        let worst n = listToMaybe (sortOn (negate . doomInSpace board) n.spaces)
        pushAll
          [ PlaceMarkerFacedown sid colour
          | (n, colour) <- zip hoods colours
          , Just sid <- [worst n]
          ]
        pushAll [AddArchiveToCodex 81, RemoveCodexCard 80]
    }

doomInSpace :: Board -> SpaceId -> Int
doomInSpace board sid = maybe 0 (.doom) (Map.lookup sid board.spaces)

{- | Card 81. Each marker turned over is a different kind of nothing, until the
blue one, which is the Club caught in the act.
-}
findTheTruth :: CodexBehavior
findTheTruth =
  defaultCodexBehavior
    { spaceEncounter = \e _ -> if e.flipped then Nothing else Just (Custom "lantern-turn-marker")
    , onFlip = \e -> when e.flipped do
        #board . #spaces . traversed . #markers %= filter (.faceUp)
        invs <- playingInvestigators
        for_ (take 1 invs) \i ->
          for_ i.space \sid -> pushAll [SpawnMonsterAt (Just sid) False, AddArchiveToCodex 82]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "lantern-masters-reckoning") else Nothing

{- | Card 82. The lantern itself, taken off the Hall School's wall with everything
the investigators can throw at the attempt.
-}
stealTheLantern :: CodexBehavior
stealTheLantern =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Steal the lantern"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                here <- investigatorSpace iid
                pure (here == Just hallSchool)
            , perform = \ctx -> do
                clues <- use #sheetClues
                chooseFor ctx.investigator "Spend clues from the scenario sheet for successes"
                  $ [ Choice
                        (AmountLabel k)
                        ([SpendSheetClues k | k > 0] <> [BeginTest (attempt ctx k)])
                    | k <- [0 .. clues]
                    ]
            }
        ]
    , onFlip = \e -> when e.flipped do
        invs <- playingInvestigators
        let atSchool = [i | i <- invs, i.space == Just hallSchool]
        arrival <- spawnHeldBack bloodlessMan hallSchool
        pushAll
          $ [AddArchiveToCodex 90]
          <> [GainNamedCard i.id "The Pale Lantern" | i <- take 1 atSchool]
          <> arrival
          <> [AddArchiveToCodex 83, RemoveCodexCard 82]
    }
 where
  attempt ctx k =
    ( newTest
        ctx.investigator
        Observation
        (-1)
        OtherTest
        (AfterCustom (SourceCodex 82) "steal-the-lantern")
    )
      { addedSuccesses = k
      }

{- | Card 83. The lantern is only worth anything filled: clues moved onto it one
test at a time, while its owner is still walking around.
-}
sunderTheLantern :: CodexBehavior
sunderTheLantern =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Pour a clue into the lantern"
            , allowedWhileEngaged = False
            , canPerform = \_ -> (>= 1) <$> use #sheetClues
            , perform = \ctx -> do
                flipped <- uses #codex (any (\e -> e.number == 83 && e.flipped))
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          (if flipped then 0 else -2)
                          OtherTest
                          (AfterCustom (SourceCodex 83) "sunder")
                      )
                  )
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "bloodless-man-down"
            , once = True
            , condition = \e -> do
                up <- inPlay bloodlessMan
                pure (not e.flipped && not up)
            , action = \_ -> push (FlipCodexCard 83)
            }
        , CodexTrigger
            { key = "lantern-full"
            , once = True
            , condition = \e -> do
                held <- tokensOn "clue" 90
                pure (e.flipped && held >= 3)
            , action = \_ -> push (FlipCodexCard 90)
            }
        ]
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "lantern-returns") else Nothing

sunderResult :: Source -> Int -> GameM ()
sunderResult _ result = when (result > 0) do
  clues <- use #sheetClues
  when (clues >= 1) do
    push (SpendSheetClues 1)
    markCard "clue" 90 1
    logText "A clue is poured into the Pale Lantern"

-- | 83's back: "Take card 89 and spawn it in the unstable space."
bloodlessManReturns :: EffectCtx -> GameM ()
bloodlessManReturns _ = do
  here <- unstableSpaces
  arrival <- case here of
    sid : _ -> spawnHeldBack bloodlessMan sid
    [] -> pure []
  pushAll (arrival <> [PlaceDoomOnSheet 1, FlipCodexCard 83])

{- | Card 84. Whoever they were hired to find is still out there, and the longer
it takes the less there is to find.
-}
theLostOne :: CodexBehavior
theLostOne =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "lost-one-trail"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 2)
            , action = \_ -> pushAll [FlipCodexCard 84, AddArchiveToCodex 80]
            }
        , CodexTrigger
            { key = "lost-one-late"
            , once = True
            , condition = \_ -> do
                doom <- use #sheetDoom
                pure (doom >= 8)
            , action = \_ -> pushAll [AddArchiveToCodex 85, RemoveCodexCard 84]
            }
        ]
    , onFlip = \e -> when e.flipped do
        later <- uses #codex (any ((`elem` [85, 86]) . (.number)))
        if later
          then push (RemoveCodexCard 84)
          else do
            invs <- playingInvestigators
            pushAll [ResolveEffect (lanternCtx 84 i.id) (Focus Nothing True) | i <- invs]
    }

lanternCtx :: ArchiveNumber -> InvestigatorId -> EffectCtx
lanternCtx n iid = EffectCtx {investigator = iid, source = SourceCodex n, testResult = Nothing}

{- | Card 85. The Club stops hiding. Everything the investigators had built is
swept off the table and the Bloodless Man comes for whoever holds the lantern.
-}
creepingThreat :: CodexBehavior
creepingThreat =
  defaultCodexBehavior
    { onAdd = \_ -> do
        returnTokensToCup [SpreadDoomToken, SpreadDoomToken]
        logText "Two spread doom tokens join the mythos cup"
    , triggers = [flipOnSheetDoom "creeping-threat" 12 85]
    , onFlip = \e -> when e.flipped do
        held <- tokensOn "clue" 90
        arrival <- bringHimTo hallSchool
        ms <- uses #monsters Map.elems
        lesser <- filterM (fmap (/= bloodlessMan) . cardCode . (.card)) ms
        damage <- damageHim (4 * held)
        pushAll
          $ arrival
          <> [DefeatMonster m.card (SourceCodex 85) | m <- lesser]
          <> [RemoveCodexCard k | k <- [77, 78, 79, 80, 81, 82, 83, 84]]
          <> damage
          <> [RemoveCodexCard 90, AddArchiveToCodex 86, RemoveCodexCard 85]
    }

-- | "spawn it at X. If it is already in play, it moves directly to the lantern."
bringHimTo :: SpaceId -> GameM [Message]
bringHimTo sid = do
  up <- inPlay bloodlessMan
  if up
    then do
      ms <- uses #monsters Map.elems
      him <- filterM (fmap (== bloodlessMan) . cardCode . (.card)) ms
      invs <- playingInvestigators
      holder <- filterM (fmap (elem "The Pale Lantern") . traverse cardName . (.assets)) invs
      pure [MoveMonsterTo m.card there | m <- take 1 him, i <- take 1 holder, Just there <- [i.space]]
    else spawnHeldBack bloodlessMan sid

cardName :: CardId -> GameM Text
cardName cid = (.name) <$> getCardDef cid

damageHim :: Int -> GameM [Message]
damageHim n = do
  ms <- uses #monsters Map.elems
  him <- filterM (fmap (== bloodlessMan) . cardCode . (.card)) ms
  pure [DealMonsterDamage m.card (SourceCodex 85) n | n > 0, m <- take 1 him]

{- | Card 86. Nothing left but the man himself: put him down, or watch him walk
out with everyone.
-}
waningHope :: CodexBehavior
waningHope =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "waning-hope-won"
            , once = True
            , condition = \e -> do
                up <- inPlay bloodlessMan
                pure (not e.flipped && not up)
            , action = \_ -> push (FlipCodexCard 86)
            }
        , CodexTrigger
            { key = "waning-hope-lost"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 16)
            , action = \_ -> push (LoseTheGame "Taken away")
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }
    & #reckoning
    .~ \e -> if e.flipped then Nothing else Just (Custom "lantern-hunts")

-- | 86's reckoning: he takes his toll and then withdraws to the unstable space.
bloodlessManHunts :: EffectCtx -> GameM ()
bloodlessManHunts _ = do
  board <- use #board
  ms <- uses #monsters Map.elems
  him <- filterM (fmap (== bloodlessMan) . cardCode . (.card)) ms
  for_ (take 1 him) \m -> do
    invs <- playingInvestigators
    let hood = spaceNeighborhood m.space board
        near = [i | i <- invs, maybe False (\sid -> spaceNeighborhood sid board == hood) i.space]
    pushAll [SufferHarm i.id (SourceCodex 86) DirectHarm 1 1 | i <- near]
    here <- unstableSpaces
    clues <- use #sheetClues
    for_ (take 1 here) \sid ->
      if clues >= 1
        then
          chooseGroup
            "The Bloodless Man withdraws to the unstable space"
            [ Choice (TextLabel "Spend a clue from the scenario sheet to hold him") [SpendSheetClues 1]
            , Choice (TextLabel "Let him go") [MoveMonsterTo m.card sid]
            ]
        else push (MoveMonsterTo m.card sid)

{- | Card 87. The storm over the Strange High House, which the investigators can
turn from something counting down into something they can use.
-}
aStormRages :: CodexBehavior
aStormRages =
  defaultCodexBehavior
    { onAdd = \_ -> markCard "doom" 87 3
    , spaceEncounter = \e sid ->
        if sid /= spaceIdFor "Strange High House"
          then Nothing
          else Just (Custom (if e.flipped then "lantern-beacon" else "lantern-storm"))
    , triggers =
        [ CodexTrigger
            { key = "storm-beacon-ready"
            , once = True
            , condition = \e -> do
                focus <- focusKindsOn 87
                pure (e.flipped && focus >= 3)
            , action = \_ -> pushAll [AddArchiveToCodex 88, RemoveCodexCard 87]
            }
        ]
    , onFlip = \e -> when e.flipped do
        held <- tokensOn "clue" 87
        markCard "clue" 87 (negate held)
        logText "The storm breaks over the Strange High House"
    }
    & #reckoning
    .~ \e -> if e.flipped then Nothing else Just (Custom "lantern-storm-fades")

focusKindsOn :: ArchiveNumber -> GameM Int
focusKindsOn n = do
  codex <- use #codex
  let toks = concat [Map.keys e.tokens | e <- codex, e.number == n]
  pure (length [k | k <- toks, T.isPrefixOf "focus:" k])

-- | 87's reckoning: "Remove one doom from this card. If there is none, flip it."
stormFades :: EffectCtx -> GameM ()
stormFades _ = do
  markCard "doom" 87 (-1)
  left <- tokensOn "doom" 87
  when (left <= 0) (push (FlipCodexCard 87))

-- | 87's back: a passed test at the house leaves a focus of that kind on the card.
stormBeacon :: Source -> Int -> GameM ()
stormBeacon src result = when (result > 0) case src of
  SourceInvestigator iid -> do
    i <- getInvestigator iid
    let kinds = [s | (s, k) <- Map.toList i.focus, k > 0]
    unless (null kinds)
      $ chooseFor
        iid
        "Set a focus on the beacon"
        [label (tshow s) [MarkCodexToken 87 ("focus:" <> tshow s) 1] | s <- kinds]
  _ -> pure ()

{- | Card 88. What the beacon called: either one of them walks away from all of
it, or Nodens holds the dark off at a price.
-}
theAidOfNodens :: CodexBehavior
theAidOfNodens =
  defaultCodexBehavior
    { onAdd = \_ -> do
        invs <- playingInvestigators
        pushAll [AddSheetClues 2]
        chooseGroup
          "What does the beacon buy?"
          ( Choice (TextLabel "Call on Nodens") [FlipCodexCard 88]
              : [ Choice
                    (InvestigatorLabel i.id)
                    ( RetireInvestigator i.id
                        : [GainConditionMsg o.id "BLESSED" | o <- invs, o.id /= i.id]
                          <> [RemoveCodexCard 88]
                    )
                | i <- invs
                ]
          )
    }
    & #reckoning
    .~ \e -> if e.flipped then Just (Custom "lantern-nodens") else Nothing

-- | 88's back: horror traded for a space swept clean and the cup emptied of blanks.
aidOfNodens :: EffectCtx -> GameM ()
aidOfNodens _ = do
  paid <- tokensOn "horror" 88
  board <- use #board
  invs <- playingInvestigators
  let doomed = [s.id | s <- Map.elems board.spaces, s.doom > 0]
  unless (null doomed || null invs)
    $ chooseGroup
      ("Suffer " <> tshow paid <> " horror to hold the dark off?")
      ( Choice (TextLabel "Let it be") []
          : [ Choice
                (InvestigatorLabel i.id)
                [ SufferHarm i.id (SourceCodex 88) DirectHarm 0 paid
                , ResolveEffect (lanternCtx 88 i.id) (Custom "lantern-nodens-aid")
                ]
            | i <- invs
            ]
      )

{- | Card 90, the lantern itself. The artifact is held by an investigator; the
codex card beside it is what the clues poured into it are counted on, and its
back is the end of the scenario.
-}
thePaleLantern :: CodexBehavior
thePaleLantern =
  defaultCodexBehavior
    { onFlip = \e -> when e.flipped (push WinTheGame)
    }

-- | 77's encounter: "You attempt to garner an invitation to the Club."
garnerAnInvitation :: EffectCtx -> GameM ()
garnerAnInvitation ctx = do
  i <- getInvestigator ctx.investigator
  chooseFor ctx.investigator "Spend money for successes"
    $ [ Choice (AmountLabel k) ([PayCost ctx (SpendMoney k) | k > 0] <> [BeginTest (bid k)])
      | k <- [0 .. i.money]
      ]
 where
  bid k =
    (newTest ctx.investigator Influence 0 OtherTest (AfterCustom (SourceCodex 77) "infiltration"))
      { addedSuccesses = k
      }

-- | 78's encounter: "You may spread doom once to test observation -1."
cornerAnOfficer :: EffectCtx -> GameM ()
cornerAnOfficer ctx =
  chooseFor
    ctx.investigator
    "Spread doom to corner the officer?"
    [ label
        "Spread doom once"
        [ ResolveEffect ctx SpreadDoomOnce
        , BeginTest
            (newTest ctx.investigator Observation (-1) OtherTest (AfterCustom (SourceCodex 78) "interrogation"))
        ]
    , label "Leave him" []
    ]

atUnstable :: EffectCtx -> GameM Bool
atUnstable ctx = do
  here <- investigatorSpace ctx.investigator
  unstable <- unstableSpaces
  pure (maybe False (`elem` unstable) here)

-- | 79's back: "You may remove one doom from any space in your neighborhood."
societySecrets :: EffectCtx -> GameM ()
societySecrets ctx = do
  board <- use #board
  mnid <- investigatorNeighborhood ctx.investigator
  let spaces =
        [ sid
        | Just nid <- [mnid]
        , Just n <- [Map.lookup nid board.neighborhoods]
        , sid <- n.spaces
        , doomInSpace board sid > 0
        ]
  unless (null spaces)
    $ chooseFor ctx.investigator "Remove one doom from a space in your neighborhood"
    $ label "Leave it" []
    : [Choice (SpaceLabel sid) [RemoveDoom sid 1] | sid <- spaces]

{- | 81's encounter: a marker turned over is white (nothing), green (manifests) or
blue (the Club caught in the act).
-}
turnAMarker :: EffectCtx -> GameM ()
turnAMarker ctx = do
  here <- investigatorSpace ctx.investigator
  board <- use #board
  for_ here \sid -> do
    let hidden = [m | Just s <- [Map.lookup sid board.spaces], m <- s.markers, not m.faceUp]
    for_ (take 1 hidden) \m -> do
      spaceL sid . #markers %= dropHidden
      logText ("The marker is a " <> m.color <> " one")
      case m.color of
        "blue" -> push (FlipCodexCard 81)
        "green" -> push (ResolveEffect ctx (DrawMythosTokens 2))
        _ -> pure ()

dropHidden :: [Marker] -> [Marker]
dropHidden ms = case span (.faceUp) ms of
  (before, _ : after) -> before <> after
  _ -> ms

-- | 87's front: a passed test at the house turns one of its doom into a clue.
stormTest :: EffectCtx -> GameM ()
stormTest ctx =
  push
    ( BeginTest
        ( newTest
            ctx.investigator
            Will
            0
            OtherTest
            (AfterCustom (SourceInvestigator ctx.investigator) "storm-doom")
        )
    )

-- | 87's front: a passed test turns one of the storm's doom into a clue.
stormDoom :: Source -> Int -> GameM ()
stormDoom _ result = when (result > 0) do
  left <- tokensOn "doom" 87
  when (left > 0) do
    markCard "doom" 87 (-1)
    markCard "clue" 87 1
    logText "The storm gives a little ground"

-- | 87's back: a passed test leaves a focus of the investigator's choosing.
beaconTest :: EffectCtx -> GameM ()
beaconTest ctx =
  chooseFor
    ctx.investigator
    "Test a skill for the beacon"
    [ label
        (tshow s)
        [ BeginTest
            ( newTest
                ctx.investigator
                s
                0
                OtherTest
                (AfterCustom (SourceInvestigator ctx.investigator) "storm-beacon")
            )
        ]
    | s <- [minBound .. maxBound]
    ]

-- | 88's back: "remove all doom from any space and return all blank tokens."
nodensAid :: EffectCtx -> GameM ()
nodensAid _ = do
  board <- use #board
  let doomed = [s.id | s <- Map.elems board.spaces, s.doom > 0]
  unless (null doomed)
    $ chooseGroup
      "Remove all doom from any space"
      [Choice (SpaceLabel sid) [RemoveDoom sid (doomInSpace board sid)] | sid <- doomed]
  blanks <- uses #drawnTokens (filter (== BlankToken))
  #drawnTokens %= filter (/= BlankToken)
  returnTokensToCup blanks
  push (MarkCodexToken 88 "horror" 1)

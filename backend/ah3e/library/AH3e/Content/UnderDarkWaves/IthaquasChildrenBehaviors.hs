-- | Ithaqua's Children's own mechanics: its reckoning, and its codex cards.
module AH3e.Content.UnderDarkWaves.IthaquasChildrenBehaviors (behaviors) where

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
import Data.List (nub)
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      ( [ (91, theHuntersKeen)
        , (92, friendOrFoe)
        , (93, bentToHisWill)
        , (94, findTheHeart)
        , (95, counteredMagic)
        , (99, vanquishIthaqua)
        , (102, theLastStand)
        , (103, endlessWinter)
        ]
          <> [(n, ritual n) | n <- [96, 97, 98, 100, 101]]
      )
    & #customEffects
    .~ Map.fromList
      [ ("ithaqua-reckoning", doomInBothTowns)
      , ("ithaqua-doom-innsmouth", doomInTown Innsmouth)
      , ("ithaqua-doom-kingsport", doomInTown Kingsport)
      , ("wendigo-lurk", spreadTerrorHere)
      , ("ithaqua-green-marker", greenMarkerHere)
      ]
    & #customAfterTests
    .~ Map.fromList
      ( ("countered-magic", counteredMagicResult)
          : [(ritualKey n, ritualResult n) | n <- [96, 97, 98]]
      )

wendigo, ithaqua :: CardCode
wendigo = "archive-104"
ithaqua = "archive-105"

{- | "Place one doom in any space in Innsmouth and one doom in any space in
Kingsport." The leader chooses, one town at a time, and a town with none of its
tiles on the board is simply skipped.
-}
doomInBothTowns :: EffectCtx -> GameM ()
doomInBothTowns ctx = pushAll [doomIn t | t <- [Innsmouth, Kingsport]]
 where
  doomIn town = ResolveEffect ctx (Custom (key town))
  key Innsmouth = "ithaqua-doom-innsmouth"
  key _ = "ithaqua-doom-kingsport"

-- | One doom anywhere in that town, chosen by the leader.
doomInTown :: Town -> EffectCtx -> GameM ()
doomInTown town ctx = do
  board <- use #board
  let hoods = [n.id | n <- Map.elems board.neighborhoods, n.town == town]
      spaces = [s.id | s <- Map.elems board.spaces, maybe False (`elem` hoods) s.neighborhood]
  unless (null spaces)
    $ chooseGroup
      ("Place one doom in " <> tshow town)
      [Choice (SpaceLabel sid) [PlaceDoom ctx.source sid] | sid <- spaces]

-- | The Wendigo's lurk: "Spread terror in this neighborhood."
spreadTerrorHere :: EffectCtx -> GameM ()
spreadTerrorHere ctx = case ctx.source of
  SourceMonster mid -> do
    board <- use #board
    msid <- uses #monsters (fmap (.space) . Map.lookup mid)
    for_ (msid >>= (`spaceNeighborhood` board)) (push . SpreadTerror)
  _ -> pure ()

{- | Card 91. The wendigo is already out there when the investigators arrive, and
the cold keeps coming whether or not they put it down.
-}
theHuntersKeen :: CodexBehavior
theHuntersKeen =
  defaultCodexBehavior
    { onAdd = \_ -> do
        here <- unstableSpaces
        for_ (take 1 here) (spawnHeldBack wendigo >=> pushAll)
        push (FlipCodexCard 91)
    , onFlip = \e -> when e.flipped (push (AddArchiveToCodex 92))
    , triggers =
        [ CodexTrigger
            { key = "cold-cruel-hunger"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (e.flipped && doom >= 4)
            , action = \_ -> pushAll [AddArchiveToCodex 103, RemoveCodexCard 91]
            }
        ]
    }

{- | Card 92. Clues taken off the scenario sheet are what brings the wendigo down,
and what it was guarding points three ways at once.
-}
friendOrFoe :: CodexBehavior
friendOrFoe =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Take clues from the scenario sheet to wound the wendigo"
            , allowedWhileEngaged = False
            , canPerform = \_ -> do
                clues <- use #sheetClues
                up <- inPlay wendigo
                pure (clues >= 1 && up)
            , perform = \ctx -> do
                clues <- use #sheetClues
                ms <- uses #monsters Map.elems
                found <- filterM (fmap (== wendigo) . cardCode . (.card)) ms
                for_ (take 1 found) \m ->
                  chooseFor ctx.investigator "Take how many clues?"
                    $ [ Choice
                          (AmountLabel k)
                          [ SpendSheetClues k
                          , TakeClues ctx.investigator k
                          , DealMonsterDamage m.card (SourceCodex 92) (2 * k)
                          ]
                      | k <- [1 .. clues]
                      ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "wendigo-down"
            , once = True
            , condition = \e -> do
                up <- inPlay wendigo
                waiting <- heldBackStill wendigo
                pure (not e.flipped && not up && not waiting)
            , action = \_ -> push (FlipCodexCard 92)
            }
        ]
    , onFlip = \e -> when e.flipped do
        leader <- leaderPlayer >>= investigatorOfPlayer
        chosen <- take 2 <$> shuffle [93, 94, 95]
        pushAll
          $ [GainNamedCard iid "Inuksuk" | Just iid <- [leader]]
          <> [AddArchiveToCodex n | n <- chosen]
          <> [RemoveCodexCard 92]
    }

heldBackStill :: CardCode -> GameM Bool
heldBackStill code = do
  aside <- use (#decks . #setAside)
  not . null <$> filterM (fmap (== code) . cardCode) aside

{- | Cards 93, 94 and 95: the three ways of pinning Ithaqua down. Two of them are
dealt out, each wants three markers of its own colour face up, and whichever
finishes first sweeps the other's markers onto the scenario sheet and picks how
the scenario ends.
-}
bentToHisWill, findTheHeart, counteredMagic :: CodexBehavior

-- | 93. Thralls put down where nothing has been marked yet.
bentToHisWill =
  approach
    93
    "green"
    [94, 95]
    [ ("Summon and battle Ithaqua", 99, Nothing)
    , ("Cleanse the victims", 96, Just "blue")
    , ("Seal Ithaqua away", 98, Just "white")
    ]
    & #afterMonsterDefeated
    .~ \e mid _ ->
      if e.flipped
        then pure []
        else do
          d <- monsterDef mid
          clues <- use #sheetClues
          pure
            [ ResolveEffect (sheetCtx 93) (Custom "ithaqua-green-marker")
            | "Thrall" `elem` d.traits
            , clues >= 1
            ]

-- | 94. Markers laid face down where nobody is, turned up by what happens there.
findTheHeart =
  approach
    94
    "blue"
    [93, 95]
    [ ("Shatter the heart of ice", 100, Nothing)
    , ("Cleanse the victims", 96, Just "green")
    , ("Banish Ithaqua", 97, Just "white")
    ]
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Set a marker where the heart might be"
           , allowedWhileEngaged = False
           , canPerform = \_ -> do
               clues <- use #sheetClues
               spaces <- emptyNeighborhoodSpaces
               pure (clues >= 1 && not (null spaces))
           , perform = \_ -> do
               spaces <- emptyNeighborhoodSpaces
               chooseGroup
                 "Place a marker face down"
                 [ Choice (SpaceLabel sid) [SpendSheetClues 1, PlaceMarkerFacedown sid "blue"]
                 | sid <- spaces
                 ]
           }
       ]

-- | 95. A ward raised in one neighborhood of each town.
counteredMagic =
  approach
    95
    "white"
    [93, 94]
    [ ("Erect an eternal ward", 101, Nothing)
    , ("Banish Ithaqua", 97, Just "blue")
    , ("Seal Ithaqua away", 98, Just "green")
    ]
    & #componentActions
    .~ [ ComponentActionDef
           { label = "Counter the magic holding this neighborhood"
           , allowedWhileEngaged = False
           , canPerform = \iid -> do
               clues <- use #sheetClues
               mnid <- investigatorNeighborhood iid
               marked <- markedHoods "white"
               pure (clues >= 1 && maybe False (`notElem` marked) mnid)
           , perform = \ctx ->
               push
                 ( BeginTest
                     (newTest ctx.investigator Lore (-1) OtherTest (AfterCustom (SourceCodex 95) "countered-magic"))
                 )
           }
       ]
    & #triggers
    .~ [ CodexTrigger
           { key = "countered-magic-done"
           , once = True
           , condition = \e -> do
               towns <- townsWarded
               pure (not e.flipped && towns >= 3)
           , action = \_ -> pushAll [RemoveCodexCard 93, RemoveCodexCard 94, FlipCodexCard 95]
           }
       ]

sheetCtx :: ArchiveNumber -> EffectCtx
sheetCtx n = EffectCtx {investigator = InvestigatorId "", source = SourceCodex n, testResult = Nothing}

{- | What all three share: three of their own markers face up finishes them, and
their back sweeps the others onto the scenario sheet and offers the branches.
-}
approach
  :: ArchiveNumber -> Text -> [ArchiveNumber] -> [(Text, ArchiveNumber, Maybe Text)] -> CodexBehavior
approach n colour others branches =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "approach-" <> colour
            , once = True
            , condition = \e -> do
                mine <- faceUpMarkers [colour]
                pure (not e.flipped && length mine >= 3)
            , action = \_ -> pushAll ([RemoveCodexCard k | k <- others] <> [FlipCodexCard n])
            }
        ]
    , onFlip = \e -> when e.flipped do
        let swept = filter (/= colour) ["green", "blue", "white"]
        moved <- markersToSheet swept
        returnTokensToCup (replicate moved BlankToken)
        onSheet <- use #sheetTokens
        let open (_, _, Nothing) = True
            open (_, _, Just c) = Map.findWithDefault 0 c onSheet > 0
        chooseGroup
          "Choose how to end it"
          [ Choice (TextLabel lbl) [AddArchiveToCodex k, RemoveCodexCard n] | b@(lbl, k, _) <- branches, open b
          ]
    }

emptyNeighborhoodSpaces :: GameM [SpaceId]
emptyNeighborhoodSpaces = do
  board <- use #board
  invs <- playingInvestigators
  let busy = mapMaybe (.space) invs
      marked n = any (\sid -> maybe False (not . null . (.markers)) (Map.lookup sid board.spaces)) n.spaces
      here n = any (`elem` busy) n.spaces
  pure
    [ sid
    | n <- Map.elems board.neighborhoods
    , not (marked n) && not (here n)
    , sid <- n.spaces
    ]

markedHoods :: Text -> GameM [NeighborhoodId]
markedHoods colour = do
  board <- use #board
  marks <- faceUpMarkers [colour]
  pure
    [ n.id
    | n <- Map.elems board.neighborhoods
    , any ((`elem` n.spaces) . fst) marks
    ]

-- | "If one neighborhood in each of Innsmouth, Kingsport, and Arkham has a ward"
townsWarded :: GameM Int
townsWarded = do
  board <- use #board
  warded <- markedHoods "white"
  let towns = [n.town | n <- Map.elems board.neighborhoods, n.id `elem` warded]
  pure (length (nub towns))

-- | 95's test: "If you succeed, spend one clue to place one white marker faceup."
counteredMagicResult :: Source -> Int -> GameM ()
counteredMagicResult _ result = when (result > 0) do
  invs <- playingInvestigators
  for_ (take 1 invs) \i ->
    for_ i.space \sid -> pushAll [SpendSheetClues 1, PlaceMarker sid "white"]

{- | Cards 96, 97, 98, 100 and 101: the five endings. Each is a different way of
turning the markers face down again, and the investigators win once none are
left face up.
-}
ritual :: ArchiveNumber -> CodexBehavior
ritual n =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = ritualLabel n
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                clues <- use #sheetClues
                here <- investigatorSpace iid
                board <- use #board
                let upHere = maybe False (any (.faceUp) . (.markers)) (here >>= \sid -> Map.lookup sid board.spaces)
                pure (clues >= 1 && upHere)
            , perform = \ctx -> case ritualSkill n of
                Nothing -> turnOneHere ctx.investigator
                Just (skill, modifier) ->
                  push
                    ( BeginTest
                        (newTest ctx.investigator skill modifier OtherTest (AfterCustom (SourceCodex n) (ritualKey n)))
                    )
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "ritual-done-" <> tshow (coerce n :: Int)
            , once = True
            , condition = \e -> do
                up <- faceUpMarkers ["green", "blue", "white"]
                placed <- placedMarkers ["green", "blue", "white"]
                pure (not e.flipped && null up && not (null placed))
            , action = \_ -> push (FlipCodexCard n)
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }

ritualKey :: ArchiveNumber -> Text
ritualKey n = "ithaqua-ritual-" <> tshow (coerce n :: Int)

ritualLabel :: ArchiveNumber -> Text
ritualLabel = \case
  96 -> "Cleanse a victim of Ithaqua's influence"
  97 -> "Banish what holds this space"
  98 -> "Seal this place under the ice"
  100 -> "Work at the Heart of Ice"
  _ -> "Ward this neighborhood"

ritualSkill :: ArchiveNumber -> Maybe (Skill, Int)
ritualSkill = \case
  96 -> Just (Lore, 1)
  97 -> Just (Will, -1)
  98 -> Just (Lore, -1)
  _ -> Nothing

ritualResult :: ArchiveNumber -> Source -> Int -> GameM ()
ritualResult _ _ result = when (result > 0) do
  invs <- playingInvestigators
  for_ (take 1 invs) \i -> turnOneHere i.id

turnOneHere :: InvestigatorId -> GameM ()
turnOneHere iid = do
  here <- investigatorSpace iid
  for_ here \sid -> do
    turned <- turnMarkerDown sid
    when turned (push (SpendSheetClues 1))

{- | Card 99. Ithaqua is summoned early and fought with clues off the scenario
sheet, before it has its full strength.
-}
vanquishIthaqua :: CodexBehavior
vanquishIthaqua =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Summon Ithaqua here"
            , allowedWhileEngaged = False
            , canPerform = \_ -> not <$> inPlay ithaqua
            , perform = \ctx -> do
                here <- investigatorSpace ctx.investigator
                for_ here (spawnHeldBack ithaqua >=> pushAll)
            }
        , ComponentActionDef
            { label = "Spend the scenario sheet's clues against Ithaqua"
            , allowedWhileEngaged = True
            , canPerform = \iid -> do
                clues <- use #sheetClues
                up <- inPlay ithaqua
                here <- investigatorSpace iid
                board <- use #board
                let green =
                      maybe
                        False
                        (any (\m -> m.color == "green" && m.faceUp) . (.markers))
                        (here >>= \sid -> Map.lookup sid board.spaces)
                pure (up && clues >= 1 && green)
            , perform = \ctx -> do
                clues <- use #sheetClues
                ms <- uses #monsters Map.elems
                found <- filterM (fmap (== ithaqua) . cardCode . (.card)) ms
                here <- investigatorSpace ctx.investigator
                for_ here (void . turnMarkerDown)
                for_ (take 1 found) \m ->
                  chooseFor
                    ctx.investigator
                    "Spend how many clues?"
                    [ Choice (AmountLabel k) [SpendSheetClues k, DealMonsterDamage m.card (SourceCodex 99) k]
                    | k <- [1 .. clues]
                    ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "ithaqua-vanquished"
            , once = True
            , condition = \e -> do
                up <- inPlay ithaqua
                waiting <- heldBackStill ithaqua
                pure (not e.flipped && not up && not waiting)
            , action = \_ -> push (FlipCodexCard 99)
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }

{- | Card 102. Everything else is swept away and Ithaqua comes for them, weakened
only by what the investigators managed to put on the scenario sheet.
-}
theLastStand :: CodexBehavior
theLastStand =
  defaultCodexBehavior
    { onAdd = \_ -> do
        up <- inPlay ithaqua
        invs <- playingInvestigators
        arrival <-
          if up
            then do
              ms <- uses #monsters Map.elems
              found <- filterM (fmap (== ithaqua) . cardCode . (.card)) ms
              for_ found \m -> #monsters . ix m.card . #damage %= max 0 . subtract (2 * length invs)
              pure []
            else do
              here <- unstableSpaces
              case here of
                sid : _ -> spawnHeldBack ithaqua sid
                [] -> pure []
        pushAll (arrival <> [RemoveCodexCard k | k <- [93, 94, 95, 96, 97, 98, 99, 100, 101]])
    , -- "Ithaqua's health is reduced by two for each clue on the scenario sheet."
      monsterHealthDelta = \_ mid -> do
        code <- cardCode mid
        clues <- use #sheetClues
        pure (if code == ithaqua then negate (2 * clues) else 0)
    , triggers =
        [ CodexTrigger
            { key = "last-stand-won"
            , once = True
            , condition = \e -> do
                up <- inPlay ithaqua
                waiting <- heldBackStill ithaqua
                pure (not e.flipped && not up && not waiting)
            , action = \_ -> push (FlipCodexCard 102)
            }
        , CodexTrigger
            { key = "last-stand-lost"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 15)
            , action = \_ -> push (LoseTheGame "It's all too much")
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }

{- | Card 103. The winter settles in for good unless the investigators can throw
enough of what they have gathered into holding it off.
-}
endlessWinter :: CodexBehavior
endlessWinter =
  defaultCodexBehavior
    { onAdd = \_ -> do
        returnTokensToCup [GateBurstToken, SpreadTerrorToken]
        logText "A gate burst and a spread terror token join the mythos cup"
    , triggers =
        [ CodexTrigger
            { key = "endless-winter"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 10)
            , action = \_ -> do
                placed <- placedMarkers ["green", "blue", "white"]
                clues <- use #sheetClues
                invs <- playingInvestigators
                let held = clues + sum (map (.clues) invs)
                    spent = length placed + held
                #board . #spaces . traversed . #markers .= []
                if spent >= 5
                  then pushAll [SpendSheetClues clues, AddArchiveToCodex 102, RemoveCodexCard 103]
                  else push (FlipCodexCard 103)
            }
        ]
    , onFlip = \e -> when e.flipped (push (LoseTheGame "Endless winter"))
    }

{- | 93's rider: "you may spend one clue from the scenario sheet to place one
green marker faceup in your space."
-}
greenMarkerHere :: EffectCtx -> GameM ()
greenMarkerHere _ = do
  invs <- playingInvestigators
  for_ (take 1 invs) \i -> for_ i.space \sid -> do
    board <- use #board
    let bare = maybe False (null . (.markers)) (Map.lookup sid board.spaces)
    when bare
      $ chooseFor
        i.id
        "Spend a clue from the scenario sheet to mark this place?"
        [ label "Place a green marker" [SpendSheetClues 1, PlaceMarker sid "green"]
        , label "Leave it" []
        ]

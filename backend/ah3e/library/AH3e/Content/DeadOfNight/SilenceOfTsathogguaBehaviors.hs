-- | Mechanics for Silence of Tsathoggua (Dead of Night).
module AH3e.Content.DeadOfNight.SilenceOfTsathogguaBehaviors (behaviors) where

import AH3e.Content.Tiles (spaceIdFor)
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
    & #codex
    .~ Map.fromList
      [ (53, aSleepingCity)
      , (54, theSearch)
      , (55, beacon 55 (nb "Northside") "green")
      , (56, beacon 56 (nb "Uptown") "blue")
      , (57, alienScience)
      , (58, arkhamRavaged)
      , (59, theFirstBite)
      ]
    & #monsters
    .~ Map.fromList [("sot-60", tsathoggua)]
    & #customEffects
    .~ Map.fromList
      [ ("sot-reckoning", reckoning)
      , ("sot-return-gate-token", returnGateToken)
      , ("sot-advance", advance)
      , ("sot-beacon-55", beaconAttempt 55 Observation)
      , ("sot-beacon-56", beaconAttempt 56 Will)
      ]
    & #customAfterTests
    .~ Map.fromList
      [ ("sot-beacon-passed-55", beaconPassed 55)
      , ("sot-beacon-passed-56", beaconPassed 56)
      , ("sot-alien-science", alienSciencePassed)
      ]

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

hangmansHill :: SpaceId
hangmansHill = spaceIdFor "Hangman's Hill"

markerColour :: Text
markerColour = "white"

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (listToMaybe . filter ((== n) . (.number)))

codexCtx :: ArchiveNumber -> GameM (Maybe EffectCtx)
codexCtx n = do
  ps <- playingInvestigators
  pure (case ps of i : _ -> Just (EffectCtx i.id (SourceCodex n) Nothing); [] -> Nothing)

doomAtLeast :: Int -> CodexEntry -> GameM Bool
doomAtLeast n e = do
  doom <- use #sheetDoom
  pure (not e.flipped && doom >= n)

-- | The scenario sheet: any neighborhood hoarding clues feeds the sheet's doom.
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  hoarded <- uses (#board . #neighborhoods) (any ((>= 2) . (.clues)) . Map.elems)
  when hoarded $ push (PlaceDoomOnSheet 1)

{- | Card 53. Two doom on the sheet and the Mi-Go's own experiment breaks loose;
every augmented monster the investigators put down is a piece of their machinery
salvaged onto the sheet.
-}
aSleepingCity :: CodexBehavior
aSleepingCity =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "maw-opens"
            , once = True
            , condition = doomAtLeast 2
            , action = \_ -> push (FlipCodexCard 53)
            }
        , CodexTrigger
            { key = "first-bite"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                already <- codexHas 59
                pure (e.flipped && doom >= 5 && not already)
            , action = \_ -> push (AddArchiveToCodex 59)
            }
        , CodexTrigger
            { key = "alien-science"
            , once = True
            , condition = \e -> do
                markers <- use #sheetMarkers
                already <- codexHas 57
                pure (e.flipped && markers >= 2 && not already)
            , action = \_ -> push (AddArchiveToCodex 57)
            }
        ]
    , onFlip = \e -> when e.flipped $ augmented "Aberration" (spaceIdFor "Black Cave") >>= pushAll
    , -- a marked monster carries Elite 1, which is one health per investigator
      monsterHealthDelta = \e mid ->
        if not e.flipped
          then pure 0
          else do
            marked <- isMarked mid
            n <- length <$> playingInvestigators
            pure (if marked then n else 0)
    , {- "defeated by an investigator in its space": the monster is still on the board
      while this runs, so its space is the one to compare against. -}
      afterMonsterDefeated = \_ mid src -> do
        marked <- isMarked mid
        inPlace <- case src of
          SourceInvestigator iid -> do
            m <- uses #monsters (Map.lookup mid)
            msid <- investigatorSpace iid
            pure (maybe False ((== msid) . Just . (.space)) m)
          _ -> pure False
        pure [m | marked && inPlace, m <- [LogText "The Mi-Go's machinery is salvaged", MarkSheet 1]]
    }

-- | Reveals a monster of that trait from the deck, puts it on the board and marks it.
augmented :: Trait -> SpaceId -> GameM [Message]
augmented trait sid = do
  found <- revealMonstersFromBottom trait 1
  pure
    $ concat [[PlaceMonster mid sid Ready, PlaceMonsterMarker mid markerColour] | mid <- take 1 found]

isMarked :: CardId -> GameM Bool
isMarked mid = uses #monsters (maybe False (any ((== markerColour) . (.color)) . (.markers)) . Map.lookup mid)

{- | Card 54. Two clues on the sheet and the Mi-Go's own comings and goings give the
search three places to look; two of them hold a beacon.
-}
theSearch :: CodexBehavior
theSearch =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "the-search"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 2)
            , action = \_ -> push (FlipCodexCard 54)
            }
        ]
    , onFlip = \e -> when e.flipped do
        colours <- shuffle ["green", "blue", "red"]
        let sites = map spaceIdFor ["Black Cave", "Science Building", "Unvisited Isle"]
        for_ (zip colours sites) \(c, sid) -> spaceL sid . #markers %= (<> [Marker c False])
        logText "Three markers go down facedown across the city"
    , componentActions =
        [ ComponentActionDef
            { label = "Watch where the Mi-Go go"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 54
                msid <- investigatorSpace iid
                hidden <- maybe (pure []) (fmap (filter (not . (.faceUp))) . markersAt) msid
                pure (maybe False (.flipped) me && not (null hidden))
            , perform = revealSite
            }
        ]
    }

{- | Card 54's action. Whichever marker is under your feet decides what you found:
nothing, or one of the two beacons, which then attaches to its neighborhood deck.
-}
revealSite :: EffectCtx -> GameM ()
revealSite ctx = do
  msid <- investigatorSpace ctx.investigator
  for_ msid \sid -> do
    ms <- markersAt sid
    case break (not . (.faceUp)) ms of
      (before, m : after) -> do
        spaceL sid . #markers .= before <> after
        logText ("You uncover the " <> m.color <> " marker")
        case m.color of
          "green" -> found "green" (nb "Northside") 55
          "blue" -> found "blue" (nb "Uptown") 56
          _ -> pure ()
        done <- bothFound
        when done do
          spaces <- uses (#board . #spaces) Map.keys
          for_ spaces \s -> spaceL s . #markers %= filter (.faceUp)
          pushEnd (RemoveCodexCard 54)
      _ -> pure ()
 where
  found colour nid n = do
    neighborhoodL nid . #markers %= (<> [Marker colour True])
    #codex %= map \e -> if e.number == 54 then e & #tokens %~ Map.insert colour 1 else e
    push (AddArchiveToCodex n)
  bothFound = do
    me <- entryOf 54
    pure (maybe False (\e -> Map.member "green" e.tokens && Map.member "blue" e.tokens) me)

{- | Cards 55 and 56. Each beacon is attached to a neighborhood deck and offers its
test after an encounter there; taking one apart puts its marker on the sheet.
-}
beacon :: ArchiveNumber -> NeighborhoodId -> Text -> CodexBehavior
beacon n hood colour =
  defaultCodexBehavior
    { reactions = \e iid -> \case
        AfterEncounter who
          | who == iid
          , not e.flipped -> do
              mine <- investigatorNeighborhood iid
              pure
                [ Reaction
                    ("sot-beacon-" <> tshow (number n))
                    "Study the Mi-Go beacon"
                    [ResolveEffect (EffectCtx iid (SourceCodex n) Nothing) (Custom ("sot-beacon-" <> tshow (number n)))]
                | mine == Just hood
                ]
        _ -> pure []
    , onFlip = \e -> when e.flipped do
        neighborhoodL hood . #markers %= filter ((/= colour) . (.color))
        markers <- use #sheetMarkers
        already <- codexHas 57
        pushAll
          $ [MarkSheet 1]
          <> [AddArchiveToCodex 57 | markers + 1 >= 2 && not already]
          <> [RemoveCodexCard n]
    }

number :: ArchiveNumber -> Int
number = coerce

beaconAttempt :: ArchiveNumber -> Skill -> EffectCtx -> GameM ()
beaconAttempt n skill ctx =
  push
    ( BeginTest
        ( newTest
            ctx.investigator
            skill
            0
            EncounterTest
            (AfterCustom (SourceCodex n) ("sot-beacon-passed-" <> tshow (number n)))
        )
    )

-- | The beacon comes apart only if the sheet can pay the two clues it costs.
beaconPassed :: ArchiveNumber -> Source -> Int -> GameM ()
beaconPassed n _ r = when (r > 0) do
  clues <- use #sheetClues
  if clues >= 2
    then pushAll [SpendSheetClues 2, FlipCodexCard n]
    else logText "There are not enough clues on the scenario sheet to finish the work"

{- | Card 57. The salvaged Mi-Go parts become a device of the investigators' own,
built a clue at a time at the Science Building.
-}
alienScience :: CodexBehavior
alienScience =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Re-purpose the alien equipment"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 57
                at' <- (== Just (spaceIdFor "Science Building")) <$> investigatorSpace iid
                clues <- use #sheetClues
                pure (maybe False (not . (.flipped)) me && at' && clues >= 1)
            , perform = \ctx -> do
                clues <- use #sheetClues
                chooseFor
                  ctx.investigator
                  "Spend clues from the scenario sheet to roll that many dice"
                  [ Choice
                      (TextLabel (tshow k <> (if k == 1 then " clue" else " clues")))
                      [ SpendSheetClues k
                      , BeginTest
                          ( newTest
                              ctx.investigator
                              Lore
                              0
                              (ActionTest (ComponentAction (CodexRef 57) 0) Nothing)
                              (AfterCustom (SourceCodex 57) "sot-alien-science")
                          )
                            { fixedPool = Just k
                            }
                      ]
                  | k <- [1 .. clues]
                  ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "the-dawn"
            , once = True
            , condition = \e -> do
                markers <- use #sheetMarkers
                pure (not e.flipped && markers >= 5)
            , action = \_ ->
                pushAll
                  [ FlipCodexCard 57
                  , LogText "The device grinds to life: Investigators win the game!"
                  , WinTheGame
                  ]
            }
        ]
    }

{- | A marker placed this round is what keeps Tsathoggua from taking another bite,
so card 58 is told which round it happened in.
-}
alienSciencePassed :: Source -> Int -> GameM ()
alienSciencePassed _ r = when (r > 0) do
  r' <- use #round
  #codex %= map \e -> if e.number == 58 then e & #tokens %~ Map.insert "marked-round" r' else e
  push (MarkSheet 1)

{- | Card 59. Five doom on the sheet and the first bite takes the bridge; nine and
Tsathoggua itself arrives.
-}
theFirstBite :: CodexBehavior
theFirstBite =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        let bridge = SpaceId (coerce (nb "Merchant District") <> "--" <> coerce (nb "Miskatonic University"))
        board <- use #board
        when (Map.member bridge board.spaces) do
          let escapes = filter (/= bridge) (adjacentSpaces bridge board)
          caught <- investigatorsAt bridge
          ms <- uses #monsters (filter ((== bridge) . (.space)) . Map.elems)
          for_ caught \i ->
            chooseFor i.id "Move off the ruined bridge"
              $ spaceChoices escapes \s -> [SufferHarm i.id SourceScenario NormalHarm 2 0, MoveDirectly i.id s]
          for_ ms \m -> for_ (listToMaybe escapes) \s -> monsterL m.card . #space .= s
          removeSpace bridge
          logText "The Erwin Bridge is pulled down into a cavernous throat"
        swapMythosToken BlankToken GateBurstToken
    , triggers =
        [ CodexTrigger
            { key = "tsathoggua"
            , once = True
            , condition = doomAtLeast 9
            , action = \_ -> push (FlipCodexCard 59)
            }
        ]
    , onFlip = \e -> when e.flipped do
        markers <- use #sheetMarkers
        spawn <- takeAside "sot-60" (spaceIdFor "Curiositie Shoppe")
        second' <- augmented "Formless Spawn" hangmansHill
        swapMythosToken BlankToken SpreadDoomToken
        #sheetDoom .= 0
        let bitten = [DealMonsterDamage mid SourceScenario (2 * markers) | markers > 0, PlaceMonster mid _ _ <- spawn]
        pushAll (spawn <> bitten <> second' <> [AddArchiveToCodex 58, RemoveCodexCard 59])
    }

-- | Takes the set-aside epic monster out of the pile and puts it on the board.
takeAside :: CardCode -> SpaceId -> GameM [Message]
takeAside wanted sid = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wanted) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      pure [PlaceMonster cid sid Ready]
    _ -> pure []

-- | Takes one token out of the cup and puts another in its place.
swapMythosToken :: MythosToken -> MythosToken -> GameM ()
swapMythosToken from to' = do
  cup <- use #cup
  case break (== from) cup of
    (before, _ : after) -> #cup .= before <> after <> [to']
    _ -> #cup %= (<> [to'])

{- | Card 58. Tsathoggua eats the city instead of the scenario sheet: every doom the
sheet would take drags it another space toward Hangman's Hill, and whatever tile
it leaves behind goes down its throat.
-}
arkhamRavaged :: CodexBehavior
arkhamRavaged =
  defaultCodexBehavior
    { sheetDoomReplacement = \e n ->
        if e.flipped
          then pure Nothing
          else do
            here <- use #round
            let marked = Map.lookup "marked-round" e.tokens == Just here
            ctx <- codexCtx 58
            pure case ctx of
              _ | marked -> Just [LogText "The device holds Tsathoggua back for now"]
              Just c -> Just (replicate n (ResolveEffect c (Custom "sot-advance")))
              Nothing -> Nothing
    , afterMonsterDefeated = \_ mid _ -> do
        code <- cardCode mid
        pure
          [ m
          | code == "sot-60"
          , m <- [FlipCodexCard 58, LogText "Arkham Salvaged: Investigators win the game!", WinTheGame]
          ]
    }

-- | One lurch toward Hangman's Hill, and the tile left behind is devoured.
advance :: EffectCtx -> GameM ()
advance _ = do
  found <- tsathogguaOnBoard
  for_ found \mid -> do
    m <- getMonster mid
    board <- use #board
    case stepToward board m.space hangmansHill of
      Nothing -> pure ()
      Just next -> do
        let riders = case m.state of Engaged is -> is; _ -> []
        monsterL mid . #space .= next
        for_ riders \iid -> investigatorL iid . #space ?= next
        logText "Tsathoggua drags itself another space toward Hangman's Hill"
        when (tileOf board m.space /= tileOf board next) $ devour (tileOf board m.space)
        board' <- use #board
        when (spaceNeighborhood next board' == Just (nb "Uptown"))
          $ pushAll
            [ FlipCodexCard 58
            , LogText "Arkham Devoured: Investigators lose the game!"
            , LoseTheGame "Arkham Devoured"
            ]

tsathogguaOnBoard :: GameM (Maybe CardId)
tsathogguaOnBoard = do
  ms <- uses #monsters Map.elems
  found <- filterM (fmap (== "sot-60") . cardCode . (.card)) ms
  pure (listToMaybe (map (.card) found))

-- | A tile is a neighborhood, or the street space between two of them.
tileOf :: Board -> SpaceId -> Either NeighborhoodId SpaceId
tileOf board sid = maybe (Right sid) Left (spaceNeighborhood sid board)

{- | Everything on the devoured tile goes with it: the tokens and monsters are
discarded, the investigators are devoured, and the spaces leave the board.
-}
devour :: Either NeighborhoodId SpaceId -> GameM ()
devour tile = do
  board <- use #board
  let spaces = case tile of
        Left nid -> neighborhoodSpaces nid board
        Right sid -> [sid]
  caught <- fmap concat (traverse investigatorsAt spaces)
  ms <- uses #monsters (filter ((`elem` spaces) . (.space)) . Map.elems)
  -- nothing may be left pointing at a space that is no longer on the board
  for_ ms \m -> #monsters . at m.card .= Nothing
  for_ caught \i -> investigatorL i.id . #space .= Nothing
  for_ spaces removeSpace
  pushAll [DevourInvestigator i.id | i <- caught]
  logText "A whole tile of Arkham goes down Tsathoggua's throat"

-- | Takes a space off the board, with the borders that led to it.
removeSpace :: SpaceId -> GameM ()
removeSpace sid = do
  #board . #spaces . at sid .= Nothing
  #board . #borders . at sid .= Nothing
  #board . #borders %= Map.map (Map.delete sid)
  #board . #layout . #anchors %= filter ((/= sid) . (.space))
  #board . #layout . #streets %= filter ((/= sid) . (.space))

-- | The first step of a shortest path, which is all a one-space lurch needs.
stepToward :: Board -> SpaceId -> SpaceId -> Maybe SpaceId
stepToward board from goal = go [(s, s) | s <- adjacentSpaces from board] [from]
 where
  go [] _ = Nothing
  go frontier seen
    | Just (first', _) <- listToMaybe [(f, s) | (f, s) <- frontier, s == goal] = Just first'
    | otherwise =
        let seen' = seen <> map snd frontier
            next =
              [ (f, a)
              | (f, s) <- frontier
              , a <- adjacentSpaces s board
              , a `notElem` seen'
              ]
         in if null next then Nothing else go (nubOnFst next) seen'
  nubOnFst = foldl (\acc x -> if snd x `elem` map snd acc then acc else acc <> [x]) []

{- | Card 60. Its lurker step hands a gate burst token back to the cup, and stepping
away from an attack of its costs either your turn or another shred of your mind.
-}
tsathoggua :: MonsterBehavior
tsathoggua =
  defaultMonsterBehavior
    & #afterAttack
    .~ \mid iid ->
      pure
        [ ResolveEffect
            (EffectCtx iid (SourceMonster mid) Nothing)
            (Choose [("Suffer one additional horror", DirectHorror (N 1)), ("Become delayed", BecomeDelayed)])
        ]

returnGateToken :: EffectCtx -> GameM ()
returnGateToken _ = do
  drawn <- use #drawnTokens
  case break (== GateBurstToken) drawn of
    (before, _ : after) -> do
      #drawnTokens .= before <> after
      returnTokensToCup [GateBurstToken]
      logText "A gate burst token returns to the mythos cup"
    _ -> logText "No gate burst token to return to the mythos cup"

{- | What The Key and the Gate asks of the engine directly: its scenario sheet, and the
codex chain from the Lurker's first pull on Arkham to whatever is done with the Key
(cards 150-165).

Yog-Sothoth is working on the gate from the other side, and the scenario is its pull.
Carl Sanford knows how to shut the gate but has lost the Elders who hold the rite, so
the first half is finding them -- each one waiting facedown on top of their own
neighborhood's deck -- and the second is fetching the Key of Zagan out of the Underworld
that card 153 opens. With the Key in hand there are two endings: lock the gate, or keep
it and the knowledge behind it.
-}
module AH3e.Content.SecretsOfTheOrder.TheKeyAndTheGateBehaviors (behaviors) where

import AH3e.Content.Tiles
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
import Data.List (find)
import Data.Map.Strict qualified as Map
import Data.Text qualified as T

behaviors :: Behaviors
behaviors =
  mempty
    { codex =
        Map.fromList
          [ (150, atTheThreshold)
          , (151, possession)
          , (152, theMissingElders)
          , (153, theWayOpens)
          , (154, lockTheGate)
          , (155, controlTheGate)
          , (156, uponTheThreshold)
          , (157, scrapingAtTheDoor)
          , (160, serveTheDarkness)
          ]
    , assets = Map.fromList [(lureOfPower, lure), (theBeyondOne, lure)]
    , customEffects =
        Map.fromList
          [ ("the-key-and-the-gate-reckoning", reckoning)
          , ("the-key-and-the-gate-pull", pull)
          , ("katg-find-an-elder", findAnElder)
          , ("katg-sanford-found", sanfordFound)
          , ("katg-reveal-in-underworld", revealInUnderworld)
          , ("katg-underworld-toll", underworldToll)
          , ("katg-underworld-marker", underworldMarker)
          , ("katg-lock-clues", lockClues)
          , ("katg-pact-count", pactCount)
          , ("katg-lure-action", lureAction)
          , ("katg-lure-reckoning", lureReckoning)
          , ("katg-fallen-reckoning", fallenReckoning)
          , ("katg-fallen-toll", fallenToll)
          ]
    }

-- the scenario sheet

{- | "Each investigator that is not engaged with one or more monsters moves one space
toward the unstable space unless they suffer one horror." A monster already has hold
of whoever it is engaged with, so the Lurker's pull passes them by.
-}
reckoning :: EffectCtx -> GameM ()
reckoning ctx = do
  invs <- playingInvestigators
  loose <- filterM (fmap null . engagedMonsters . (.id)) invs
  pushAll
    [ResolveEffect (ctx & #investigator .~ i.id) (Custom "the-key-and-the-gate-pull") | i <- loose]

{- | The pull itself. Only a step that closes the distance counts as moving toward the
unstable space, and standing in it already there is nowhere nearer to go.
-}
pull :: EffectCtx -> GameM ()
pull ctx = do
  let iid = ctx.investigator
  board <- use #board
  msid <- investigatorSpace iid
  unstable <- unstableSpaces
  let nearer = case (msid, unstable) of
        (Just here, target : _) ->
          let dist = distancesFrom (`adjacentSpaces` board) target
              mine = Map.lookup here dist
           in [ sid
              | sid <- adjacentSpaces here board
              , Just d <- [Map.lookup sid dist]
              , maybe False (d <) mine
              ]
        _ -> []
  unless (null nearer)
    $ chooseFor iid "The Lurker draws you toward the gate"
    $ label "Suffer one horror" [ResolveEffect ctx (SufferHorror (N 1))]
    : spaceChoices nearer \sid -> [MoveDirectly iid sid]

-- the places and cards the scenario names

theUnnamable, cityOfTheGugs :: SpaceId
theUnnamable = spaceIdFor "The Unnamable"
cityOfTheGugs = spaceIdFor "City of the Gugs"

frenchHill, underworld :: NeighborhoodId
frenchHill = NeighborhoodId "french-hill"
underworld = NeighborhoodId "the-underworld"

lureOfPower, theBeyondOne :: CardCode
lureOfPower = "archive-158"
theBeyondOne = "archive-159"

elderCards :: [CardCode]
elderCards = [CardCode ("archive-" <> tshow n) | n <- [161 .. 165 :: Int]]

-- shared state

sourceCard :: EffectCtx -> Maybe CardId
sourceCard ctx = case ctx.source of
  SourceCard cid -> Just cid
  _ -> Nothing

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (find ((== n) . (.number)))

inCodex :: ArchiveNumber -> GameM Bool
inCodex n = isJust <$> entryOf n

countedOn :: Text -> CodexEntry -> Int
countedOn key e = Map.findWithDefault 0 key e.tokens

scenarioCtx :: ArchiveNumber -> InvestigatorId -> EffectCtx
scenarioCtx n iid = EffectCtx {investigator = iid, source = SourceCodex n, testResult = Nothing}

-- | Whoever stands closest to a space, measured the way an investigator walks.
nearestInvestigatorTo :: SpaceId -> GameM (Maybe InvestigatorId)
nearestInvestigatorTo sid = do
  board <- use #board
  invs <- playingInvestigators
  let dist = distancesFrom (`adjacentSpaces` board) sid
      scored = [(i.id, d) | i <- invs, Just s <- [i.space], Just d <- [Map.lookup s dist]]
  pure case scored of
    [] -> listToMaybe [i.id | i <- invs]
    _ -> let best = minimum (map snd scored) in listToMaybe [iid | (iid, d) <- scored, d == best]

-- | The investigator the Lurker reaches for: whoever is nearest the gate itself.
nearestToTheGate :: GameM (Maybe InvestigatorId)
nearestToTheGate =
  unstableSpaces >>= \case
    (sid : _) -> nearestInvestigatorTo sid
    [] -> fmap (.id) . listToMaybe <$> playingInvestigators

-- | A test every investigator takes at once, which both sides of the threshold ask for.
everyoneTests :: ArchiveNumber -> Int -> Effect -> GameM ()
everyoneTests n modifier onFail = do
  invs <- playingInvestigators
  pushAll [ResolveEffect (scenarioCtx n i.id) (Test Will modifier NoEffect onFail) | i <- invs]

-- 150

{- | The Lurker's first reach into Arkham: a whisper everyone hears in their sleep, and
once it is loud enough the gate itself starts to open.
-}
atTheThreshold :: CodexBehavior
atTheThreshold =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "threshold"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 4) <$> use #sheetDoom
            , action = \_ -> push (FlipCodexCard 150)
            }
        , -- the card stays in the codex, so the gate opening does not close the dream
          CodexTrigger
            { key = "beyond"
            , once = True
            , condition = \e -> (\doom -> e.flipped && doom >= 8) <$> use #sheetDoom
            , action = \_ -> do
                there <- inCodex 156
                pushAll [AddArchiveToCodex 156 | not there]
            }
        ]
    , onFlip = \e ->
        when e.flipped
          $ everyoneTests
            150
            0
            ( Choose
                [ ("Place one doom in your space", PlaceDoomAt YourSpace (N 1))
                , ("Become FATIGUED", GainE (Condition "FATIGUED"))
                ]
            )
    }

-- 151

{- | Carl Sanford went on to the Unnamable without them, and finding him costs whatever
they have not already learned for themselves.
-}
possession :: CodexBehavior
possession =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Find Carl Sanford"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 151
                here <- investigatorSpace iid
                pure (maybe False (not . (.flipped)) me && here == Just theUnnamable)
            , perform = \ctx -> do
                toll <- sanfordToll
                pushAll
                  $ [SufferHarm ctx.investigator (SourceCodex 151) NormalHarm 0 toll | toll > 0]
                  <> [FlipCodexCard 151]
            }
        ]
    , onFlip = \e -> when e.flipped do
        asker <- nearestToTheGate
        pushAll
          [ResolveEffect (scenarioCtx 151 iid) (Custom "katg-sanford-found") | iid <- maybeToList asker]
    }

{- | "Reduce the cost of this action by one horror for each clue on the scenario sheet.
(Do not spend or discard those clues.)" What they already know is what spares them.
-}
sanfordToll :: GameM Int
sanfordToll = do
  clues <- use #sheetClues
  pure (max 0 (2 - clues))

{- | 151's back. The five Elders go facedown on top of their own neighborhoods' decks,
so each is found by going to the place rather than by looking for them, and a white
marker on each neighborhood is what says one is still missing there.
-}
sanfordFound :: EffectCtx -> GameM ()
sanfordFound _ = do
  aside <- use (#decks . #setAside)
  elders <- filterM (fmap (`elem` elderCards) . cardCode) aside
  board <- use #board
  hoods <- fmap catMaybes $ for elders \cid -> do
    d <- getCardDef cid
    pure case d.kind of
      NeighborhoodCard nid _ | Map.member nid board.neighborhoods -> Just (cid, nid)
      _ -> Nothing
  #decks . #setAside %= filter (`notElem` map fst hoods)
  for_ hoods \(cid, nid) -> #decks . #neighborhoods . at nid %= Just . (cid :) . fromMaybe []
  unless (null hoods) $ logText "An Elder waits in each neighborhood, if you know where to look"
  pushAll
    $ [PlaceNeighborhoodMarker nid "white" True | (_, nid) <- hoods]
    <> [AddArchiveToCodex 152, RemoveCodexCard 151]

-- 152

{- | The Elders themselves. Each one brought back is a white marker off its neighborhood
and onto the sheet, and the count is the whole first half of the scenario: one draws the
Lurker's attention, three open the way to the Underworld, five finish the rite.
-}
theMissingElders :: CodexBehavior
theMissingElders =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "noticed"
            , once = True
            , condition = \e -> (\m -> not e.flipped && m >= 1) <$> use #sheetMarkers
            , action = \_ -> do
                there <- inCodex 157
                pushAll [AddArchiveToCodex 157 | not there]
            }
        , CodexTrigger
            { key = "the-way"
            , once = True
            , condition = \e -> (\m -> not e.flipped && m >= 3) <$> use #sheetMarkers
            , action = \_ -> do
                there <- inCodex 153
                pushAll [AddArchiveToCodex 153 | not there]
            }
        , CodexTrigger
            { key = "restored"
            , once = True
            , condition = \e -> (\m -> not e.flipped && m >= 5) <$> use #sheetMarkers
            , action = \_ -> push (FlipCodexCard 152)
            }
        ]
    , onFlip = \e -> when e.flipped do
        -- one fewer way for the doom to spread, now that the Order is whole again
        #cup %= dropFirst (== SpreadDoomToken)
        #cup %= (<> [BlankToken])
        #sheetMarkers .= 0
        logText "The Order closes ranks, and Arkham breathes"
        push (RemoveCodexCard 152)
    }

dropFirst :: (a -> Bool) -> [a] -> [a]
dropFirst p xs = case break p xs of
  (before, _ : after) -> before <> after
  _ -> xs

{- | "After you 'find an Elder' as part of an encounter, move the white marker from your
neighborhood to the scenario sheet." The card goes back to the archive either way: that
Elder has been found and is not waiting to be found again.
-}
findAnElder :: EffectCtx -> GameM ()
findAnElder ctx = do
  #encounter . _Just . #returnToArchive .= True
  board <- use #board
  msid <- investigatorSpace ctx.investigator
  let nid = msid >>= (`spaceNeighborhood` board)
  there <- inCodex 152
  for_ nid \n -> do
    held <- uses (#board . #neighborhoods . ix n . #markers) (filter ((== "white") . (.color)))
    when (there && not (null held)) do
      neighborhoodL n . #markers %= dropFirstMarker ((== "white") . (.color))
      logText "An Elder is brought back to the fold"
      push (MarkSheet 1)

-- 153

{- | The rite is done and the way opens. The Underworld is not on the board until this
card arrives: it brings the tile, the derelict portal that reaches it, and the Underworld
event cards that were set aside for exactly this.
-}
theWayOpens :: CodexBehavior
theWayOpens =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        swap <- openTheWay
        pushAll (swap <> [FlipCodexCard 153])
    , onFlip = \e -> when e.flipped do
        colours <- shuffle ["red", "red", "green"]
        board <- use #board
        let there = neighborhoodSpaces underworld board
        pushAll (zipWith PlaceMarkerFacedown there colours)
        logText "Something is hidden in each of the Underworld's three places"
    , componentActions =
        [ ComponentActionDef
            { label = "Search this place for the Key"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 153
                hidden <- hiddenMarkersWith iid
                pure (maybe False (.flipped) me && not (null hidden))
            , perform = \ctx -> push (ResolveEffect ctx (Custom "katg-reveal-in-underworld"))
            }
        ]
    }

hiddenMarkersWith :: InvestigatorId -> GameM [Marker]
hiddenMarkersWith iid = do
  here <- investigatorSpace iid
  ms <- maybe (pure []) markersAt here
  pure [m | m <- ms, not m.faceUp]

{- | What card 153 puts on the table: the Underworld joined to French Hill by a derelict
portal, doom in all three of its places, something already waiting in the gug city, and
an event deck that now reads the Underworld as well as Arkham.
-}
openTheWay :: GameM [Message]
openTheWay = do
  let added =
        buildMapOf
          [frenchHill, underworld]
          []
          noPieces
            { thresholds =
                [ThresholdTile frenchHill SideLeft underworld DerelictPortal [HazardDamage]]
            }
  -- the four cards nearest the top are what the opening gate burns through
  burnt <- uses (#decks . #event) (take 4)
  #decks . #event %= drop 4
  #decks . #removed %= (burnt <>)
  aside <- use (#decks . #setAside)
  theirs <- filterM (fmap isUnderworldEvent . cardCode) aside
  chosen <- shuffle theirs
  let (joining, shelved) = splitAt 2 chosen
  #decks . #setAside %= filter (`notElem` chosen)
  #decks . #eventDiscard %= (shelved <>)
  deck <- use (#decks . #event)
  #decks . #event <~ shuffle (deck <> joining)
  logText "The way opens, and the Underworld is on the board"
  pure
    [ AddToBoard frenchHill added
    , PlaceDoom (SourceCodex 153) (spaceIdFor "City of the Gugs")
    , PlaceDoom (SourceCodex 153) (spaceIdFor "Vaults of Zin")
    , PlaceDoom (SourceCodex 153) (spaceIdFor "Vale of Pnath")
    , SpawnMonsterAt (Just cityOfTheGugs) False
    ]

-- | The scenario's own event cards 18-21, which are the Underworld's.
isUnderworldEvent :: CardCode -> Bool
isUnderworldEvent code =
  or [coerce code == "the-key-and-the-gate-event-" <> tshow n | n <- [18 .. 21 :: Int]]

{- | 153's back. Turning a marker over is what the search costs, and the Underworld
charges for it whether or not the Key is under this one.
-}
revealInUnderworld :: EffectCtx -> GameM ()
revealInUnderworld ctx = do
  msid <- investigatorSpace ctx.investigator
  pushAll
    $ [RevealMarkerAt sid | sid <- maybeToList msid]
    <> [ResolveEffect ctx (Custom "katg-underworld-toll")]

underworldToll :: EffectCtx -> GameM ()
underworldToll ctx = do
  clues <- use #sheetClues
  let found = [ResolveEffect ctx (Custom "katg-underworld-marker")]
  if clues >= 1
    then
      chooseFor
        ctx.investigator
        "The Underworld exacts its price"
        [ label "Discard one clue from the scenario sheet" (SpendSheetClues 1 : found)
        , label
            "Suffer two damage"
            (SufferHarm ctx.investigator (SourceCodex 153) NormalHarm 2 0 : found)
        ]
    else pushAll (SufferHarm ctx.investigator (SourceCodex 153) NormalHarm 2 0 : found)

{- | What was under it. The red markers are dead ends and go as they are turned; the
green one is the Key, and finding it ends the search outright.
-}
underworldMarker :: EffectCtx -> GameM ()
underworldMarker ctx = do
  msid <- investigatorSpace ctx.investigator
  board <- use #board
  here <- maybe (pure []) markersAt msid
  case [m.color | m <- here, m.faceUp] of
    ("green" : _) -> do
      for_ (neighborhoodSpaces underworld board) \sid -> spaceL sid . #markers .= []
      logText "The Key of Zagan is in your hands"
      pushAll [AddArchiveToCodex 154, RemoveCodexCard 153]
    (colour : _) -> do
      for_ msid \sid -> spaceL sid . #markers %= dropFirstMarker ((== colour) . (.color))
      logText "Nothing here but the dark"
    [] -> pure ()

-- 154

{- | The Key, used as Sanford says to use it. Sealing the gate takes everything they have
learned, poured into it a clue at a time.
-}
lockTheGate :: CodexBehavior
lockTheGate =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        there <- inCodex 155
        pushAll [AddArchiveToCodex 155 | not there]
    , componentActions =
        [ ComponentActionDef
            { label = "Seal the gate with the Key of Zagan"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 154
                here <- investigatorSpace iid
                unstable <- unstableSpaces
                pure (maybe False (not . (.flipped)) me && maybe False (`elem` unstable) here)
            , perform = \ctx ->
                push
                  ( BeginTest
                      ( newTest
                          ctx.investigator
                          Lore
                          (-1)
                          (ActionTest (ComponentAction (CodexRef 154) 0) Nothing)
                          (AfterEffect ctx (Custom "katg-lock-clues") (SufferHorror (N 2)))
                      )
                  )
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "sealed"
            , once = True
            , condition = \e -> pure (not e.flipped && countedOn "clues" e >= 4)
            , action = \e -> pushAll [MarkCodexToken 154 "clues" (-(countedOn "clues" e)), FlipCodexCard 154]
            }
        ]
    , onFlip = \e -> when e.flipped $ pushAll [LogText "Lock the Gate: the way is shut", WinTheGame]
    }

-- | "For each success that you roll, move one clue from the scenario sheet to this card."
lockClues :: EffectCtx -> GameM ()
lockClues ctx = do
  clues <- use #sheetClues
  let moved = min (fromMaybe 0 ctx.testResult) clues
  when (moved > 0) $ pushAll [SpendSheetClues moved, MarkCodexToken 154 "clues" moved]

-- 155

{- | The Key, used the other way. Every pact is a share of what is behind the gate, and
four of them between the party is enough to keep it open on their own terms.
-}
controlTheGate :: CodexBehavior
controlTheGate =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Take a share of what lies beyond"
            , allowedWhileEngaged = False
            , canPerform = \_ -> do
                me <- entryOf 155
                clues <- use #sheetClues
                pure (maybe False (not . (.flipped)) me && clues >= 1)
            , perform = \ctx ->
                pushAll
                  [ SpendSheetClues 1
                  , GainAnotherCondition ctx.investigator "DARK PACT"
                  ]
            }
        ]
    , -- "resolve this effect after all other reckoning effects": the pacts have their
      -- own reckoning, and one that comes due is no longer a pact to count
      reckoningLast = True
    , reckoning = \e -> if e.flipped then Nothing else Just (Custom "katg-pact-count")
    , onFlip = \e -> when e.flipped do
        sworn <- pactHolders
        names <- for sworn \iid -> (.name) <$> getInvestigatorDef iid
        logText
          ("Control the Gate: " <> T.intercalate ", " names <> " hold the Key, and the Lurker's bargain")
        push WinTheGame
    }

pactHolders :: GameM [InvestigatorId]
pactHolders = do
  invs <- playingInvestigators
  filterM (`hasCondition` "DARK PACT") (map (.id) invs)

-- | Every pact between them, counted as the card counts them: all of them, not one each.
pactCount :: EffectCtx -> GameM ()
pactCount _ = do
  invs <- playingInvestigators
  pacts <- fmap concat $ for invs \i -> filterM (fmap isPact . cardCode) i.assets
  when (length pacts >= 4) $ push (FlipCodexCard 155)
 where
  isPact code = "dark-pact-" `T.isPrefixOf` coerce code

-- 156

{- | The gate is open a crack, and from here the whisper is constant. Nothing stops this
one but shutting the gate.
-}
uponTheThreshold :: CodexBehavior
uponTheThreshold =
  defaultCodexBehavior
    { onAdd = \e ->
        unless e.flipped
          $ everyoneTests
            156
            (-1)
            ( Choose
                [ ("Place two doom in your space", PlaceDoomAt YourSpace (N 2))
                , ("Become CURSED", GainE (Condition "CURSED"))
                ]
            )
    , triggers =
        [ CodexTrigger
            { key = "opened"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 13) <$> use #sheetDoom
            , action = \_ -> push (FlipCodexCard 156)
            }
        ]
    , onFlip = \e -> when e.flipped $ push (LoseTheGame "Upon the Threshold")
    }

-- 157

{- | The Lurker notices them, and picks one to work on. Whoever it is holding when anybody
falls is beside the point: the one who falls is the one it keeps.
-}
scrapingAtTheDoor :: CodexBehavior
scrapingAtTheDoor =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        invs <- playingInvestigators
        lead <- nearestToTheGate
        let wanted = if length invs >= 4 then theBeyondOne else lureOfPower
        for_ lead \iid -> handLure wanted iid >>= pushAll
    , afterInvestigatorDefeated = \e iid ->
        if e.flipped
          then pure []
          else do
            -- the lure does not go in the box with them, and nor do they
            rehomed <- rehomeLures iid
            pure (rehomed <> [MarkCodexToken 157 (fallenKey iid) 1, FlipCodexCard 157])
    , onFlip = \e -> when e.flipped do
        unstable <- unstableSpaces
        side <- randomR (0, 1)
        let fallen = [iid | (key, n) <- Map.toList e.tokens, n > 0, Just iid <- [fallenOf key]]
        for_ (take 1 fallen) \iid -> for_ (take 1 unstable) \sid -> do
          -- their token stays on the board although their sheet is put away
          investigatorL iid . #space ?= sid
          d <- getInvestigatorDef iid
          logText (d.name <> " rises again, and serves the darkness")
        pushAll
          $ [if side == (0 :: Int) then AddArchiveToCodex 160 else AddArchiveToCodexFlipped 160]
          <> [MarkCodexToken 160 (fallenKey iid) 1 | iid <- take 1 fallen]
          <> [RemoveCodexCard 157]
    }

fallenKey :: InvestigatorId -> Text
fallenKey iid = "fallen:" <> coerce iid

fallenOf :: Text -> Maybe InvestigatorId
fallenOf key = coerce <$> T.stripPrefix "fallen:" key

-- | Puts the Lurker's card in someone's hands, out of the box it was set aside in.
handLure :: CardCode -> InvestigatorId -> GameM [Message]
handLure wanted iid = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wanted) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      pure [GainAsset iid cid]
    _ -> [] <$ logText "The Lurker has nothing left to whisper through"

{- | "If you are defeated, gain this card after you select a new investigator." The card
is undiscardable, so it survives the fall; it is handed to whoever the Lurker can reach
instead, since the replacement is not chosen yet.
-}
rehomeLures :: InvestigatorId -> GameM [Message]
rehomeLures iid = do
  held <- uses #assets (Map.elems . Map.filter ((== iid) . (.owner)))
  theirs <- filterM (fmap (`elem` [lureOfPower, theBeyondOne]) . cardCode . (.card)) held
  taker <- nearestToTheGate
  fmap concat $ for theirs \a -> case taker of
    Just next | next /= iid -> do
      #assets . at a.card . _Just . #owner .= next
      investigatorL iid . #assets %= filter (/= a.card)
      investigatorL next . #assets %= (<> [a.card])
      pure []
    _ -> pure []

-- 158 and 159

{- | The Lurker's card, whichever of the two it is and whichever side is showing. Every
action it spoils costs its holder a doom on the card or a horror of their own, and each
reckoning tips the doom into the board and passes the card on.
-}
lure :: AssetBehavior
lure =
  defaultAssetBehavior
    { undiscardable = True
    , reckoning = Just (Custom "katg-lure-reckoning")
    , afterOwnerAction = \cid iid kind -> do
        spoils <- spoiledActions cid
        pure
          [ ResolveEffect (EffectCtx iid (SourceCard cid) Nothing) (Custom "katg-lure-action")
          | spoils kind
          ]
    }

{- | Which actions the side now showing turns against its holder. A component action
carries the card it belongs to, so the side spoils that kind of action rather than any
one of them in particular.
-}
spoiledActions :: CardId -> GameM (ActionKind -> Bool)
spoiledActions cid = do
  code <- cardCode cid
  flipped <- uses #assets (maybe False (.flipped) . Map.lookup cid)
  let plain ks k = k `elem` ks
      component = \case ComponentAction {} -> True; _ -> False
  pure case (code == lureOfPower, flipped) of
    (True, False) -> plain [FocusAction, WardAction]
    (True, True) -> plain [ResearchAction, AttackAction]
    (False, False) -> \k -> plain [WardAction, FocusAction] k || component k
    (False, True) -> plain [ResearchAction, AttackAction, GatherResourcesAction]

{- | "Place one doom on this card unless you suffer one direct horror." Neither way out is
free, so it is asked rather than taken.
-}
lureAction :: EffectCtx -> GameM ()
lureAction ctx = for_ (sourceCard ctx) \cid -> do
  held <- uses #assets (maybe 0 (Map.findWithDefault 0 "doom" . (.tokens)) . Map.lookup cid)
  chooseFor
    ctx.investigator
    "The whisper turns your own hands against you"
    [ label "Place one doom on the card" [NoteOnCard cid "doom" (held + 1)]
    , label "Suffer one direct horror" [ResolveEffect ctx (DirectHorror (N 1))]
    ]

{- | "Move all doom from this card to your space. Then the investigator nearest to the
unstable space gains this card and flips it." Whatever the holder resisted lands where
they stand, and the card moves on to whoever is closest to the gate.
-}
lureReckoning :: EffectCtx -> GameM ()
lureReckoning ctx = for_ (sourceCard ctx) \cid -> do
  ma <- use (#assets . at cid)
  for_ ma \a -> do
    let held = Map.findWithDefault 0 "doom" a.tokens
    msid <- investigatorSpace a.owner
    #assets . at cid . _Just . #tokens . at "doom" .= Nothing
    taker <- nearestToTheGate
    for_ taker \next -> when (next /= a.owner) do
      investigatorL a.owner . #assets %= filter (/= cid)
      investigatorL next . #assets %= (<> [cid])
      #assets . at cid . _Just . #owner .= next
      d <- getInvestigatorDef next
      logText ("The whisper finds " <> d.name)
    #assets . at cid . _Just . #flipped %= not
    pushAll [PlaceDoom (SourceCard cid) sid | sid <- maybeToList msid, held > 0, _ <- [1 .. held]]

-- 160

{- | What is left of whoever the Lurker took. Their token walks the board without them,
and each mythos phase it does one of three things before making for the gate again.
-}
serveTheDarkness :: CodexBehavior
serveTheDarkness =
  defaultCodexBehavior & #reckoning .~ \_ -> Just (Custom "katg-fallen-reckoning")

-- | Whoever's sheet is under card 160.
theFallenOne :: GameM (Maybe InvestigatorId)
theFallenOne = do
  me <- entryOf 160
  pure case me of
    Just e -> listToMaybe [iid | (key, n) <- Map.toList e.tokens, n > 0, Just iid <- [fallenOf key]]
    Nothing -> Nothing

{- | The fallen one's reckoning. Which band of the die does what depends on the side
showing: hunting the light is quicker to reach for the living than serving the darkness is.
-}
fallenReckoning :: EffectCtx -> GameM ()
fallenReckoning _ = do
  mfallen <- theFallenOne
  flipped <- maybe False (.flipped) <$> entryOf 160
  for_ mfallen \fallen -> do
    n <- rollDie
    logText ("The fallen one rolls " <> tshow n)
    here <- investigatorSpace fallen
    victim <- maybe (pure Nothing) nearestInvestigatorTo here
    unstable <- unstableSpaces
    let doomBand = if flipped then n <= 1 else n <= 3
        harmBand = if flipped then n >= 2 && n <= 4 else n == 4 || n == 5
        toll
          | doomBand = [PlaceDoom (SourceCodex 160) sid | sid <- maybeToList here, _ <- [1, 2 :: Int]]
          | harmBand = [SufferHarm iid (SourceCodex 160) NormalHarm 1 1 | iid <- maybeToList victim]
          | otherwise =
              [ ResolveEffect (scenarioCtx 160 iid) (Custom "katg-fallen-toll")
              | iid <- maybeToList victim
              ]
    for_ (take 1 unstable) \sid -> investigatorL fallen . #space ?= sid
    pushAll (toll <> [FlipCodexCard 160])

-- | "Discards one focus, one clue, or one item (of their choice)."
fallenToll :: EffectCtx -> GameM ()
fallenToll ctx = do
  let iid = ctx.investigator
  i <- getInvestigator iid
  items <- matchingAssets iid ItemCard
  chooseFor
    iid
    "The fallen one takes something from you"
    $ [ label ("Discard one " <> T.toLower (tshow sk) <> " focus") [DiscardFocus iid sk]
      | (sk, n) <- Map.toList i.focus
      , n > 0
      ]
    <> [label "Discard one clue" [DiscardClue (Just iid)] | i.clues > 0]
    <> [Choice (CardLabel cid) [DiscardAsset cid] | cid <- items]

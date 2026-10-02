{- | What The Dead Cry Out asks of the engine directly: its scenario sheet, and the
codex chain from the first wave of beasts to the Seer of Mnar's undoing (cards
135-149).

The gugs are here for the city's people, not for the investigators: while a bystander
stands anywhere on the board every monster hunts it instead, and every one the gugs
reach puts doom on the sheet. Saving them is what buys the clues that find out what
the invasion is for, and the markers card 139 hides around Arkham name which of three
threads -- the captives, the phylactery, the seal -- can be cut. Cutting one opens card
144, and three markers on the sheet ends it.
-}
module AH3e.Content.SecretsOfTheOrder.TheDeadCryOutBehaviors (behaviors) where

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
import Data.List (find, nub)
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { codex =
        Map.fromList
          [ (135, shiftingPath)
          , (136, waveOfBlood)
          , (137, darkDisciples)
          , (138, darkScion)
          , (139, strangePantheon)
          , (140, darkRite)
          , (141, freeTheVessels)
          , (142, thePhylactery)
          , (143, reinforceTheSeal)
          , (144, atLast)
          ]
    , monsters =
        Map.fromList
          [ ("archive-145", mummifiedGug)
          , ("archive-146", seerOfMnar)
          ]
    , customEffects =
        Map.fromList
          [ ("the-dead-cry-out-reckoning", reckoning)
          , ("dco-bound-to-darkness", boundToDarkness)
          , ("dco-darkness-toll", darknessToll)
          , ("dco-vessels-freed", vesselsFreed)
          , ("dco-phylactery-found", phylacteryFound)
          , ("dco-phylactery-toll", phylacteryToll)
          , ("dco-search-goes-on", searchGoesOn)
          , ("dco-seer-disengaged", seerDisengaged)
          ]
    }

-- the scenario sheet

{- | "Place one ally card facedown in the street nearest the unstable space; then spawn
one monster in the unstable space." Another soul is put where the gugs will find it,
and something comes through to look for it.
-}
reckoning :: EffectCtx -> GameM ()
reckoning _ = do
  unstable <- unstableSpaces
  nearby <- case unstable of
    (sid : _) -> nearestSpacesMatching isStreetLike sid
    [] -> pure []
  pushAll
    $ [PlaceBystander sid | sid <- take 1 nearby]
    <> [SpawnMonsterAt (Just sid) False | sid <- take 1 unstable]

-- the places the scenario names

underworld :: NeighborhoodId
underworld = NeighborhoodId "the-underworld"

cityOfTheGugs, hiddenPath :: SpaceId
cityOfTheGugs = spaceIdFor "City of the Gugs"
hiddenPath = spaceIdFor "Hidden Path"

mummifiedGugCode, seerOfMnarCode :: CardCode
mummifiedGugCode = "archive-145"
seerOfMnarCode = "archive-146"

-- shared state

entryOf :: ArchiveNumber -> GameM (Maybe CodexEntry)
entryOf n = uses #codex (find ((== n) . (.number)))

inCodex :: ArchiveNumber -> GameM Bool
inCodex n = isJust <$> entryOf n

countedOn :: Text -> CodexEntry -> Int
countedOn key e = Map.findWithDefault 0 key e.tokens

scenarioCtx :: ArchiveNumber -> InvestigatorId -> EffectCtx
scenarioCtx n iid = EffectCtx {investigator = iid, source = SourceCodex n, testResult = Nothing}

bystandersOnBoard :: GameM [(CardId, SpaceId)]
bystandersOnBoard = uses #bystanders (fromMaybe [])

-- | Takes every face-up marker of that colour off the board, and says how many it found.
liftRevealed :: Text -> GameM Int
liftRevealed colour = do
  found <- length . filter (matching . snd) <$> allMarkers
  #board . #spaces . traversed . #markers %= filter (not . matching)
  pure found
 where
  matching m = m.faceUp && m.color == colour

-- | The space of a neighborhood holding the most doom, which is where a marker hides.
mostDoomIn :: NeighborhoodId -> Board -> Maybe SpaceId
mostDoomIn nid board = mostDoomOf [s | s <- Map.elems board.spaces, s.neighborhood == Just nid]

mostDoomOf :: [Space] -> Maybe SpaceId
mostDoomOf ss = (.id) <$> listToMaybe (sortOn (negate . (.doom)) ss)

-- | Whether that monster is anywhere on the board, which several cards ask.
onBoard :: CardCode -> GameM Bool
onBoard wanted = uses #monsters Map.elems >>= fmap (not . null) . filterM (fmap (== wanted) . cardCode . (.card))

{- | Takes an epic monster out of the box and puts it where the card says. Unlike an
ordinary spawn it is placed directly, because the card names the space.
-}
takeAside :: CardCode -> SpaceId -> GameM [Message]
takeAside wanted sid = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== wanted) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      pure [PlaceMonster cid sid Ready]
    _ -> [] <$ logText ("That monster is not in the box: " <> coerce wanted)

{- | What each of the three threads leaves behind when it is cut: the Seer is one step
more mortal, and the first thread to finish opens card 144. The rest only mean more
people in the gugs' way.
-}
threadCut :: ArchiveNumber -> GameM [Message]
threadCut n = do
  already <- inCodex 144
  unstable <- unstableSpaces
  pure
    $ (if already then [PlaceBystander sid | sid <- take 1 unstable] else [AddArchiveToCodex 144])
    <> [RemoveCodexCard n]

-- | A marker leaves the board for the scenario sheet, which is how the ending is counted.
markerToSheet :: Text -> GameM ()
markerToSheet colour = do
  logText ("A " <> colour <> " marker moves to the scenario sheet")
  push (MarkSheet 1)

-- 135

{- | The path itself. It is wild, not placed: every blank token lifts it off the board
and sets it down against the next corner of the Underworld, a random way round.
-}
shiftingPath :: CodexBehavior
shiftingPath =
  defaultCodexBehavior
    { tokenDrawn = \_ _ -> \case
        BlankToken -> pure [MoveCornerTile HiddenPath underworld]
        _ -> pure []
    }

-- 136

{- | Both sides send the monsters after the bystanders rather than the investigators,
and both make the party pay for every one the gugs reach. Only the front pays a clue
for a rescue; by the back the city has stopped talking to them.
-}
waveOfBlood :: CodexBehavior
waveOfBlood =
  defaultCodexBehavior
    { preyReplacement = \_ _ -> do
        bys <- bystandersOnBoard
        pure (if null bys then Nothing else Just (map snd bys))
    , atEndOfMonsterPhase = \_ -> do
        caught <- bystandersCaught
        pure
          $ concat
            [ [DiscardBystander cid, ExhaustMonster mid, PlaceDoomOnSheet 1]
            | (cid, mid) <- caught
            ]
    , componentActions =
        [ ComponentActionDef
            { label = "Help a bystander to safety"
            , allowedWhileEngaged = False
            , canPerform = \iid -> not . null <$> bystandersWith iid
            , perform = \ctx -> do
                here <- bystandersWith ctx.investigator
                flipped <- maybe False (.flipped) <$> entryOf 136
                chooseFor
                  ctx.investigator
                  "Choose a bystander to help"
                  [ Choice
                      (CardLabel cid)
                      (TakeBystander ctx.investigator cid : [AddSheetClues 1 | not flipped])
                  | cid <- here
                  ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "city-emptied"
            , once = True
            , condition = \e -> (\bys -> not e.flipped && null bys) <$> bystandersOnBoard
            , action = \_ -> do
                there <- inCodex 139
                pushAll ([AddArchiveToCodex 139 | not there] <> [FlipCodexCard 136])
            }
        ]
    , reckoning = \e -> if e.flipped then Just (Custom "dco-bound-to-darkness") else Nothing
    }

-- | The bystanders in an investigator's own space, which are the ones they can reach.
bystandersWith :: InvestigatorId -> GameM [CardId]
bystandersWith iid = do
  here <- investigatorSpace iid
  bys <- bystandersOnBoard
  pure [cid | (cid, sid) <- bys, Just sid == here]

{- | The bystanders the monsters have closed with, each with the monster that has it. A
monster already holding an investigator is busy with someone who can fight back, so the
bystander standing beside them is left alone -- which is what makes standing guard worth
doing. One monster can only be engaged with one of them, so two bystanders sharing a
space need two monsters to take them both.
-}
bystandersCaught :: GameM [(CardId, CardId)]
bystandersCaught = do
  bys <- bystandersOnBoard
  go [] bys
 where
  free = \case Engaged _ -> False; _ -> True
  go _ [] = pure []
  go taken ((cid, sid) : rest) = do
    ms <- monstersAt sid
    case [m.card | m <- ms, free m.state, m.card `notElem` taken] of
      (mid : _) -> ((cid, mid) :) <$> go (mid : taken) rest
      [] -> go taken rest

{- | 136's back. Every ally they have is one more thread for the dark to pull on, so the
luckier they have been the likelier the call comes.
-}
boundToDarkness :: EffectCtx -> GameM ()
boundToDarkness _ = do
  invs <- playingInvestigators
  called <- for invs \i -> do
    allies <- matchingAssets i.id AllyCard
    n <- rollDie
    d <- getInvestigatorDef i.id
    logText (d.name <> " rolls " <> tshow n)
    pure [ResolveEffect (scenarioCtx 136 i.id) (Custom "dco-darkness-toll") | n <= length allies]
  pushAll (concat called)

darknessToll :: EffectCtx -> GameM ()
darknessToll ctx = do
  allies <- matchingAssets ctx.investigator AllyCard
  unstable <- unstableSpaces
  chooseFor
    ctx.investigator
    "Something answers to your name"
    $ [Choice (CardLabel cid) [DiscardAsset cid] | cid <- allies]
    <> [ label "Place one doom in the unstable space" [PlaceDoom SourceScenario sid]
       | sid <- take 1 unstable
       ]

-- 137

{- | The deaths are feeding something. Once enough of them have, the beasts come with a
priest behind them.
-}
darkDisciples :: CodexBehavior
darkDisciples =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "disciples"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 3) <$> use #sheetDoom
            , action = \_ -> push (FlipCodexCard 137)
            }
        , CodexTrigger
            { key = "hatred"
            , once = True
            , condition = \e -> (\doom -> e.flipped && doom >= 9) <$> use #sheetDoom
            , action = \_ -> do
                there <- inCodex 138
                pushAll ([AddArchiveToCodex 138 | not there] <> [RemoveCodexCard 137])
            }
        ]
    , onFlip = \e -> when e.flipped do
        #cup %= (<> [SpawnMonsterToken, BlankToken])
        logText "A monster token and a blank token join the mythos cup"
        unstable <- unstableSpaces
        pushAll [PlaceBystander sid | sid <- take 1 unstable]
    }

-- 138

{- | The Seer takes the field itself, and the giant that led the beasts until now is
spent. Killing the Seer only sends it back to Kadath for a while: its card goes into the
headline deck, and whoever reads that headline finds it standing in the portal again.
-}
darkScion :: CodexBehavior
darkScion =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        unstable <- unstableSpaces
        spent <- spendTheGug
        arrival <- takeAside seerOfMnarCode cityOfTheGugs
        pushAll ([PlaceBystander sid | sid <- take 1 unstable] <> spent <> arrival)
    , afterMonsterDefeated = \_ mid _ -> do
        code <- cardCode mid
        if code /= seerOfMnarCode
          then pure []
          else do
            -- it is already off the board, wherever its own defeat left it
            #decks . #monster %= filter (/= mid)
            #decks . #removed %= filter (/= mid)
            deck <- use (#decks . #headline)
            #decks . #headline <~ shuffleIntoTopTwo mid deck
            logText "The Seer of Mnar is gone, for now"
            pure []
    , headlineReplacement = \_ _ cid -> do
        code <- cardCode cid
        if code /= seerOfMnarCode
          then pure Nothing
          else do
            unstable <- unstableSpaces
            logText "The Seer of Mnar steps back into Arkham"
            pure (Just [PlaceMonster cid sid Ready | sid <- take 1 unstable])
    , triggers =
        [ CodexTrigger
            { key = "kadath"
            , once = True
            , condition = \e -> (\doom -> not e.flipped && doom >= 13) <$> use #sheetDoom
            , action = \_ -> push (FlipCodexCard 138)
            }
        ]
    , onFlip = \e -> when e.flipped $ push (LoseTheGame "A Dark Scion")
    }

{- | "If the Mummified Gug epic monster is on the board, it deals one damage to each
investigator engaged with it. Then discard that monster." Its last act is to hurt
whoever stayed to fight it.
-}
spendTheGug :: GameM [Message]
spendTheGug = do
  ms <- uses #monsters Map.elems >>= filterM (fmap (== mummifiedGugCode) . cardCode . (.card))
  pure
    $ concat
      [ [SufferHarm iid (SourceMonster m.card) NormalHarm 1 0 | iid <- held m.state]
          <> [DiscardMonster m.card]
      | m <- ms
      ]
 where
  held = \case Engaged is -> is; _ -> []

-- 139

{- | The names the beasts call out are gods, and the clues that prove it bring the gug
priest down on the party. What it leaves exposed is the plot itself, hidden around
Arkham under six markers.
-}
strangePantheon :: CodexBehavior
strangePantheon =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "pantheon"
            , once = True
            , condition = \e -> (\clues -> not e.flipped && clues >= 3) <$> use #sheetClues
            , action = \_ -> push (FlipCodexCard 139)
            }
        ]
    , onFlip = \e -> when e.flipped do
        board <- use #board
        seer <- onBoard seerOfMnarCode
        unstable <- unstableSpaces
        giant <- if seer then pure [] else concat <$> for (take 1 unstable) (takeAside mummifiedGugCode)
        colours <- shuffle (concatMap (replicate 2) ["red", "blue", "green"])
        let hoods = [n.id | n <- Map.elems board.neighborhoods, n.town == Arkham]
            hiding = mapMaybe (`mostDoomIn` board) hoods
        pushAll
          $ giant
          <> [PlaceBystander sid | sid <- maybeToList (mostDoomOf (Map.elems board.spaces))]
          <> zipWith PlaceMarkerFacedown hiding colours
          <> [AddArchiveToCodex 140, RemoveCodexCard 139]
    }

-- 140

{- | What the markers are for. Reading two of them is enough to see that the plot runs
along three threads at once, and which of them can be reached.
-}
darkRite :: CodexBehavior
darkRite =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Spend two clues to read a marker here"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                clues <- use #sheetClues
                hidden <- hiddenMarkersWith iid
                pure (clues >= 2 && not (null hidden))
            , perform = \ctx -> do
                here <- investigatorSpace ctx.investigator
                pushAll (SpendSheetClues 2 : [RevealMarkerAt sid | sid <- maybeToList here])
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "two-colours"
            , once = True
            , condition = \e -> (\cs -> not e.flipped && length cs >= 2) <$> revealedColours
            , action = \_ -> push (FlipCodexCard 140)
            }
        ]
    , onFlip = \e -> when e.flipped do
        colours <- revealedColours
        #board . #spaces . traversed . #markers %= filter (.faceUp)
        logText "What is left unread is swept away"
        pushAll
          $ [AddArchiveToCodex 141 | "red" `elem` colours]
          <> [AddArchiveToCodex 142 | "blue" `elem` colours]
          <> [AddArchiveToCodex 143 | "green" `elem` colours]
          <> [RemoveCodexCard 140]
    }

hiddenMarkersWith :: InvestigatorId -> GameM [Marker]
hiddenMarkersWith iid = do
  here <- investigatorSpace iid
  ms <- maybe (pure []) markersAt here
  pure [m | m <- ms, not m.faceUp]

-- | The colours the party has read off the board so far.
revealedColours :: GameM [Text]
revealedColours = nub . map ((.color) . snd) . filter ((.faceUp) . snd) <$> allMarkers

-- 141

{- | The red thread: the Great Ones need bodies to come back into, and the gugs have
taken them from the oldest families in Arkham. The captives are held in the City of the
Gugs, and the more of them there are the easier they are to find.
-}
freeTheVessels :: CodexBehavior
freeTheVessels =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        moved <- liftRevealed "red"
        pushAll (replicate moved (PlaceMarker cityOfTheGugs "red"))
    , componentActions =
        [ ComponentActionDef
            { label = "Lead the captives out"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 141
                here <- investigatorSpace iid
                clues <- use #sheetClues
                cost <- vesselCost
                pure (maybe False (not . (.flipped)) me && here == Just cityOfTheGugs && clues >= cost)
            , perform = \ctx -> do
                cost <- vesselCost
                pushAll
                  [ SpendSheetClues cost
                  , BeginTest
                      ( newTest
                          ctx.investigator
                          Observation
                          0
                          (ActionTest (ComponentAction (CodexRef 141) 0) Nothing)
                          (AfterEffect ctx (Custom "dco-vessels-freed") NoEffect)
                      )
                  ]
            }
        ]
    , onFlip = \e -> when e.flipped do
        left <- length . filter (\(_, m) -> m.color == "red") <$> allMarkers
        #board . #spaces . traversed . #markers %= filter ((/= "red") . (.color))
        when (left > 0) $ markerToSheet "red"
        threadCut 141 >>= pushAll
    }

-- | Two of the captives together are easier to reach than one on its own.
vesselCost :: GameM Int
vesselCost = do
  held <- length . filter ((== "red") . (.color)) <$> markersAt cityOfTheGugs
  pure (if held >= 2 then 1 else 2)

vesselsFreed :: EffectCtx -> GameM ()
vesselsFreed _ = push (FlipCodexCard 141)

-- 142

{- | The blue thread: the Seer cannot be killed while its soul is kept somewhere else,
and that somewhere is in the Underworld. One of three cards says which of its three
places, and it goes on top of the deck so the search carries on where it left off.
-}
thePhylactery :: CodexBehavior
thePhylactery =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        found <- liftRevealed "blue"
        pushAll (replicate found (PlaceNeighborhoodMarker underworld "blue" True))
        hunt <- use (#decks . #setAside) >>= filterM (fmap (`elem` huntCards) . cardCode)
        chosen <- pickRandom hunt
        for_ chosen \cid -> do
          #decks . #setAside %= filter (/= cid)
          deck <- use (#decks . #neighborhoods . at underworld . non [])
          if found >= 2
            then do
              #decks . #neighborhoods . at underworld ?= (cid : deck)
              neighborhoodL underworld . #markers %= dropFirstMarker ((== "blue") . (.color))
              logText "Two of the captives agree on where the reliquary is kept"
            else do
              mixed <- shuffleIntoTopTwo cid deck
              #decks . #neighborhoods . at underworld ?= mixed
              logText "Somewhere in the Underworld, the Seer keeps its soul"
    , onFlip = \e -> when e.flipped (threadCut 142 >>= pushAll)
    }

huntCards :: [CardCode]
huntCards = ["archive-147", "archive-148", "archive-149"]

{- | The phylactery is found. The marker comes off the Underworld and onto the sheet, and
the card leaves the deck for good -- but what was stored in the thing has to go
somewhere, and only a clue spent now keeps it off the party.
-}
phylacteryFound :: EffectCtx -> GameM ()
phylacteryFound ctx = do
  #encounter . _Just . #returnToArchive .= True
  held <- uses (#board . #neighborhoods . ix underworld . #markers) (filter ((== "blue") . (.color)))
  unless (null held) do
    neighborhoodL underworld . #markers %= dropFirstMarker ((== "blue") . (.color))
    markerToSheet "blue"
  push (ResolveEffect ctx (Custom "dco-phylactery-toll"))

phylacteryToll :: EffectCtx -> GameM ()
phylacteryToll _ = do
  clues <- use #sheetClues
  invs <- playingInvestigators
  let harm = [SufferHarm i.id (SourceCodex 142) NormalHarm 1 1 | i <- invs]
  if clues >= 1
    then
      chooseGroup
        "The light stored in the reliquary has to go somewhere"
        [ label "Spend one clue from the scenario sheet" [SpendSheetClues 1, FlipCodexCard 142]
        , label "Let it out" (harm <> [FlipCodexCard 142])
        ]
    else pushAll (harm <> [FlipCodexCard 142])

-- | The reliquary was not in this place, so the search begins again from the top.
searchGoesOn :: EffectCtx -> GameM ()
searchGoesOn _ = #encounter . _Just . #returnToTop ?= True

-- 143

{- | The green thread: the old seal between the worlds is what is letting the beasts
through at all, and warding the portal itself is what mends it.
-}
reinforceTheSeal :: CodexBehavior
reinforceTheSeal =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        moved <- liftRevealed "green"
        pushAll [MarkCodexToken 143 "green" moved | moved > 0]
    , reactions = \e iid -> \case
        AfterWardResult warder _
          | not e.flipped
          , warder == iid -> do
              here <- investigatorSpace iid
              unstable <- unstableSpaces
              clues <- use #sheetClues
              pure
                [ Reaction
                    { key = "dco-seal"
                    , label = "Spend one clue to reinforce the seal"
                    , messages = [SpendSheetClues 1, MarkCodexToken 143 "green" 1]
                    }
                | clues >= 1
                , maybe False (`elem` unstable) here
                ]
        _ -> pure []
    , triggers =
        [ CodexTrigger
            { key = "sealed"
            , once = True
            , condition = \e -> pure (not e.flipped && countedOn "green" e >= 3)
            , action = \e -> do
                markerToSheet "green"
                pushAll [MarkCodexToken 143 "green" (-(countedOn "green" e)), FlipCodexCard 143]
            }
        ]
    , onFlip = \e -> when e.flipped (threadCut 143 >>= pushAll)
    }

-- 144

{- | Whichever thread was cut, the Seer is mortal now. Severing it from Kadath has to be
done where the two worlds touch, and it costs the party their own luck in the cup to do
it.
-}
atLast :: CodexBehavior
atLast =
  defaultCodexBehavior
    { onAdd = \e -> unless e.flipped do
        unstable <- unstableSpaces
        pushAll [PlaceBystander sid | sid <- take 1 unstable]
    , componentActions =
        [ ComponentActionDef
            { label = "Sever the Seer from the Great Ones"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                me <- entryOf 144
                here <- investigatorSpace iid
                clues <- use #sheetClues
                pure (maybe False (not . (.flipped)) me && here == Just hiddenPath && clues >= 2)
            , perform = \ctx ->
                pushAll
                  [ SpendSheetClues 2
                  , ResolveEffect ctx (DrawMythosTokens 2)
                  , MarkSheet 1
                  , LogText "A white marker joins the scenario sheet"
                  ]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "severed"
            , once = True
            , condition = \e -> (\marked -> not e.flipped && marked >= 3) <$> use #sheetMarkers
            , action = \_ -> push (FlipCodexCard 144)
            }
        ]
    , onFlip = \e -> when e.flipped $ pushAll [LogText "At Last: the Seer crumbles to ash", WinTheGame]
    }

-- the two gug priests

{- | Card 145. Walking away from it lets more of the portal through, and it leaves the
same mark behind wherever it lurks.
-}
mummifiedGug :: MonsterBehavior
mummifiedGug =
  defaultMonsterBehavior
    { afterDisengage = \_ _ -> do
        unstable <- unstableSpaces
        pure [PlaceDoom SourceScenario sid | sid <- take 1 unstable]
    }

{- | Card 146. Coming away from the Seer means turning your back on it, and it is the
mythos that answers. Defeating it does not retire it either: card 138 takes its card and
puts it back in the headline deck.
-}
seerOfMnar :: MonsterBehavior
seerOfMnar =
  defaultMonsterBehavior
    { removedWhenDefeated = True
    , afterDisengage = \mid iid ->
        pure [ResolveEffect (EffectCtx iid (SourceMonster mid) Nothing) (Custom "dco-seer-disengaged")]
    }

seerDisengaged :: EffectCtx -> GameM ()
seerDisengaged ctx = push (ResolveEffect ctx (DrawMythosTokens 2))

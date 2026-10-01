-- | Dreams of R'lyeh's own mechanics: the town the investigation turns up.
module AH3e.Content.UnderDarkWaves.DreamsOfRlyehBehaviors (behaviors) where

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
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (106, songOfChaos)
      , (107, maddeningMelody)
      , (108, theCultRevealed)
      , (117, arrival 117 card117)
      , (118, arrival 118 card118)
      , (119, arrival 119 card119)
      , (120, arrival 120 card120)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("rlyeh-research", \ctx -> push (ResearchCluesExact ctx.investigator 1))
      , ("rlyeh-blank-token", \_ -> returnTokensToCup [BlankToken])
      ]
    & #customPredicates
    .~ Map.fromList [("rlyeh-white-marker", standsOnWhite)]

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

{- | Cards 117-120 are the investigation deck. Only one of them ever reaches the
codex, so a scenario sees either Innsmouth or Kingsport, never both. Both 106 and
107 start in the codex and either may need the deck, so whichever is added first
sets it out.
-}
investigationCards :: [ArchiveNumber]
investigationCards = [117, 118, 119, 120]

seedInvestigation :: GameM ()
seedInvestigation = do
  deck <- use (#decks . #investigation)
  taken <- uses #codex (map (.number))
  when (null deck && not (any (`elem` taken) investigationCards))
    $ #decks
    . #investigation
    .= investigationCards

-- | Reveal one of the cards still in the investigation deck and return it there.
drawInvestigation :: GameM (Maybe ArchiveNumber)
drawInvestigation = do
  deck <- use (#decks . #investigation)
  shuffled <- shuffle deck
  case shuffled of
    [] -> pure Nothing
    n : rest -> do
      #decks . #investigation .= rest
      logText ("The investigation turns up archive card " <> tshow n)
      pure (Just n)

{- | Card 106. Its front waits on two clues; its back puts down the white markers
the melody leaves behind and lets an investigator standing on one discard it to
rule a card out, until only one card is left to turn up.
-}
songOfChaos :: CodexBehavior
songOfChaos =
  defaultCodexBehavior
    { onAdd = const seedInvestigation
    , triggers =
        [ CodexTrigger
            { key = "song-of-chaos"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 2)
            , action = \_ -> push (FlipCodexCard 106)
            }
        , CodexTrigger
            { key = "song-of-chaos-last"
            , once = True
            , condition = \e -> do
                deck <- use (#decks . #investigation)
                pure (e.flipped && length deck == 1)
            , action = \_ -> do
                deck <- use (#decks . #investigation)
                for_ deck \n -> do
                  #decks . #investigation .= []
                  pushAll [AddArchiveToCodex n, RemoveCodexCard 106, RemoveCodexCard 107]
            }
        ]
    , onFlip = \e -> when e.flipped markEveryNeighborhood
    , componentActions =
        [ ComponentActionDef
            { label = "Discard a white marker to rule out one line of investigation"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                flipped <- uses #codex (any (\e -> e.number == 106 && e.flipped))
                here <- whiteMarkerUnder iid
                deck <- use (#decks . #investigation)
                pure (flipped && isJust here && length deck > 1)
            , perform = \ctx -> do
                msid <- whiteMarkerUnder ctx.investigator
                for_ msid \sid -> do
                  spaceL sid . #markers %= dropOne "white"
                  void drawInvestigation
                  clues <- use #sheetClues
                  when (clues >= 1)
                    $ chooseFor
                      ctx.investigator
                      "Spend one clue from the scenario sheet?"
                      [ label
                          "Add one blank token to the mythos cup"
                          [SpendSheetClues 1, ResolveEffect ctx (Custom "rlyeh-blank-token")]
                      , label "Keep the clue" []
                      ]
            }
        ]
    }

-- | "the space with the most doom in each neighborhood that does not contain a white marker"
markEveryNeighborhood :: GameM ()
markEveryNeighborhood = do
  board <- use #board
  for_ (Map.elems board.neighborhoods) \n ->
    unless (any (hasWhite board) n.spaces)
      $ for_ (mostDoom board n.spaces) \sid -> push (PlaceMarker sid "white")

-- | The one neighborhood the melody has not reached yet, if there is one.
markOneNeighborhood :: GameM ()
markOneNeighborhood = do
  board <- use #board
  let bare = [n | n <- Map.elems board.neighborhoods, not (any (hasWhite board) n.spaces)]
  for_ (take 1 bare) \n -> for_ (mostDoom board n.spaces) \sid -> push (PlaceMarker sid "white")

hasWhite :: Board -> SpaceId -> Bool
hasWhite board sid = maybe False (any ((== "white") . (.color)) . (.markers)) (Map.lookup sid board.spaces)

mostDoom :: Board -> [SpaceId] -> Maybe SpaceId
mostDoom board sids = case sortOn (negate . doomAt) sids of
  [] -> Nothing
  sid : _ -> Just sid
 where
  doomAt sid = maybe 0 (.doom) (Map.lookup sid board.spaces)

whiteMarkerUnder :: InvestigatorId -> GameM (Maybe SpaceId)
whiteMarkerUnder iid = do
  board <- use #board
  msid <- investigatorSpace iid
  pure (msid >>= \sid -> if hasWhite board sid then Just sid else Nothing)

dropOne :: Text -> [Marker] -> [Marker]
dropOne colour ms = case break ((== colour) . (.color)) ms of
  (before, _ : after) -> before <> after
  _ -> ms

{- | Card 107. Its front leaves a white marker behind each time doom reaches the
scenario sheet, and gives anyone standing on one a way to turn their own clue into
a sheet clue. Its back decides which town the investigators end up in.
-}
maddeningMelody :: CodexBehavior
maddeningMelody =
  defaultCodexBehavior
    { onAdd = const seedInvestigation
    , triggers =
        [ {- The card answers each doom reaching the sheet. Counting the markers
          already down against the doom already there comes to the same thing,
          and needs no memory of its own. -}
          CodexTrigger
            { key = "maddening-melody-marker"
            , once = False
            , condition = \e -> do
                doom <- use #sheetDoom
                white <- whiteMarkers
                pure (not e.flipped && white < doom)
            , action = const markOneNeighborhood
            }
        , CodexTrigger
            { key = "maddening-melody"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 4)
            , action = \_ -> push (FlipCodexCard 107)
            }
        ]
    , -- "You may suffer one direct horror to research a clue."
      spaceEncounter = \e sid ->
        if e.flipped || sid == SpaceId ""
          then Nothing
          else Just (If (CustomPredicate "rlyeh-white-marker") melodyResearch NoEffect)
    , onFlip = \e -> when e.flipped do
        drawn <- drawInvestigation
        #decks . #investigation .= []
        for_ drawn \n ->
          pushAll [AddArchiveToCodex n, SpendSheetClues 2, RemoveCodexCard 106, RemoveCodexCard 107]
    }

melodyResearch :: Effect
melodyResearch =
  May
    "Suffer one direct horror to research a clue"
    (Seq [DirectHorror (N 1), Custom "rlyeh-research"])

whiteMarkers :: GameM Int
whiteMarkers = do
  board <- use #board
  pure (length [m | s <- Map.elems board.spaces, m <- s.markers, m.color == "white"])

{- | Card 108's back: "Take cards 109-112 from the archive, shuffle them, and
randomly add one of them to the codex." Which one the investigators get is how the
scenario is won.
-}
theCultRevealed :: CodexBehavior
theCultRevealed =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "cult-revealed"
            , once = True
            , condition = \e -> do
                clues <- use #sheetClues
                pure (not e.flipped && clues >= 2)
            , action = \_ -> pushAll [SpendSheetClues 2, FlipCodexCard 108]
            }
        ]
    , onFlip = \e -> when e.flipped do
        drawn <- shuffle [109, 110, 111, 112]
        for_ (take 1 drawn) \n -> pushAll [AddArchiveToCodex n, RemoveCodexCard 108]
    }

{- | What one of cards 117-120 brings with it. All four print the same
instructions over a different map, so they differ only in the two tiles, the edge
the travel route hangs off, and where the markers, the doom and the monster go.
-}
data Arrival = Arrival
  { against :: NeighborhoodId
  -- ^ the tile already on the board that the pair is laid against
  , edge :: Edge
  , tiles :: [NeighborhoodId]
  , street :: StreetDef
  , route :: RouteDef
  , events :: [CardCode]
  -- ^ the town's event cards, set aside until its tiles arrive
  , red :: Text
  , blue :: Text
  , doomAt :: Text
  , monsterAt :: Text
  }

-- | Innsmouth arrives above Arkham, its two tiles side by side.
innsmouth :: Arrival
innsmouth =
  Arrival
    { against = nb "Miskatonic University"
    , edge = TopRight
    , tiles = [nb "Innsmouth Village", nb "Innsmouth Shore"]
    , street = StreetDef (nb "Innsmouth Village") SideRight (nb "Innsmouth Shore") Scenic
    , route = RouteDef (nb "Innsmouth Village") BottomLeft CountryRoad
    , events = eventCodes [5 .. 12]
    , red = ""
    , blue = ""
    , doomAt = ""
    , monsterAt = ""
    }

-- | Kingsport arrives below Arkham, its harbor set down and to the right.
kingsport :: Arrival
kingsport =
  Arrival
    { against = nb "Uptown"
    , edge = BottomRight
    , tiles = [nb "Central Kingsport", nb "Kingsport Harbor"]
    , street = StreetDef (nb "Central Kingsport") BottomRight (nb "Kingsport Harbor") Scenic
    , route = RouteDef (nb "Central Kingsport") TopRight TrainPlatform
    , events = eventCodes ([1 .. 4] <> [13 .. 16])
    , red = ""
    , blue = ""
    , doomAt = ""
    , monsterAt = ""
    }

card117, card118, card119, card120 :: Arrival
card117 =
  innsmouth
    { route = RouteDef (nb "Innsmouth Village") BottomLeft CountryRoad
    , red = "Esoteric Order of Dagon"
    , blue = "Gilman House"
    , doomAt = "Falcon Point"
    , monsterAt = "Innsmouth Jail"
    }
card118 =
  kingsport
    { route = RouteDef (nb "Central Kingsport") TopRight TrainPlatform
    , red = "North Point Lighthouse"
    , blue = "Hall School"
    , doomAt = "Congregational Hospital"
    , monsterAt = "The Rope and Anchor"
    }
card119 =
  innsmouth
    { route = RouteDef (nb "Innsmouth Shore") BottomRight FerryTerminal
    , red = "Marsh Refinery"
    , blue = "Innsmouth Jail"
    , doomAt = "Esoteric Order of Dagon"
    , monsterAt = "Gilman House"
    }
card120 =
  kingsport
    { route = RouteDef (nb "Kingsport Harbor") SideRight FerryTerminal
    , red = "Congregational Hospital"
    , blue = "St. Erasmus's Home"
    , doomAt = "North Point Lighthouse"
    , monsterAt = "Hall School"
    }

eventCodes :: [Int] -> [CardCode]
eventCodes ns = [CardCode ("rlyeh-event-" <> (if n < 10 then "0" else "") <> tshow n) | n <- ns]

{- | "Add ... to the board and place one red and one blue marker as shown below.
Shuffle the event discard pile and the set-aside event cards into the event deck.
Place one doom in the indicated space for each white marker in Arkham, then
discard all white markers. Spawn one monster in the indicated space. Then flip
this card." The flip comes last, so the card is read front up before any of it.
-}
arrival :: ArchiveNumber -> Arrival -> CodexBehavior
arrival n a =
  defaultCodexBehavior
    { onAdd = \_ -> do
        shuffleIn a.events
        white <- whiteMarkersInArkham
        pushAll
          $ [ AddToBoard a.against (addedMap a.against a.edge a.tiles [a.street] [a.route])
            , PlaceMarker (spaceIdFor a.red) "red"
            , PlaceMarker (spaceIdFor a.blue) "blue"
            ]
          <> replicate white (PlaceDoom (SourceCodex n) (spaceIdFor a.doomAt))
          <> [ DiscardMarkers "white"
             , SpawnMonsterAt (Just (spaceIdFor a.monsterAt)) False
             , FlipCodexCard n
             ]
    }

-- | The town's events join the deck along with everything already discarded.
shuffleIn :: [CardCode] -> GameM ()
shuffleIn codes = do
  aside <- use (#decks . #setAside)
  theirs <- filterM (fmap (`elem` codes) . cardCode) aside
  discard <- use (#decks . #eventDiscard)
  deck <- use (#decks . #event)
  #decks . #setAside %= filter (`notElem` theirs)
  #decks . #eventDiscard .= []
  #decks . #event <~ shuffle (deck <> discard <> theirs)

-- | "for each white marker in Arkham", counted over the spaces of Arkham's tiles.
whiteMarkersInArkham :: GameM Int
whiteMarkersInArkham = do
  board <- use #board
  let arkham = [n.id | n <- Map.elems board.neighborhoods, n.town == Arkham]
      inArkham s = maybe False (`elem` arkham) s.neighborhood
  pure (length [m | s <- Map.elems board.spaces, inArkham s, m <- s.markers, m.color == "white"])

-- | "only be performed by an investigator in a space with a white marker"
standsOnWhite :: EffectCtx -> GameM Bool
standsOnWhite ctx = isJust <$> whiteMarkerUnder ctx.investigator

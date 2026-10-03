-- | Dreams of R'lyeh's own mechanics: the town the investigation turns up.
module AH3e.Content.UnderDarkWaves.DreamsOfRlyehBehaviors (behaviors) where

import AH3e.Content.Tiles
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

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (106, songOfChaos)
      , (107, maddeningMelody)
      , (108, theCultRevealed)
      , (109, cutOffTheHead)
      , (110, callForHelp)
      , (111, endlessSong)
      , (112, razeTheShrine)
      , (113, aDarkHerald)
      , (114, cripplingVisions)
      , (115, bloodSacrament)
      , (116, aDarkAlliance)
      , (117, arrival 117 card117)
      , (118, arrival 118 card118)
      , (119, arrival 119 card119)
      , (120, arrival 120 card120)
      ]
    & #customEffects
    .~ Map.fromList
      [ ("rlyeh-research", \ctx -> push (ResearchCluesExact ctx.investigator 1))
      , ("rlyeh-blank-token", \_ -> returnTokensToCup [BlankToken])
      , ("rlyeh-bomb", bombReckoning)
      , ("rlyeh-doom-ritual-site", doomAtRitualSite)
      , ("rlyeh-monster-shrine", monsterAtShrine)
      ]
    & #customAfterTests
    .~ Map.fromList
      ( ("call-for-help", \_ r -> when (r >= 5) (push (FlipCodexCard 110)))
          : [ ("hold-" <> tshow (coerce n :: Int), heldBackTheEnd n against)
            | (n, against) <- [(113, 117), (114, 118), (115, 119), (116, 120)]
            ]
      )
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

seedInvestigation :: CodexEntry -> GameM ()
seedInvestigation e = setInvestigation e.number investigationCards

{- | Card 106. Its front waits on two clues; its back puts down the white markers
the melody leaves behind and lets an investigator standing on one discard it to
rule a card out, until only one card is left to turn up.
-}
songOfChaos :: CodexBehavior
songOfChaos =
  defaultCodexBehavior
    { onAdd = seedInvestigation
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
                  push (RevealArchiveCard n [AddArchiveToCodex n, RemoveCodexCard 106, RemoveCodexCard 107])
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
                  ruled <- revealInvestigation
                  for_ ruled \k -> push (RevealArchiveCard k [])
                  {- Ruling one out may leave a single card, which this card then turns up
                  of its own accord; nothing else looks at the pile, so the trigger is
                  read here. It is queued behind the card just revealed. -}
                  push CheckStateTriggers
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
    { onAdd = seedInvestigation
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
        drawn <- revealInvestigation
        #decks . #investigation .= []
        for_ drawn \n ->
          push
            ( RevealArchiveCard
                n
                [AddArchiveToCodex n, SpendSheetClues 2, RemoveCodexCard 106, RemoveCodexCard 107]
            )
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

{- | Cards 109-112, one of which card 108 deals out: the four ways the cult can be
broken. Each ends the game the moment its own condition is met.
-}
cutOffTheHead, callForHelp, endlessSong, razeTheShrine :: CodexBehavior

{- | 109. Cthulhu is called up at the ritual site and then drowned in the clues
that would otherwise have gone to the scenario sheet.
-}
cutOffTheHead =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Call Cthulhu up at the ritual site"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                there <- standsOnMarked "blue" iid
                up <- inPlay "echoes-40"
                pure (there && not up)
            , perform = \ctx -> do
                msid <- investigatorSpace ctx.investigator
                for_ msid (spawnHeldBack "echoes-40" >=> pushAll)
            }
        ]
    , -- "Each time a clue would be added to the scenario sheet, instead deal four
      -- damage to the Cthulhu epic monster."
      sheetClueReplacement = \_ n -> do
        ms <- uses #monsters Map.keys
        codes <- for ms \mid -> (mid,) <$> cardCode mid
        pure case [mid | (mid, code) <- codes, code == "echoes-40"] of
          [] -> Nothing
          mid : _ -> Just (replicate n (DealMonsterDamage mid (SourceCodex 109) 4))
    , afterMonsterDefeated = \e mid _ -> do
        code <- cardCode mid
        pure [FlipCodexCard 109 | code == "echoes-40" && not e.flipped]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }

{- | 110. A call from the cultist shrine, with the sheet's clues spent to make the
case stick.
-}
callForHelp =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Call in the Feds"
            , allowedWhileEngaged = False
            , canPerform = standsOnMarked "red"
            , perform = \ctx -> do
                clues <- use #sheetClues
                chooseFor
                  ctx.investigator
                  "Spend clues from the scenario sheet to add that many successes"
                  [ Choice
                      (TextLabel (if k == 0 then "Spend no clues" else tshow k <> " clue" <> plural k))
                      ([SpendSheetClues k | k > 0] <> [BeginTest (attempt ctx k)])
                  | k <- [0 .. clues]
                  ]
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }
 where
  attempt ctx k =
    (newTest ctx.investigator Influence (-2) OtherTest (AfterCustom (SourceCodex 110) "call-for-help"))
      { addedSuccesses = k
      }

{- | 111. The song is answered space by space until the ritual site's whole
neighborhood is singing back.
-}
endlessSong =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Spend a clue from the scenario sheet to sing back"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                clues <- use #sheetClues
                board <- use #board
                here <- investigatorSpace iid
                let quiet sid = maybe False (\s -> s.doom == 0 && not (hasWhite board sid)) (Map.lookup sid board.spaces)
                pure (clues >= 1 && maybe False quiet here)
            , perform = \ctx -> do
                msid <- investigatorSpace ctx.investigator
                for_ msid \sid -> pushAll [SpendSheetClues 1, PlaceMarker sid "white"]
            }
        ]
    , triggers =
        [ CodexTrigger
            { key = "endless-song"
            , once = True
            , condition = \e -> do
                board <- use #board
                site <- markerSpace "blue"
                let hood = site >>= \sid -> Map.lookup sid board.spaces >>= (.neighborhood)
                    spaces = maybe [] (\n -> maybe [] (.spaces) (Map.lookup n board.neighborhoods)) hood
                pure (not e.flipped && not (null spaces) && all (hasWhite board) spaces)
            , action = \_ -> push (FlipCodexCard 111)
            }
        ]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }

{- | 112. The bomb is left at the shrine and has to last until the reckoning.
Every monster makes for it as though an investigator were standing there, and the
first one to reach it tears it apart.
-}
razeTheShrine =
  defaultCodexBehavior
    { componentActions =
        [ ComponentActionDef
            { label = "Set the bomb at the cultist shrine"
            , allowedWhileEngaged = False
            , canPerform = \iid -> do
                clues <- use #sheetClues
                there <- standsOnMarked "red" iid
                planted <- markerSpace "bomb"
                pure (there && clues >= 2 && isNothing planted)
            , perform = \ctx -> do
                msid <- investigatorSpace ctx.investigator
                for_ msid \sid -> do
                  drawn <- use #drawnTokens
                  #drawnTokens .= []
                  returnTokensToCup drawn
                  pushAll [SpendSheetClues 2, PlaceMarker sid "bomb"]
            }
        ]
    , -- "All monsters consider the bomb to be their prey and destination"
      quarrySpaces = \_ -> maybeToList <$> markerSpace "bomb"
    , -- "If the bomb suffers any damage from a monster in its space, it is discarded."
      afterMonsterArrives = \_ _ sid -> do
        bomb <- markerSpace "bomb"
        pure [DiscardMarkers "bomb" | bomb == Just sid]
    , onFlip = \e -> when e.flipped (push WinTheGame)
    }
    & #reckoning
    .~ \_ -> Just (Custom "rlyeh-bomb")

plural :: Int -> Text
plural k = if k > 1 then "s" else ""

-- | "Reckoning -- If the bomb is at the cultist shrine, flip this card."
bombReckoning :: EffectCtx -> GameM ()
bombReckoning _ = do
  planted <- markerSpace "bomb"
  shrine <- markerSpace "red"
  when (isJust planted && planted == shrine) $ push (FlipCodexCard 112)

{- | Cards 113-116: the end drawing in. Each spawns or stirs something on its
front, flips at ten doom on the sheet, and then asks one investigator to hold it
back at a price. Each tests against the doom already gathered on the town card
the investigation turned up, so holding out twice costs more than once.
-}
theEnd
  :: ArchiveNumber
  -> ArchiveNumber
  -> Skill
  -> Text
  -> (InvestigatorId -> [Message])
  -> CodexBehavior
  -> CodexBehavior
theEnd n against skill priceLabel price base =
  base
    { triggers = flipOnSheetDoom ("end-" <> tshow (coerce n :: Int)) 10 n : base.triggers
    , onFlip = \e ->
        if e.flipped
          then do
            held <- doomOn against
            leader <- leaderPlayer >>= investigatorOfPlayer
            case leader of
              Nothing -> push (LoseTheGame "The end comes")
              Just iid ->
                chooseFor
                  iid
                  ("Hold the end at bay? You must beat " <> tshow held)
                  [ label priceLabel (price iid <> [BeginTest (attempt iid)])
                  , label "Let it come" [LoseTheGame "The end comes"]
                  ]
          else base.onFlip e
    }
 where
  attempt iid =
    newTest iid skill 0 OtherTest (AfterCustom (SourceCodex n) ("hold-" <> tshow (coerce n :: Int)))

-- | Whether the attempt beat the doom already on the town card.
heldBackTheEnd :: ArchiveNumber -> ArchiveNumber -> Source -> Int -> GameM ()
heldBackTheEnd n against _ result = do
  held <- doomOn against
  if result > held
    then do
      doomOntoCard against
      push (FlipCodexCard n)
    else push (LoseTheGame "The end comes")

aDarkHerald, cripplingVisions, bloodSacrament, aDarkAlliance :: CodexBehavior

-- | 113. The Servitor of R'lyeh wades ashore at Falcon Point.
aDarkHerald =
  theEnd
    113
    117
    Lore
    "Suffer two direct horror"
    (\iid -> [SufferHarm iid (SourceCodex 113) DirectHarm 0 2])
    $ defaultCodexBehavior
      { onAdd = \_ -> spawnHeldBack "echoes-39" (spaceIdFor "Falcon Point") >>= pushAll
      }

-- | 114. Warding the board back is answered with visions, and the ritual site drinks doom.
cripplingVisions =
  theEnd 114 118 Will "Discard one focus" (\iid -> [ResolveEffect (codexCtx 114 iid) DiscardAFocus])
    $ defaultCodexBehavior
    & #reckoning
    .~ \_ -> Just (Custom "rlyeh-doom-ritual-site")

-- | 115. The cup turns against them and the shrine keeps producing.
bloodSacrament =
  theEnd
    115
    119
    Strength
    "Suffer two direct damage"
    (\iid -> [SufferHarm iid (SourceCodex 115) DirectHarm 2 0])
    $ defaultCodexBehavior
      { onAdd = \_ -> do
          #cup %= replaceOne BlankToken SpawnMonsterToken
          logText "A blank token leaves the mythos cup for a spawn monster token"
      }
    & #reckoning
    .~ \_ -> Just (Custom "rlyeh-monster-shrine")
 where
  replaceOne from to' = \case
    [] -> []
    x : xs | x == from -> to' : xs
    x : xs -> x : replaceOne from to' xs

-- | 116. Mother Hydra comes up under the lighthouse.
aDarkAlliance =
  theEnd
    116
    120
    Will
    "Suffer one direct damage and one direct horror"
    (\iid -> [SufferHarm iid (SourceCodex 116) DirectHarm 1 1])
    $ defaultCodexBehavior
      { onAdd = \_ -> spawnHeldBack "archive-75" (spaceIdFor "North Point Lighthouse") >>= pushAll
      }

codexCtx :: ArchiveNumber -> InvestigatorId -> EffectCtx
codexCtx n iid = EffectCtx {investigator = iid, source = SourceCodex n, testResult = Nothing}

-- | 114's reckoning: "Place one doom at the ritual site."
doomAtRitualSite :: EffectCtx -> GameM ()
doomAtRitualSite ctx = markerSpace "blue" >>= traverse_ (push . PlaceDoom ctx.source)

-- | 115's reckoning: "Spawn one monster at the cultist shrine."
monsterAtShrine :: EffectCtx -> GameM ()
monsterAtShrine _ = markerSpace "red" >>= traverse_ \sid -> push (SpawnMonsterAt (Just sid) False)

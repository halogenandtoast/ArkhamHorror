-- | Dreams of R'lyeh's own mechanics: the town the investigation turns up.
module AH3e.Content.UnderDarkWaves.DreamsOfRlyehBehaviors (behaviors) where

import AH3e.Content.Tiles
import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    & #codex
    .~ Map.fromList
      [ (107, maddeningMelody)
      , (117, arrival 117 card117)
      , (118, arrival 118 card118)
      , (119, arrival 119 card119)
      , (120, arrival 120 card120)
      ]

nb :: Text -> NeighborhoodId
nb = NeighborhoodId . coerce . spaceIdFor

{- | Cards 117-120 are the investigation deck. Only one of them ever reaches the
codex, so a scenario sees either Innsmouth or Kingsport, never both.
-}
investigation :: [ArchiveNumber]
investigation = [117, 118, 119, 120]

{- | Card 107's back: "Add one card at random from the investigation deck to the
codex and return the others to the archive." This is what decides which town the
investigators end up in.
-}
maddeningMelody :: CodexBehavior
maddeningMelody =
  defaultCodexBehavior
    { triggers =
        [ CodexTrigger
            { key = "maddening-melody"
            , once = True
            , condition = \e -> do
                doom <- use #sheetDoom
                pure (not e.flipped && doom >= 4)
            , action = \_ -> push (FlipCodexCard 107)
            }
        ]
    , onFlip = \e -> when e.flipped do
        drawn <- shuffle investigation
        for_ (take 1 drawn) \n -> pushAll [AddArchiveToCodex n, RemoveCodexCard 107]
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

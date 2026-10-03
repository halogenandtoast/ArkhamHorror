{- | What Under Dark Waves' codex cards keep doing: fetching an epic monster the
box held back, finding the space a card marked, and counting the doom a card has
gathered on itself.
-}
module AH3e.Content.UnderDarkWaves.Codex where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Ids
import AH3e.Types.Skill
import AH3e.Types.State
import Data.Map.Strict qualified as Map

{- | "Take card N (the epic monster) and spawn it at ...". The card waits in the
set-aside pile, and a card that asks for one already in play gets nothing.
-}
spawnHeldBack :: CardCode -> SpaceId -> GameM [Message]
spawnHeldBack code sid = do
  aside <- use (#decks . #setAside)
  found <- filterM (fmap (== code) . cardCode) aside
  case take 1 found of
    [cid] -> do
      #decks . #setAside %= filter (/= cid)
      pure [PlaceMonster cid sid Ready]
    _ -> pure []

-- | Whether that epic monster is on the board.
inPlay :: CardCode -> GameM Bool
inPlay code = do
  ms <- uses #monsters Map.keys
  codes <- traverse cardCode ms
  pure (code `elem` codes)

{- | Whether an investigator stands on the space a card marked -- Dreams of
R'lyeh's ritual site is its blue marker, its cultist shrine the red.
-}
standsOnMarked :: Text -> InvestigatorId -> GameM Bool
standsOnMarked colour iid = do
  there <- markerSpace colour
  here <- investigatorSpace iid
  pure (isJust here && here == there)

-- | The doom a card has gathered on itself, which several of them test against.
doomOn :: ArchiveNumber -> GameM Int
doomOn n = uses #codex (sum . map (Map.findWithDefault 0 "doom" . (.tokens)) . filter ((== n) . (.number)))

-- | Move one doom from the scenario sheet onto a card, holding the end at bay.
doomOntoCard :: ArchiveNumber -> GameM ()
doomOntoCard n = do
  #sheetDoom %= max 0 . subtract 1
  #codex . traversed . filtered ((== n) . (.number)) . #tokens %= Map.insertWith (+) "doom" 1

-- | "When there is N or more doom on the scenario sheet, flip this card."
flipOnSheetDoom :: Text -> Int -> ArchiveNumber -> CodexTrigger
flipOnSheetDoom key n card =
  CodexTrigger
    { key = key
    , once = True
    , condition = \e -> do
        doom <- use #sheetDoom
        pure (not e.flipped && doom >= n)
    , action = \_ -> push (FlipCodexCard card)
    }

-- | "When there are N or more clues on the scenario sheet, flip this card."
flipOnSheetClues :: Text -> Int -> ArchiveNumber -> CodexTrigger
flipOnSheetClues key n card =
  CodexTrigger
    { key = key
    , once = True
    , condition = \e -> do
        clues <- use #sheetClues
        pure (not e.flipped && clues >= n)
    , action = \_ -> push (FlipCodexCard card)
    }

-- | The common ending: this card's back is read and the investigators have won.
winOnFlip :: CodexBehavior -> CodexBehavior
winOnFlip b = b {onFlip = \e -> when e.flipped (push WinTheGame)}

{- | Two scenarios deal a few archive cards out face down and turn them up over
time: Tyrants of Ruin's relics and Dreams of R'lyeh's four endings. The pile is
the same thing both times.
-}
setInvestigation :: ArchiveNumber -> [ArchiveNumber] -> GameM ()
setInvestigation under ns = do
  deck <- use (#decks . #investigation)
  taken <- uses #codex (map (.number))
  when (null deck && not (any (`elem` taken) ns)) do
    shuffled <- shuffle ns
    #decks . #investigation .= shuffled
    -- the pile lies under whichever card set it out, which is where the table shows it
    #decks . #investigationUnder ?= under

-- | Turn up the next card of that pile.
revealInvestigation :: GameM (Maybe ArchiveNumber)
revealInvestigation =
  use (#decks . #investigation) >>= \case
    [] -> pure Nothing
    n : rest -> do
      #decks . #investigation .= rest
      when (null rest) (#decks . #investigationUnder .= Nothing)
      logText ("Archive card " <> tshow (coerce n :: Int) <> " is turned up")
      pure (Just n)

-- | A card a neighborhood is holding, by the colour of its marker.
markedNeighborhoods :: Text -> GameM [NeighborhoodId]
markedNeighborhoods colour = do
  board <- use #board
  pure [n.id | n <- Map.elems board.neighborhoods, any ((== colour) . (.color)) n.markers]

-- | Every neighborhood, most doom first.
byDoom :: GameM [Neighborhood]
byDoom = do
  board <- use #board
  let total n = sum [maybe 0 (.doom) (Map.lookup sid board.spaces) | sid <- n.spaces]
  pure (sortOn (negate . total) (Map.elems board.neighborhoods))

-- | Every face-up marker on the board, with the space holding it.
faceUpMarkers :: [Text] -> GameM [(SpaceId, Marker)]
faceUpMarkers colours = do
  board <- use #board
  pure [(s.id, m) | s <- Map.elems board.spaces, m <- s.markers, m.faceUp, m.color `elem` colours]

-- | Every marker on the board, face up or down, in the colours a card cares about.
placedMarkers :: [Text] -> GameM [(SpaceId, Marker)]
placedMarkers colours = do
  board <- use #board
  pure [(s.id, m) | s <- Map.elems board.spaces, m <- s.markers, m.color `elem` colours]

-- | Turn one face-up marker in that space face down, and say whether there was one.
turnMarkerDown :: SpaceId -> GameM Bool
turnMarkerDown sid = do
  s <- getSpace sid
  case break (.faceUp) s.markers of
    (before, m : after) -> do
      spaceL sid . #markers .= before <> (m {faceUp = False} : after)
      logText ("A " <> m.color <> " marker is turned face down")
      pure True
    _ -> pure False

{- | "Move all X and Y markers on the board to the scenario sheet", which is where
Ithaqua's Children keeps the ones a branch did not use.
-}
markersToSheet :: [Text] -> GameM Int
markersToSheet colours = do
  moving <- placedMarkers colours
  #board . #spaces . traversed . #markers %= filter ((`notElem` colours) . (.color))
  pushAll [MarkSheetToken m.color 1 | (_, m) <- moving]
  pure (length moving)

-- | Tokens a card has gathered on itself, whatever it calls them.
tokensOn :: Text -> ArchiveNumber -> GameM Int
tokensOn name n =
  uses #codex (sum . map (Map.findWithDefault 0 name . (.tokens)) . filter ((== n) . (.number)))

-- | Put tokens on a card, or take them off.
markCard :: Text -> ArchiveNumber -> Int -> GameM ()
markCard name n k =
  #codex
    . traversed
    . filtered ((== n) . (.number))
    . #tokens
    . at name
    %= Just
    . max 0
    . (+ k)
    . fromMaybe 0

{- | "Each investigator tests X. Each investigator that fails places one doom in
their space." The Pale Lantern's codex cards all keep one of these.
-}
everyoneTests :: Skill -> Int -> Text -> GameM ()
everyoneTests skill modifier key = do
  invs <- playingInvestigators
  pushAll
    [ BeginTest (newTest i.id skill modifier OtherTest (AfterCustom (SourceInvestigator i.id) key))
    | i <- invs
    ]

{- | What failing one of those costs: doom where you stand. The test carries the
investigator as its source, since an after-test handler is told nothing else
about who rolled it.
-}
doomWhereTheyStand :: Source -> Int -> GameM ()
doomWhereTheyStand src result = when (result <= 0) case src of
  SourceInvestigator iid -> investigatorSpace iid >>= traverse_ (push . PlaceDoom src)
  _ -> pure ()

{- | What Under Dark Waves' codex cards keep doing: fetching an epic monster the
box held back, finding the space a card marked, and counting the doom a card has
gathered on itself.
-}
module AH3e.Content.UnderDarkWaves.Codex where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Ids
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

{- | The space a card marked, named by the colour of the marker it put there --
Dreams of R'lyeh's ritual site is its blue marker, its cultist shrine the red.
-}
markedSpace :: Text -> GameM (Maybe SpaceId)
markedSpace colour = do
  board <- use #board
  pure $ listToMaybe [s.id | s <- Map.elems board.spaces, any ((== colour) . (.color)) s.markers]

-- | Whether an investigator is standing on the space a card marked.
standsOnMarked :: Text -> InvestigatorId -> GameM Bool
standsOnMarked colour iid = do
  there <- markedSpace colour
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

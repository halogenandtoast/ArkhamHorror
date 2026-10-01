-- | Ithaqua's Children's own mechanics: its reckoning, and its codex cards.
module AH3e.Content.UnderDarkWaves.IthaquasChildrenBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Effect
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("ithaqua-reckoning", doomInBothTowns)
          , ("ithaqua-doom-innsmouth", doomInTown Innsmouth)
          , ("ithaqua-doom-kingsport", doomInTown Kingsport)
          , ("wendigo-lurk", spreadTerrorHere)
          ]
    }

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

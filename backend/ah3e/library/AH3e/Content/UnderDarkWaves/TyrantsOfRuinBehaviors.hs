-- | Tyrants of Ruin's own mechanics: its reckoning, and its codex cards.
module AH3e.Content.UnderDarkWaves.TyrantsOfRuinBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.Card
import AH3e.Types.State
import Data.List (nub)
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects =
        Map.fromList
          [ ("tyrants-reckoning", spreadTerrorWhereDeepOnesAre)
          , ("father-dagon-lurk", deepOnesRecover)
          ]
    }

{- | "Spread terror in each neighborhood with a Deep One monster." Each
neighborhood is counted once however many of them are standing in it.
-}
spreadTerrorWhereDeepOnesAre :: EffectCtx -> GameM ()
spreadTerrorWhereDeepOnesAre _ = do
  board <- use #board
  ms <- uses #monsters Map.elems
  deepOnes <- filterM (fmap (elem "Deep One" . (.traits)) . monsterDef . (.card)) ms
  let hoods = nub (mapMaybe ((`spaceNeighborhood` board) . (.space)) deepOnes)
  pushAll [SpreadTerror nid | nid <- hoods]

-- | Father Dagon's lurk: "Each Deep One monster recovers two health."
deepOnesRecover :: EffectCtx -> GameM ()
deepOnesRecover _ = do
  ms <- uses #monsters Map.elems
  deepOnes <- filterM (fmap (elem "Deep One" . (.traits)) . monsterDef . (.card)) ms
  for_ deepOnes \m -> #monsters . ix m.card . #damage %= max 0 . subtract 2
  unless (null deepOnes) (logText "Each Deep One monster recovers two health")

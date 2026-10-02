-- | What The Key and the Gate asks of the engine directly.
module AH3e.Content.SecretsOfTheOrder.TheKeyAndTheGateBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
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
          [ ("the-key-and-the-gate-reckoning", reckoning)
          , ("the-key-and-the-gate-pull", pull)
          ]
    }

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

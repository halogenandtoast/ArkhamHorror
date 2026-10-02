-- | What The Dead Cry Out asks of the engine directly.
module AH3e.Content.SecretsOfTheOrder.TheDeadCryOutBehaviors (behaviors) where

import AH3e.Engine.Behavior
import AH3e.Engine.Helpers
import AH3e.Engine.Monad
import AH3e.Engine.Query
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Board
import AH3e.Types.State
import Data.Map.Strict qualified as Map

behaviors :: Behaviors
behaviors =
  mempty
    { customEffects = Map.fromList [("the-dead-cry-out-reckoning", reckoning)]
    }

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

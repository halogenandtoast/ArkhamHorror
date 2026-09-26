module AH3e.Engine (
  module AH3e.Engine.Setup,
  runEngine,
  answer,
  applyDebug,
  withSeedFrom,
  EngineError (..),
) where

import AH3e.Engine.Run
import AH3e.Engine.Setup
import AH3e.Game
import AH3e.Message
import AH3e.Prelude
import AH3e.Types.Ids
import AH3e.Types.State
import Data.Map.Strict qualified as Map

data EngineError
  = NoQuestionFor PlayerId
  | InvalidChoice Int
  | DebugDisabled
  | EngineStuck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

maxSteps :: Int
maxSteps = 100000

runEngine :: Game -> Either EngineError Game
runEngine = go maxSteps
 where
  go 0 _ = Left EngineStuck
  go fuel g
    | not (Map.null g.questions) = Right g
    | g.status /= InProgress = Right g
    | otherwise = case g.queue of
        [] -> Right g
        (m : ms) -> go (fuel - 1) (execState (runMessage m) g {queue = ms})

answer :: PlayerId -> Int -> Game -> Either EngineError Game
answer pid idx g = do
  q <- maybe (Left (NoQuestionFor pid)) Right (Map.lookup pid g.questions)
  c <- maybe (Left (InvalidChoice idx)) Right (listToMaybe (drop idx q.choices))
  when (idx < 0) $ Left (InvalidChoice idx)
  runEngine g {questions = mempty, queue = c.messages <> g.queue, phasesEntered = []}

applyDebug :: DebugAction -> Game -> Either EngineError Game
applyDebug action g
  | not g.debug = Left DebugDisabled
  -- runs now, even with a question open: the open questions wait behind it and
  -- come back once it finishes, after any question of its own
  | otherwise =
      runEngine
        g
          { questions = mempty
          , queue = Debug action : RestoreQuestions g.questions : g.queue
          , phasesEntered = []
          }

{- | An earlier state to resume from, carrying the seed the state being thrown away
had reached. The seed is part of the game, so an undo hands back the roll it just
undid unless the stream is carried across; with it, replaying an action rolls anew.
-}
withSeedFrom :: Game -> Game -> Game
withSeedFrom earlier undone = earlier {seed = undone.seed}

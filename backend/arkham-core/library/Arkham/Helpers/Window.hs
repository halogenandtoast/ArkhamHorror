module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Window
import Arkham.Matcher
import Arkham.Message
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Timing qualified as Timing
import Arkham.Classes.Query

checkWindows :: (Monad m, HasGame m) => [Window] -> m Message
checkWindows windows' = do
  iids <- selectList UneliminatedInvestigator
  if null iids
    then do
      iids' <- selectList Anyone
      pure $ CheckWindow iids' windows'
    else pure $ CheckWindow iids windows'

windows :: (Monad m, HasGame m) => [WindowType] -> m [Message]
windows windows' = do
  iids <- selectList UneliminatedInvestigator
  pure $ do
    timing <- [Timing.When, Timing.After]
    [CheckWindow iids $ map (Window timing) windows']

splitWithWindows :: (Monad m, HasGame m) => Message -> [WindowType] -> m [Message]
splitWithWindows msg windows' = do
  iids <- selectList UneliminatedInvestigator
  pure
    $ [CheckWindow iids $ map (Window Timing.When) windows']
    <> [msg]
    <> [CheckWindow iids $ map (Window Timing.After) windows']

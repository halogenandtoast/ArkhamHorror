module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.Query
import {-# SOURCE #-} Arkham.Game ()
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message
import Arkham.Store
import Arkham.Timing qualified as Timing
import Arkham.Window

checkWindows :: (HasGame m, Store m Card) => [Window] -> m Message
checkWindows windows' = do
  iids <- selectList UneliminatedInvestigator
  if null iids
    then do
      iids' <- selectList Anyone
      pure $ CheckWindow iids' windows'
    else pure $ CheckWindow iids windows'

windows :: (HasGame m, Store m Card) => [WindowType] -> m [Message]
windows windows' = do
  iids <- selectList UneliminatedInvestigator
  pure $ do
    timing <- [Timing.When, Timing.After]
    [CheckWindow iids $ map (Window timing) windows']

splitWithWindows :: (HasGame m, Store m Card) => Message -> [WindowType] -> m [Message]
splitWithWindows msg windows' = do
  iids <- selectList UneliminatedInvestigator
  pure $
    [CheckWindow iids $ map (Window Timing.When) windows']
      <> [msg]
      <> [CheckWindow iids $ map (Window Timing.After) windows']

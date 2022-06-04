module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Window
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Classes.Query

checkWindows :: Query InvestigatorMatcher m => [Window] -> m Message
checkWindows windows' = do
  iids <- selectList Anyone
  pure $ CheckWindow iids windows'

windows :: Query InvestigatorMatcher m => [WindowType] -> m [Message]
windows windows' = do
  iids <- selectList Anyone
  pure $ do
    timing <- [Timing.When, Timing.After]
    [CheckWindow iids $ map (Window timing) windows']

splitWithWindows :: Query InvestigatorMatcher m => Message -> [WindowType] -> m [Message]
splitWithWindows msg windows' = do
  iids <- selectList Anyone
  pure
    $ [CheckWindow iids $ map (Window Timing.When) windows']
    <> [msg]
    <> [CheckWindow iids $ map (Window Timing.After) windows']

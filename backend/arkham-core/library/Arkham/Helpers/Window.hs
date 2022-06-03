module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Window
import Arkham.Matcher
import Arkham.Message
import Arkham.Timing qualified as Timing

checkWindows :: [Window] -> m Message
checkWindows windows' = do
  iids <- select Anyone
  pure $ CheckWindow iids windows'

windows :: [WindowType] -> m [Message]
windows windows' = do
  iids <- getInvestigatorIds
  pure $ do
    timing <- [Timing.When, Timing.After]
    [CheckWindow iids $ map (Window timing) windows']

splitWithWindows :: Message -> [WindowType] -> m [Message]
splitWithWindows msg windows' = do
  iids <- getInvestigatorIds
  pure
    $ [CheckWindow iids $ map (Window Timing.When) windows']
    <> [msg]
    <> [CheckWindow iids $ map (Window Timing.After) windows']

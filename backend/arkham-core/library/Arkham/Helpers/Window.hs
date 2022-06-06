module Arkham.Helpers.Window where

import Arkham.Prelude

import Arkham.Window
import Arkham.Matcher
import Arkham.Message
import {-# SOURCE #-} Arkham.GameEnv
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Timing qualified as Timing
import Arkham.Classes.Query

checkWindows :: [Window] -> GameT Message
checkWindows windows' = do
  iids <- selectList Anyone
  pure $ CheckWindow iids windows'

windows :: [WindowType] -> GameT [Message]
windows windows' = do
  iids <- selectList Anyone
  pure $ do
    timing <- [Timing.When, Timing.After]
    [CheckWindow iids $ map (Window timing) windows']

splitWithWindows :: Message -> [WindowType] -> GameT [Message]
splitWithWindows msg windows' = do
  iids <- selectList Anyone
  pure
    $ [CheckWindow iids $ map (Window Timing.When) windows']
    <> [msg]
    <> [CheckWindow iids $ map (Window Timing.After) windows']

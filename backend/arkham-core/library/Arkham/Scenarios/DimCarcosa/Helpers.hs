module Arkham.Scenarios.DimCarcosa.Helpers where

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.GameEnv
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

readStory :: InvestigatorId -> LocationId -> CardDef -> GameT ()
readStory iid lid storyDef = do
  whenWindowMsg <-
    checkWindows
      [Window Timing.When (Window.FlipLocation iid lid)]
  afterWindowMsg <-
    checkWindows
      [Window Timing.After (Window.FlipLocation iid lid)]
  storyCard <- genCard storyDef
  pushAll [whenWindowMsg, afterWindowMsg, ReadStory iid storyCard]

module Arkham.Scenarios.DimCarcosa.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasQueue
import Arkham.GameEnv
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Timing qualified as Timing
import Arkham.Window (mkWindow)
import Arkham.Window qualified as Window

readStory :: InvestigatorId -> LocationId -> CardDef -> GameT ()
readStory iid lid storyDef = do
  whenWindowMsg <-
    checkWindows
      [mkWindow Timing.When (Window.FlipLocation iid lid)]
  afterWindowMsg <-
    checkWindows
      [mkWindow Timing.After (Window.FlipLocation iid lid)]
  storyCard <- genCard storyDef
  pushAll [whenWindowMsg, afterWindowMsg, ReadStory iid storyCard ResolveIt Nothing]

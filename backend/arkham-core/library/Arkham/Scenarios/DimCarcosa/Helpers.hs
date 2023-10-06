module Arkham.Scenarios.DimCarcosa.Helpers where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Classes.HasQueue
import Arkham.Helpers.Window
import Arkham.Id
import Arkham.Message
import Arkham.Window qualified as Window

readStory
  :: (CardGen m, HasQueue Message m, HasGame m) => InvestigatorId -> LocationId -> CardDef -> m ()
readStory iid lid storyDef = do
  (whenWindowMsg, _, afterWindowMsg) <- frame (Window.FlipLocation iid lid)
  storyCard <- genCard storyDef
  pushAll [whenWindowMsg, afterWindowMsg, ReadStory iid storyCard ResolveIt Nothing]

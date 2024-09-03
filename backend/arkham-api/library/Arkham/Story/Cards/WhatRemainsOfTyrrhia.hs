module Arkham.Story.Cards.WhatRemainsOfTyrrhia ( WhatRemainsOfTyrrhia(..) , whatRemainsOfTyrrhia) where

import Arkham.ScenarioLogKey
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype WhatRemainsOfTyrrhia = WhatRemainsOfTyrrhia StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatRemainsOfTyrrhia :: StoryCard WhatRemainsOfTyrrhia
whatRemainsOfTyrrhia = story WhatRemainsOfTyrrhia Cards.whatRemainsOfTyrrhia

instance RunMessage WhatRemainsOfTyrrhia where
  runMessage msg s@(WhatRemainsOfTyrrhia attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> WhatRemainsOfTyrrhia <$> runMessage msg attrs

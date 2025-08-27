module Arkham.Story.Cards.WhatRemainsOfTyrrhia (whatRemainsOfTyrrhia) where

import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Import.Lifted

newtype WhatRemainsOfTyrrhia = WhatRemainsOfTyrrhia StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

whatRemainsOfTyrrhia :: StoryCard WhatRemainsOfTyrrhia
whatRemainsOfTyrrhia = story WhatRemainsOfTyrrhia Cards.whatRemainsOfTyrrhia

instance RunMessage WhatRemainsOfTyrrhia where
  runMessage msg s@(WhatRemainsOfTyrrhia attrs) = runQueueT $ case msg of
    ResolveThisStory _ (is attrs -> True) -> do
      push $ ScenarioCountIncrementBy SignOfTheGods 1
      pure s
    _ -> WhatRemainsOfTyrrhia <$> liftRunMessage msg attrs

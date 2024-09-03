module Arkham.Helpers.Choose where

import Arkham.Choose
import Arkham.Collection
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target

randomlyChooseFrom
  :: (Targetable a, Sourceable a, IsCollection col) => a -> InvestigatorId -> col -> Int -> Message
randomlyChooseFrom a iid col n = ChooseFrom iid $ chooseRandom a col n

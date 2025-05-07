module Arkham.Helpers.Choose where

import Arkham.Choose
import Arkham.Collection
import Arkham.Id
import Arkham.Message
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Arkham.Classes.HasQueue (push)
import Arkham.Message.Lifted.Queue

randomlyChooseFrom
  :: (ReverseQueue m, Targetable a, Sourceable a, IsCollection col) => a -> InvestigatorId -> col -> Int -> m ()
randomlyChooseFrom a iid col n = push $ ChooseFrom iid $ chooseRandom a col n

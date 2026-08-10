module Arkham.Helpers.Search where

import Arkham.Card
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Helpers.Modifiers
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorSearch))
import Arkham.Message (Message)
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Queue
import Arkham.Prelude
import Arkham.Projection
import Arkham.Queue
import Arkham.Search (Search (..))

getFoundCards :: HasGame m => InvestigatorId -> m [Card]
getFoundCards iid = do
  field InvestigatorSearch iid <&> \case
    Nothing -> []
    Just search -> concat $ toList (searchFoundCards search)

chooseFromSearch
  :: ReverseQueue m => InvestigatorId -> Int -> [Card] -> (Card -> QueueT Message m ()) -> m ()
chooseFromSearch iid n cards f = do
  total <- getTotalSearchTargets iid cards n
  chooseUpToNM_ iid total $ targets cards f

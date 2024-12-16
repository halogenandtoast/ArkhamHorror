module Arkham.Helpers.Search where

import Arkham.Card
import Arkham.Classes.HasGame
import {-# SOURCE #-} Arkham.Game ()
import Arkham.Id
import Arkham.Investigator.Types (Field (InvestigatorSearch))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Search (Search (..))

getFoundCards :: HasGame m => InvestigatorId -> m [Card]
getFoundCards iid = do
  field InvestigatorSearch iid <&> \case
    Nothing -> []
    Just search -> concat $ toList (searchFoundCards search)

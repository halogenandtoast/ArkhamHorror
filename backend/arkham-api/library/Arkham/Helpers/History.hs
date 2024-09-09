module Arkham.Helpers.History (module X, module Arkham.Helpers.History) where

import Arkham.Classes.HasGame
import Arkham.GameEnv as X (getHistory, getHistoryField)
import Arkham.Helpers.Query
import Arkham.History as X
import Arkham.Prelude

getAllHistoryField :: (Monoid k, HasGame m) => HistoryType -> HistoryField k -> m k
getAllHistoryField hType fld = foldMapM (\i -> getHistoryField hType i fld) =<< allInvestigatorIds

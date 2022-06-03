module Arkham.Classes.HasHistory where

import Arkham.Prelude

import Arkham.History
import Arkham.Id

class Monad m => HasHistory m where
  getHistory :: HistoryType -> InvestigatorId -> m History

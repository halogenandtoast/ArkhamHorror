{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Helpers.Investigator where

import Arkham.Capability
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher.Investigator
import Arkham.Prelude
import Arkham.Tracing

matchWho
  :: (HasGame m, Tracing m) => InvestigatorId -> InvestigatorId -> InvestigatorMatcher -> m Bool
getSpendableClueCount
  :: (HasGame m, Tracing m, AsId investigator, IdOf investigator ~ InvestigatorId) => investigator -> m Int

instance (HasGame m, Tracing m) => Capable (InvestigatorId -> m Bool)

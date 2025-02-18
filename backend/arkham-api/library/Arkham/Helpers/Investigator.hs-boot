{-# OPTIONS_GHC -Wno-orphans #-}
module Arkham.Helpers.Investigator where

import Arkham.Prelude
import Arkham.Capability
import Arkham.Classes.HasGame
import Arkham.Id
import Arkham.Matcher.Investigator

matchWho :: HasGame m => InvestigatorId -> InvestigatorId -> InvestigatorMatcher -> m Bool

instance HasGame m => Capable (InvestigatorId -> m Bool)

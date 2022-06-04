module Arkham.Helpers.Query where

import Arkham.Prelude

import Arkham.Matcher
import Arkham.Id
import Arkham.Classes.Query

getLeadInvestigatorId :: Query InvestigatorMatcher m => m InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getInvestigatorIds :: Query InvestigatorMatcher m => m [InvestigatorId]
getInvestigatorIds = selectList Anyone

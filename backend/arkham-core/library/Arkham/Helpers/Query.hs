module Arkham.Helpers.Query where

import Arkham.Classes.Query
import Arkham.Id
import Arkham.Matcher

getLeadInvestigatorId :: Query InvestigatorMatcher m => m InvestigatorId
getLeadInvestigatorId = selectJust LeadInvestigator

getInvestigatorIds :: Query InvestigatorMatcher m => m [InvestigatorId]
getInvestigatorIds = selectList Anyone

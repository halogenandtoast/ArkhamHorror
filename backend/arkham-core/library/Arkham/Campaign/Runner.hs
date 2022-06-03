module Arkham.Campaign.Runner where

import Arkham.Card
import Arkham.Classes
import Arkham.InvestigatorId
import Arkham.Matcher
import Arkham.Query
import Arkham.ScenarioId

type CampaignRunner m
  = ( HasSet InvestigatorId m ()
    , HasId LeadInvestigatorId m ()
    , HasRecord m ()
    , HasList CampaignStoryCard m ()
    , HasName m ScenarioId
    , Query InvestigatorMatcher m
    )

module Arkham.Campaign.Runner where

import Arkham.Card
import Arkham.Classes
import Arkham.InvestigatorId
import Arkham.Matcher
import Arkham.Query
import Arkham.ScenarioId

type CampaignRunner env
  = ( HasQueue env
    , HasSet InvestigatorId env ()
    , HasId LeadInvestigatorId env ()
    , HasRecord env ()
    , HasList CampaignStoryCard env ()
    , HasName env ScenarioId
    , Query InvestigatorMatcher env
    )

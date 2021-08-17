module Arkham.Types.Campaign.Runner where

import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Query
import Arkham.Types.ScenarioId

type CampaignRunner env
  = ( HasSet InvestigatorId env ()
    , HasId LeadInvestigatorId env ()
    , HasRecord env
    , HasList CampaignStoryCard env ()
    , HasName env ScenarioId
    )

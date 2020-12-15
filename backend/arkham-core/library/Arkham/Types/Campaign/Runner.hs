module Arkham.Types.Campaign.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId
import Arkham.Types.Query

type CampaignRunner env
  = ( HasQueue env
    , HasSet InvestigatorId env ()
    , HasId LeadInvestigatorId env ()
    , HasRecord env
    )

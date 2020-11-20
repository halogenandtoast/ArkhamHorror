module Arkham.Types.Campaign.Runner where

import Arkham.Types.Classes
import Arkham.Types.InvestigatorId

type CampaignRunner env = (HasQueue env, HasSet InvestigatorId env ())

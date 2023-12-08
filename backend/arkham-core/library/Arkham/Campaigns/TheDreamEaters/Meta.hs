module Arkham.Campaigns.TheDreamEaters.Meta where

import Arkham.Campaign.Types
import Arkham.Id
import Arkham.Investigator.Types
import Arkham.Message
import Arkham.Prelude

pattern InTheDreamQuest :: Message -> Message
pattern InTheDreamQuest msg <- (DoStep 1 msg)
  where
    InTheDreamQuest msg = DoStep 1 msg

pattern InTheWebOfDreams :: Message -> Message
pattern InTheWebOfDreams msg <- (DoStep 2 msg)
  where
    InTheWebOfDreams msg = DoStep 2 msg

data CampaignPart = TheDreamQuest | TheWebOfDreams
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data CampaignMode = PartialMode CampaignPart | FullMode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Metadata = Metadata
  { campaignMode :: CampaignMode
  , currentCampaignMode :: Maybe CampaignPart
  , otherCampaignAttrs :: Maybe CampaignAttrs
  , currentCampaignPlayers :: Map PlayerId InvestigatorAttrs
  , otherCampaignPlayers :: Map PlayerId InvestigatorAttrs
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

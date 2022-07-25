module Arkham.Campaign.Campaigns.TheForgottenAge
  ( TheForgottenAge(..)
  , theForgottenAge
  ) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.Campaigns.TheForgottenAge.Import
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message
import Arkham.Token

newtype TheForgottenAge = TheForgottenAge CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theForgottenAge :: Difficulty -> TheForgottenAge
theForgottenAge difficulty = TheForgottenAge $ baseAttrs
  (CampaignId "04")
  "The Forgotten Age"
  difficulty
  (chaosBagContents difficulty)

instance RunMessage TheForgottenAge where
  runMessage msg c@(TheForgottenAge attrs) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getInvestigatorIds
      pushAll $ [story investigatorIds prologue] <> [NextCampaignStep Nothing]
      pure c
    _ -> TheForgottenAge <$> runMessage msg attrs

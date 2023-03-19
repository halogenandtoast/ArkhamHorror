module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.CampaignStep
import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Helpers.Query
import Arkham.Message

newtype NightOfTheZealot = NightOfTheZealot CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq, HasModifiersFor)

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot difficulty = campaign
  NightOfTheZealot
  "01"
  "Night of the Zealot"
  difficulty
  (chaosBagContents difficulty)

instance RunMessage NightOfTheZealot where
  runMessage msg c@(NightOfTheZealot attrs@CampaignAttrs {..}) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      pushAll [story investigatorIds prologue, NextCampaignStep Nothing]
      pure c
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push $ CampaignStep step
      pure
        . NightOfTheZealot
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep campaignStep)
    _ -> NightOfTheZealot <$> runMessage msg attrs

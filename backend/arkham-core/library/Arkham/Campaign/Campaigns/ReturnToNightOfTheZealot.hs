module Arkham.Campaign.Campaigns.ReturnToNightOfTheZealot where

import Arkham.Prelude

import Arkham.Campaign.Attrs
import Arkham.Campaign.Campaigns.NightOfTheZealot
import Arkham.Campaign.Runner
import Arkham.CampaignId
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Message

newtype ReturnToNightOfTheZealot = ReturnToNightOfTheZealot NightOfTheZealot
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

returnToNightOfTheZealot :: Difficulty -> ReturnToNightOfTheZealot
returnToNightOfTheZealot difficulty =
  ReturnToNightOfTheZealot . NightOfTheZealot $ baseAttrs
    (CampaignId "50")
    "Return to the Night of the Zealot"
    difficulty
    (nightOfTheZealotChaosBagContents difficulty)

instance (CampaignRunner env) => RunMessage env ReturnToNightOfTheZealot where
  runMessage msg (ReturnToNightOfTheZealot nightOfTheZealot'@(NightOfTheZealot attrs@CampaignAttrs {..}))
    = case msg of
      NextCampaignStep _ -> do
        let
          nextStep = case campaignStep of
            Just PrologueStep -> Just (ScenarioStep "50011")
            Just (ScenarioStep "50011") -> Just (ScenarioStep "50025")
            Just (ScenarioStep "50025") -> Just (ScenarioStep "50032")
            _ -> Nothing
        push (CampaignStep nextStep)
        pure
          . ReturnToNightOfTheZealot
          . NightOfTheZealot
          $ attrs
          & (stepL .~ nextStep)
          & (completedStepsL %~ completeStep campaignStep)
      _ -> ReturnToNightOfTheZealot <$> runMessage msg nightOfTheZealot'

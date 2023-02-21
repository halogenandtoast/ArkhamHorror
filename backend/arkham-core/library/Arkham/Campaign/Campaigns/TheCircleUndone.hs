module Arkham.Campaign.Campaigns.TheCircleUndone
  ( TheCircleUndone(..)
  , theCircleUndone
  ) where

import Arkham.Prelude

import Arkham.Campaign.Runner
import Arkham.Campaigns.TheCircleUndone.Import
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Id
import Arkham.Message

newtype TheCircleUndone = TheCircleUndone CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone difficulty = campaign
  (TheCircleUndone . initCampaign)
  (CampaignId "05")
  "The Circle Undone"
  difficulty
  (chaosBagContents difficulty)
 where
  initCampaign attrs = attrs { campaignStep = Just (ScenarioStep "05043") }

instance RunMessage TheCircleUndone where
  runMessage msg (TheCircleUndone attrs) = case msg of
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push (CampaignStep step)
      pure
        . TheCircleUndone
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    _ -> TheCircleUndone <$> runMessage msg attrs

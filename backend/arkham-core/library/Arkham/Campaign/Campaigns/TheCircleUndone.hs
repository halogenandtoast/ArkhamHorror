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
import Arkham.Helpers.Query
import Arkham.Id
import Arkham.Message

newtype TheCircleUndone = TheCircleUndone CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasModifiersFor)

theCircleUndone :: Difficulty -> TheCircleUndone
theCircleUndone difficulty = campaign
  TheCircleUndone
  (CampaignId "05")
  "The Circle Undone"
  difficulty
  (chaosBagContents difficulty)

instance RunMessage TheCircleUndone where
  runMessage msg c@(TheCircleUndone attrs) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- allInvestigatorIds
      pushAll $ story investigatorIds prologue
        : [ CampaignStep (Just (InvestigatorCampaignStep iid PrologueStep))
           | iid <- investigatorIds
           ]
        <> [story investigatorIds intro, NextCampaignStep Nothing]
      pure c
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push $ CampaignStep step
      pure
        . TheCircleUndone
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep (campaignStep attrs))
    _ -> TheCircleUndone <$> runMessage msg attrs

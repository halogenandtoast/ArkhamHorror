module Arkham.Campaign.Campaigns.NightOfTheZealot where

import Arkham.Prelude

import Arkham.Campaigns.NightOfTheZealot.Import
import Arkham.Campaign.Attrs
import Arkham.Campaign.Runner
import Arkham.CampaignId
import Arkham.CampaignStep
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Message

newtype NightOfTheZealot = NightOfTheZealot CampaignAttrs
  deriving anyclass IsCampaign
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

nightOfTheZealot :: Difficulty -> NightOfTheZealot
nightOfTheZealot difficulty = NightOfTheZealot $ baseAttrs
  (CampaignId "01")
  "Night of the Zealot"
  difficulty
  (chaosBagContents difficulty)

instance CampaignRunner env => RunMessage NightOfTheZealot where
  runMessage msg c@(NightOfTheZealot attrs@CampaignAttrs {..}) = case msg of
    CampaignStep (Just PrologueStep) -> do
      investigatorIds <- getSetList ()
      c <$ pushAll
        [ story investigatorIds prologue
        , NextCampaignStep Nothing
        ]
    NextCampaignStep _ -> do
      let step = nextStep attrs
      push (CampaignStep step)
      pure
        . NightOfTheZealot
        $ attrs
        & (stepL .~ step)
        & (completedStepsL %~ completeStep campaignStep)
    _ -> NightOfTheZealot <$> runMessage msg attrs

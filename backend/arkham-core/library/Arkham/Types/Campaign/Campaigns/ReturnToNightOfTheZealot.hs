{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Campaign.Campaigns.ReturnToNightOfTheZealot where

import Arkham.Import hiding (Cultist)

import Arkham.Types.Campaign.Attrs
import Arkham.Types.Campaign.Campaigns.NightOfTheZealot
import Arkham.Types.Campaign.Runner
import Arkham.Types.CampaignId
import Arkham.Types.CampaignStep
import Arkham.Types.Difficulty

newtype ReturnToNightOfTheZealot = ReturnToNightOfTheZealot NightOfTheZealot
  deriving newtype (Show, ToJSON, FromJSON)

returnToNightOfTheZealot :: Difficulty -> ReturnToNightOfTheZealot
returnToNightOfTheZealot difficulty =
  ReturnToNightOfTheZealot
    . NightOfTheZealot
    $ (baseAttrs
        (CampaignId "50")
        "Return to the Night of the Zealot"
        difficulty
        (nightOfTheZealotChaosBagContents difficulty)
      )
        { campaignSteps = fromList
          [ PrologueStep
          , ScenarioStep "50011"
          , ScenarioStep "50025"
          , ScenarioStep "50032"
          ]
        }
instance (CampaignRunner env) => RunMessage env ReturnToNightOfTheZealot where
  runMessage msg (ReturnToNightOfTheZealot nightOfTheZealot') =
    ReturnToNightOfTheZealot <$> runMessage msg nightOfTheZealot'

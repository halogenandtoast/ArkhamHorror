module Arkham.Homebrew.Types where

import Arkham.Campaign.Types (IsCampaign)
import Arkham.Difficulty
import Arkham.Scenario.Types (IsScenario)

data HomebrewCampaign = forall a. IsCampaign a => HomebrewCampaign (Difficulty -> a)

data HomebrewScenario = forall a. IsScenario a => HomebrewScenario (Difficulty -> a)

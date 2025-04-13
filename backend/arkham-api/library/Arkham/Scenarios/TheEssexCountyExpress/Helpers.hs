module Arkham.Scenarios.TheEssexCountyExpress.Helpers where

import Arkham.Layout
import Arkham.Campaigns.TheDunwichLegacy.Helpers
import Arkham.Classes
import Arkham.Classes.HasGame
import Arkham.Direction
import Arkham.I18n
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theEssexCountyExpress" a

leftmostLocation :: HasGame m => m LocationId
leftmostLocation = go =<< selectJust (LocationWithTitle "Engine Car")
 where
  go lid = maybe (pure lid) go =<< selectOne (LocationInDirection LeftOf $ LocationWithId lid)

scenarioLayout :: [GridTemplateRow]
scenarioLayout = ["trainCar6 trainCar5 trainCar4 trainCar3 trainCar2 trainCar1 engineCar"]

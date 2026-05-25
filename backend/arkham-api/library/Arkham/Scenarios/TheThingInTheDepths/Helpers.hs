module Arkham.Scenarios.TheThingInTheDepths.Helpers where

import Arkham.Campaigns.TheFeastOfHemlockVale.Helpers
import Arkham.I18n
import Arkham.Location.Grid (Pos (..))
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theThingInTheDepths" a

northShorePos :: Pos
northShorePos = Pos 1 1

startingPos :: Pos
startingPos = Pos (-1) (-1)

cranberryBogPos :: Pos
cranberryBogPos = Pos 1 (-1)

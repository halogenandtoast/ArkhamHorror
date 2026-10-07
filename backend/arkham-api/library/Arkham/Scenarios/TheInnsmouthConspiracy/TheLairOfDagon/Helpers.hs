module Arkham.Scenarios.TheInnsmouthConspiracy.TheLairOfDagon.Helpers where

import Arkham.Campaigns.TheInnsmouthConspiracy.Helpers
import Arkham.I18n
import Arkham.Layout
import Arkham.Prelude

scenarioI18n :: (HasI18n => a) -> a
scenarioI18n a = campaignI18n $ scope "theLairOfDagon" a

{- | The lair's three floors. Shared with the Return To box, which places the same
locations and halls.
-}
scenarioLayout :: [GridTemplateRow]
scenarioLayout =
  [ ". thirdFloorHall ."
  , "secondFloorHall1 foulCorridors secondFloorHall2"
  , "firstFloorHall1 grandEntryway firstFloorHall2"
  ]

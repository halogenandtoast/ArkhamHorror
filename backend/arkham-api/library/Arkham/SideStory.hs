module Arkham.SideStory where

import Arkham.Prelude
import Arkham.Id

getSideStoryCost :: ScenarioId -> Int
getSideStoryCost = \case
  "81001" -> 1
  "82001" -> 3
  "84001" -> 3
  "71001" -> 2
  "72001" -> 3
  sid -> error $ "Unknown standalone scenario for spending xp: " <> show sid

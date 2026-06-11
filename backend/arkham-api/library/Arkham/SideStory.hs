module Arkham.SideStory where

import Arkham.Prelude
import Arkham.Id

getSideStoryCost :: ScenarioId -> Int
getSideStoryCost = \case
  "81001" -> 1
  "82001" -> 3
  "83001" -> 2
  "83016" -> 2
  "85001" -> 2
  "86001" -> 3
  "87001" -> 2
  "70001" -> 2
  "84001" -> 3
  "88001" -> 3
  "71001" -> 2
  "72001" -> 3
  sid -> error $ "Unknown standalone scenario for spending xp: " <> show sid

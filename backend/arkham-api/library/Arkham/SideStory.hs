module Arkham.SideStory where

import Arkham.Homebrew.SideStories (homebrewSideStoryCosts)
import Arkham.Id
import Arkham.Prelude

{- | Every scenario that can be played as a side story, and what it costs.

The list is the authoritative answer to "is this scenario a side story", which
the stats panel needs: a scenario's own @isSideStory@ flag is only set while that
scenario is loaded, and it is absent entirely on games saved before the flag
existed, so it cannot be read back off a finished campaign.
-}
sideStoryCosts :: [(ScenarioId, Int)]
sideStoryCosts = officialSideStoryCosts <> homebrewSideStoryCosts

{- | Homebrew side stories are discovered instead (see
'Arkham.Homebrew.SideStoryDefs'), so a homebrew box adds one without editing
this list.
-}
officialSideStoryCosts :: [(ScenarioId, Int)]
officialSideStoryCosts =
  [ ("81001", 1)
  , ("82001", 3)
  , ("83001", 2)
  , ("83016", 2)
  , ("85001", 2)
  , ("86001", 3)
  , ("87001", 2)
  , ("70001", 2)
  , ("84001", 3)
  , ("88001", 3)
  , ("71001", 2)
  , ("72001", 3)
  , ("90004", 3)
  , ("90011", 3)
  , ("90020", 3)
  , ("90032", 3)
  , ("90041", 3)
  , ("90054", 3)
  , ("90065", 3)
  , ("90094", 2)
  ]

sideStoryIds :: [ScenarioId]
sideStoryIds = map fst sideStoryCosts

-- | Driven off 'sideStoryCosts' so the list and the cost cannot drift apart.
getSideStoryCost :: ScenarioId -> Int
getSideStoryCost sid = case [n | (s, n) <- sideStoryCosts, s == sid] of
  (n : _) -> n
  [] -> error $ "Unknown standalone scenario for spending xp: " <> show sid

{- | Challenge scenarios center on a specific investigator who must be chosen
when playing the scenario. As a side-story that investigator pays the full
cost while each other investigator pays only 1 xp. Enthralling Encore is the
exception: it only requires parallel content and costs everyone 2 xp.
-}
challengeScenarioInvestigator :: ScenarioId -> Maybe Text
challengeScenarioInvestigator = \case
  "90004" -> Just "Daisy Walker"
  "90011" -> Just "\"Skids\" O'Toole"
  "90020" -> Just "Agnes Baker"
  "90032" -> Just "Roland Banks"
  "90041" -> Just "Wendy Adams"
  "90054" -> Just "Jim Culver"
  "90065" -> Just "Monterey Jack"
  _ -> Nothing

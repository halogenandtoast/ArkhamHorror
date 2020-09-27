module Arkham.Types.Investigator.Cards.RexMurphySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Helpers
import Arkham.Types.Location.Attrs as Location
import Arkham.Types.Token

spec :: Spec
spec = describe "Rex Murphy" $ do
  it "discovers a clue if succeed a skill test by 2 or more" $ do
    let rexMurphy = lookupInvestigator "02002"
    scenario' <- testScenario "00000" id
    location <- testLocation "00000" (Location.clues .~ 1)
    game <-
      runGameTest
        rexMurphy
        [ BeginSkillTest
            (getId () rexMurphy)
            TestSource
            Nothing
            SkillIntellect
            2
            mempty
            mempty
            mempty
            mempty
        ]
        ((chaosBag .~ Bag [Zero])
        . (locations %~ insertEntity location)
        . (scenario ?~ scenario')
        )
      >>= runGameTestOnlyOption "start skill test"
      >>= runGameTestOnlyOption "apply results"
      >>= runGameTestOptionMatching
            "use ability"
            (\case
              Run{} -> True
              _ -> False
            )
    updated game rexMurphy `shouldSatisfy` hasClueCount 1
    updated game location `shouldSatisfy` hasClueCount 0

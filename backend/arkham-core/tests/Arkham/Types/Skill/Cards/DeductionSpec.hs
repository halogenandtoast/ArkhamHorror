module Arkham.Types.Skill.Cards.DeductionSpec
  ( spec
  )
where

import TestImport.Lifted

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Types.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Deduction" $ do
  it "it allows you to discover another clue if you succeed" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1 }
    location <- testLocation
      $ \attrs -> attrs { locationClues = 2, locationShroud = 2 }
    deduction <- buildPlayerCard "01039"
    gameTest
        investigator
        [ SetTokens [Zero]
        , moveTo investigator location
        , addToHand investigator (PlayerCard deduction)
        , investigate investigator location
        ]
        (locationsL %~ insertEntity location)
      $ do
          runMessages
          chooseOptionMatching
            "commit skill card"
            (\case
              Run{} -> True
              _ -> False
            )
          chooseOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
          chooseOnlyOption "apply results"
          getCount (toId investigator) `shouldReturn` ClueCount 2

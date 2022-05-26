module Arkham.Skill.Cards.DeductionSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Skill.Cards qualified as Cards
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Location.Attrs (LocationAttrs(..))

spec :: Spec
spec = describe "Deduction" $ do
  it "it allows you to discover another clue if you succeed" $ do
    investigator <- testInvestigator
      $ \attrs -> attrs { investigatorIntellect = 1 }
    location <- testLocation
      $ \attrs -> attrs { locationClues = 2, locationShroud = 2 }
    deduction <- genPlayerCard Cards.deduction
    gameTest
        investigator
        [ SetTokens [Zero]
        , moveTo investigator location
        , addToHand investigator (PlayerCard deduction)
        , investigate investigator location
        ]
        (entitiesL . locationsL %~ insertEntity location)
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

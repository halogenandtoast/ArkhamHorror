module Arkham.Types.Skill.Cards.DeductionSpec
  ( spec
  ) where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))
import Arkham.Types.Location.Attrs (Attrs(..))

spec :: Spec
spec = describe "Deduction" $ do
  it "it allows you to discover another clue if you succeed" $ do
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorIntellect = 1 }
    location <- testLocation "00000"
      $ \attrs -> attrs { locationClues = 2, locationShroud = 2 }
    deduction <- buildPlayerCard "01039"
    game <-
      runGameTest
        investigator
        [ SetTokens [Zero]
        , addToHand investigator (PlayerCard deduction)
        , investigate investigator location
        ]
        (locationsL %~ insertEntity location)
      >>= runGameTestOptionMatching
            "commit skill card"
            (\case
              Run{} -> True
              _ -> False
            )
      >>= runGameTestOptionMatching
            "start skill test"
            (\case
              StartSkillTest{} -> True
              _ -> False
            )
      >>= runGameTestOnlyOption "apply results"
    withGame game (asks $ unClueCount . getCount (toId investigator))
      `shouldReturn` 2

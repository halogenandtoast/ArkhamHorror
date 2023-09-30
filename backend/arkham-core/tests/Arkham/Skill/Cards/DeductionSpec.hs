module Arkham.Skill.Cards.DeductionSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..))
import Arkham.Location.Types (LocationAttrs (..))
import Arkham.Skill.Cards qualified as Cards

spec :: Spec
spec = describe "Deduction" $ do
  it "it allows you to discover another clue if you succeed" $ gameTest $ \investigator -> do
    updateInvestigator investigator $
      \attrs -> attrs {investigatorIntellect = 1}
    location <- testLocationWith $
      \attrs -> attrs {locationRevealClues = Static 2, locationShroud = 2}
    deduction <- genCard Cards.deduction
    pushAndRunAll
      [ SetChaosTokens [Zero]
      , moveTo investigator location
      , addToHand (toId investigator) deduction
      , investigate investigator location
      ]
    chooseOptionMatching
      "commit skill card"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    chooseOptionMatching
      "start skill test"
      ( \case
          StartSkillTestButton {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    fieldAssert InvestigatorClues (== 2) investigator

module Arkham.Event.Cards.LuckySpec (
  spec,
) where

import TestImport

import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types (InvestigatorAttrs (..))

spec :: Spec
spec = describe "Lucky!" $ do
  it "adds 2 to a skill test when you would fail" $ gameTest $ \investigator -> do
    updateInvestigator investigator $ \attrs ->
      attrs {investigatorIntellect = 1, investigatorResources = 1}
    lucky <- genCard Cards.lucky

    didPassTest <- didPassSkillTestBy investigator SkillIntellect 0

    pushAndRunAll
      [ SetChaosTokens [MinusOne]
      , addToHand (toId investigator) lucky
      , beginSkillTest investigator SkillIntellect 2
      ]
    chooseOnlyOption "start skill test"
    chooseOptionMatching
      "play lucky!"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didPassTest `refShouldBe` True

  it "does not cause an autofail to pass" $ gameTest $ \investigator -> do
    updateInvestigator investigator $ \attrs ->
      attrs {investigatorIntellect = 1, investigatorResources = 1}
    lucky <- genCard Cards.lucky

    didFailTest <- didFailSkillTestBy investigator SkillIntellect 2

    pushAndRunAll
      [ SetChaosTokens [AutoFail]
      , addToHand (toId investigator) lucky
      , beginSkillTest investigator SkillIntellect 2
      ]

    chooseOnlyOption "start skill test"
    chooseOptionMatching
      "play lucky!"
      ( \case
          TargetLabel {} -> True
          _ -> False
      )
    chooseOnlyOption "apply results"
    didFailTest `refShouldBe` True

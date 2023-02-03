module Arkham.Investigator.Cards.SkidsOTooleSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Game.Helpers (getCanAffordCost)
import Arkham.Investigator.Types (Field(..))
import Arkham.Projection

spec :: Spec
spec = describe "\"Skids\" O'Toole" $ do
  context "ability" $ do
    it "allows you to spend two resources to buy an additional action" $ do
      let skidsOToole = lookupInvestigator "01003"
      gameTest
          skidsOToole
          [ TakeResources (toId skidsOToole) 2 (toSource skidsOToole) False
          , LoseActions (toId skidsOToole) (TestSource mempty) 3
          ]
          id
        $ do
            runMessages
            [buyAction] <- field InvestigatorAbilities (toId skidsOToole)
            push $ UseAbility (toId skidsOToole) buyAction []
            runMessages
            getCanAffordCost
                (toId skidsOToole)
                (TestSource mempty)
                Nothing
                []
                (ActionCost 1)
              `shouldReturn` True

  context "elder sign" $ do
    it "gains 2 resources on success" $ do
      let skidsOToole = lookupInvestigator "01003"
      gameTest
          skidsOToole
          [SetTokens [ElderSign], beginSkillTest skidsOToole SkillAgility 4]
          id
        $ do
            runMessages
            chooseOnlyOption "start skill test"
            chooseOnlyOption "apply results"
            fieldAssert InvestigatorResources (== 2) skidsOToole

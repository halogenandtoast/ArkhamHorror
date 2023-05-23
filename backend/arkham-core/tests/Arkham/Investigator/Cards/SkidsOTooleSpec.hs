module Arkham.Investigator.Cards.SkidsOTooleSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Game.Helpers (getCanAffordCost)
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection

spec :: Spec
spec = describe "\"Skids\" O'Toole" $ do
  context "ability" $ do
    it
      "allows you to spend two resources to buy an additional action"
      $ gameTestWith Investigators.skidsOToole
      $ \skidsOToole -> do
        pushAndRunAll
          [ TakeResources (toId skidsOToole) 2 (toSource skidsOToole) False
          , LoseActions (toId skidsOToole) (TestSource mempty) 3
          ]
        [buyAction] <- field InvestigatorAbilities (toId skidsOToole)
        pushAndRun $ UseAbility (toId skidsOToole) buyAction []
        assert $
          getCanAffordCost
            (toId skidsOToole)
            (TestSource mempty)
            Nothing
            []
            (ActionCost 1)

  context "elder sign" $ do
    it "gains 2 resources on success" $ gameTestWith Investigators.skidsOToole $ \skidsOToole -> do
      pushAndRunAll [SetTokens [ElderSign], beginSkillTest skidsOToole SkillAgility 4]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      fieldAssert InvestigatorResources (== 2) skidsOToole

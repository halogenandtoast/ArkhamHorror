module Arkham.Types.Investigator.Cards.JimCulverSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = describe "Jum Culver" $ do
  context "ability" $ do
    it "changes skull modifier to 0" $ do
      let jimCulver = lookupInvestigator "02004"
      scenario' <- testScenario "00000" id
      game <-
        runGameTest
          jimCulver
          [ BeginSkillTest
              (getId () jimCulver)
              TestSource
              Nothing
              SkillIntellect
              2
              mempty
              mempty
              mempty
              mempty
          ]
          ((scenario ?~ scenario') . (chaosBag .~ Bag [Skull]))
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      game `shouldSatisfy` hasProcessedMessage
        (RunSkillTest (getId () jimCulver) (TokenValue Skull 0))

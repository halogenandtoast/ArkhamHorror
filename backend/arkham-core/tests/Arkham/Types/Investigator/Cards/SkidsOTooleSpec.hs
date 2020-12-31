module Arkham.Types.Investigator.Cards.SkidsOTooleSpec
  ( spec
  )
where

import TestImport

spec :: Spec
spec = describe "\"Skids\" O'Toole" $ do
  context "ability" $ do
    it "allows you to spend two resources to buy an additional action" $ do
      let skidsOToole = lookupInvestigator "01003"
      game <- runGameTest
        skidsOToole
        [ TakeResources (toId skidsOToole) 2 False
        , LoseActions (toId skidsOToole) (TestSource mempty) 3
        ]
        id
      let skidsOToole' = updated game skidsOToole
      [buyAction] <- getActionsOf
        game
        skidsOToole'
        (DuringTurn You)
        skidsOToole'
      game' <- runGameTestMessages game [buyAction]
      withGame
          game'
          (getCanAffordCost
            (toId skidsOToole')
            (TestSource mempty)
            Nothing
            (ActionCost 1)
          )
        `shouldReturn` True

  context "elder sign" $ do
    it "gains 2 resources on success" $ do
      let skidsOToole = lookupInvestigator "01003"
      game <-
        runGameTest
          skidsOToole
          [SetTokens [ElderSign], beginSkillTest skidsOToole SkillAgility 4]
          id
        >>= runGameTestOnlyOption "start skill test"
        >>= runGameTestOnlyOption "apply results"
      updatedResourceCount game skidsOToole `shouldBe` 2

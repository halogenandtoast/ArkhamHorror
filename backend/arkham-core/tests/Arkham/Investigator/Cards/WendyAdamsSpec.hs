module Arkham.Investigator.Cards.WendyAdamsSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = describe "Wendy Adams" $ do
  context "ability" $ do
    it "allows you to discard a card to redraw a chaos token" $ gameTestWith Investigators.wendyAdams $ \wendyAdams -> do
      card <- testPlayerCard id

      didPassTest <- didPassSkillTestBy wendyAdams SkillWillpower 0

      pushAndRunAll
        [ SetTokens [MinusOne]
        , addToHand (toId wendyAdams) (PlayerCard card)
        , beginSkillTest wendyAdams SkillWillpower 3
        ]
      chooseOnlyOption "start skill test"
      chooseOptionMatching
        "use ability"
        ( \case
            AbilityLabel {} -> True
            _ -> False
        )
      chooseOnlyOption "discard card"
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True

  context "elder sign" $ do
    it "gives +0" $ gameTestWith Investigators.wendyAdams $ \wendyAdams -> do
      didPassTest <- didPassSkillTestBy wendyAdams SkillWillpower 0

      pushAndRunAll [SetTokens [ElderSign], beginSkillTest wendyAdams SkillWillpower 4]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True

    it "automatically succeeds if Wendy's Amulet is in play" $ gameTestWith Investigators.wendyAdams $ \wendyAdams -> do
      putCardIntoPlay wendyAdams Assets.wendysAmulet

      didPassTest <- didPassSkillTestBy wendyAdams SkillWillpower 4

      pushAndRunAll
        [ SetTokens [ElderSign]
        , beginSkillTest wendyAdams SkillWillpower 20
        ]
      chooseOnlyOption "start skill test"
      chooseOnlyOption "apply results"
      didPassTest `refShouldBe` True

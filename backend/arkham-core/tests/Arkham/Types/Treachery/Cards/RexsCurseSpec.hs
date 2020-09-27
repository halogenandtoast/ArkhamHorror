module Arkham.Types.Treachery.Cards.RexsCurseSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = describe "Rex's Curse" $ do
  it "is put into play into your threat area" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
    game <- runGameTest
      investigator
      [loadDeck investigator [rexsCurse], drawCards investigator 1]
      id
    updated game investigator
      `shouldSatisfy` hasTreacheryWithMatchingCardCode
                        game
                        (PlayerCard rexsCurse)
  it "causes you to reveal another token" $ do
    investigator <- testInvestigator "00000" id
    rexsCurse <- buildPlayerCard "02009"
    scenario' <- testScenario "00000" id
    game <-
      runGameTest
          investigator
          [ loadDeck investigator [rexsCurse]
          , drawCards investigator 1
          , BeginSkillTest
            (getId () investigator)
            TestSource
            Nothing
            SkillIntellect
            4
            mempty
            mempty
            mempty
            mempty
          ]
          ((scenario ?~ scenario') . (chaosBag .~ Bag [Zero, PlusOne]))
        >>= runGameTestOnlyOption "start skill test"
    updated game investigator
      `shouldSatisfy` hasTreacheryWithMatchingCardCode
                        game
                        (PlayerCard rexsCurse)
  it "is shuffled back into your deck if you fail the test" pending

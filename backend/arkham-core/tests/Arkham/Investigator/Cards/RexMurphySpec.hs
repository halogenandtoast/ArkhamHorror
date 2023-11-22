module Arkham.Investigator.Cards.RexMurphySpec (spec) where

import Arkham.Investigator.Cards
import Arkham.Location.Types as Location
import TestImport.New

spec :: Spec
spec = describe "Rex Murphy" $ do
  context "special ability" $ do
    it "discovers a clue if succeed a skill test by 2 or more" $ gameTestWith rexMurphy $ \self -> do
      location1 <- testLocationWith (Location.revealCluesL .~ Static 2)
      setChaosTokens [Zero]
      self `moveTo` location1
      run $ beginActionSkillTest self #investigate (Just $ toTarget location1) #intellect 2
      click "start skill test"
      click "apply results"
      useReaction
      self.clues `shouldReturn` 2
      location1.clues `shouldReturn` 0

  context "elder sign token" $ do
    it "can autofail to draw 3 cards" $ gameTestWith rexMurphy $ \self -> do
      cards <- testPlayerCards 3
      setChaosTokens [ElderSign]
      self `loadDeckCards` cards
      runSkillTest self #intellect 2
      clickLabel "Automatically fail to draw 3"
      assertFailedSkillTest
      click "apply results"
      self.hand `shouldMatchListM` map PlayerCard cards

    it "can resolve normally with +2" $ gameTestWith rexMurphy $ \self -> do
      cards <- testPlayerCards 3
      setChaosTokens [ElderSign]
      self `loadDeckCards` cards
      runSkillTest self #intellect 6 -- two higher
      clickLabel "Resolve normally"
      assertPassedSkillTest
      click "apply results"
      self.hand `shouldReturn` []

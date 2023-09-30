module Arkham.Asset.Cards.TheNecronomiconSpec (spec) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Investigator.Types (Investigator)
import Arkham.Matcher (AssetMatcher (..), assetIs)
import Arkham.Window (WindowType (DealtHorror))
import TestImport.New

setup :: Investigator -> TestAppT ()
setup self = do
  self `loadDeck` [Cards.theNecronomicon]
  self `drawCards` 1

spec :: Spec
spec = describe "The Necronomicon" $ do
  context "Revelation" $ do
    it "comes into play with 3 horror on it" . gameTest $ \self -> do
      setup self
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      theNecronomicon.horror `shouldReturn` 3

    it "cannot leave play while it has 1 or more horror on it" . gameTest $ \self -> do
      setup self
      assert $ selectNone DiscardableAsset

  context "Constant Ability" $ do
    it "treats elder sign tokens as auto fail" . gameTest $ \self -> do
      setup self
      setChaosTokens [ElderSign]
      runSkillTest self #willpower 0
      assertFailedSkillTest

  context "Action Ability" $ do
    it "it moves on horror to daisy" . gameTest $ \self -> do
      setup self
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      [action] <- theNecronomicon.abilities
      self `useAbility` action
      theNecronomicon.horror `shouldReturn` 2
      self.horror `shouldReturn` 1

    it "it is discarded when there is no more horror" . gameTest $ \self -> do
      setup self
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      [action] <- theNecronomicon.abilities
      replicateM_ 3 $ self `useAbility` action
      assert $ Cards.theNecronomicon `isInDiscardOf` self

    it
      "[FAQ] Moving damage or horror is different from 'dealing' or 'taking' damage or horror, so does not trigger effects or allow the player controlling Daisy Walker to reassign the horror."
      . gameTest
      $ \self -> do
        setup self
        let
          dealtHorrorWindow = \case
            DealtHorror {} -> True
            _ -> False
        ref <- createMessageChecker \case
          CheckWindow _ ws -> any (dealtHorrorWindow . windowType) ws
          _ -> False
        theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
        [action] <- theNecronomicon.abilities
        self `useAbility` action
        ref `refShouldBe` False

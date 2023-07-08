module Arkham.Asset.Cards.TheNecronomiconSpec (
  spec,
)
where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Types (Field (..))
import Arkham.Investigator.Types (Field (..), Investigator)
import Arkham.Matcher (AssetMatcher (..), assetIs)
import Arkham.Projection
import Arkham.Window (WindowType (DealtHorror))

setup :: Investigator -> TestAppT ()
setup investigator = do
  card <- genPlayerCard Cards.theNecronomicon
  drawing <- drawCards (toId investigator) investigator 1
  pushAndRunAll [loadDeck investigator [card], drawing]

spec :: Spec
spec = describe "The Necronomicon" $ do
  context "Revelation" $ do
    it "comes into play with 3 horror on it" . gameTest $ \investigator -> do
      setup investigator
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      fieldAssert AssetHorror (== 3) theNecronomicon

    it "cannot leave play while it has 1 or more horror on it" . gameTest $ \investigator -> do
      setup investigator
      assert $ selectNone DiscardableAsset

  context "Constant Ability" $ do
    it "treats elder sign tokens as auto fail" . gameTest $ \investigator -> do
      setup investigator
      ref <- createMessageChecker \case
        FailSkillTest -> True
        _ -> False
      pushAndRunAll
        [ SetTokens [ElderSign]
        , beginSkillTest investigator SkillWillpower 0
        ]
      chooseOnlyOption "Start skill test"
      ref `refShouldBe` True

  context "Action Ability" $ do
    it "it moves on horror to daisy" . gameTest $ \investigator -> do
      setup investigator
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      [action] <- field AssetAbilities theNecronomicon
      pushAndRun $ UseAbility (toId investigator) action []
      fieldAssert AssetHorror (== 2) theNecronomicon
      fieldAssert InvestigatorHorror (== 1) investigator

    fit "it is discarded when there is no more horror" . gameTest $ \investigator -> do
      setup investigator
      theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
      [action] <- field AssetAbilities theNecronomicon
      pushAndRunAll $ replicate 3 $ UseAbility (toId investigator) action []
      assert $ isInDiscardOf investigator Cards.theNecronomicon

    it
      "[FAQ] Moving damage or horror is different from 'dealing' or 'taking' damage or horror, so does not trigger effects or allow the player controlling Daisy Walker to reassign the horror."
      . gameTest
      $ \investigator -> do
        setup investigator
        let
          dealtHorrorWindow = \case
            DealtHorror {} -> True
            _ -> False
        ref <- createMessageChecker \case
          CheckWindow _ ws -> any (dealtHorrorWindow . windowType) ws
          _ -> False
        theNecronomicon <- selectJust $ assetIs Cards.theNecronomicon
        [action] <- field AssetAbilities theNecronomicon
        pushAndRun $ UseAbility (toId investigator) action []
        ref `refShouldBe` False

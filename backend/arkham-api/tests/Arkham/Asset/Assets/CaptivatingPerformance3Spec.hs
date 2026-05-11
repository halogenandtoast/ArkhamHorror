module Arkham.Asset.Assets.CaptivatingPerformance3Spec (spec) where

import Arkham.Action (Action (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Runner (longestUniqueStreak, pickSDR)
import TestImport.New

spec :: Spec
spec = describe "Captivating Performance (3)" do
  -- input is newest-first: head is the just-completed action
  describe "longestUniqueStreak" do
    it "extends through multi-type actions when an SDR exists" . gameTest $ \_ -> liftIO do
      -- latest=[#play], prev=[#fight,#activate], oldest=[#fight,#activate]
      length (longestUniqueStreak @Action [[#play], [#fight, #activate], [#fight, #activate]])
        `shouldBe` 3

    it "stops at 1 when the previous action repeats the latest type" . gameTest $ \_ -> liftIO do
      -- latest=[#fight], prev=[#fight]: no distinct assignment
      length (longestUniqueStreak @Action [[#fight], [#fight]]) `shouldBe` 1

    it "stops at 2 when extending further would force a duplicate" . gameTest $ \_ -> liftIO do
      -- latest=[#fight,#activate] picks #activate, prev=[#fight] picks #fight,
      -- oldest=[#fight] cannot pick a new type
      length (longestUniqueStreak @Action [[#fight, #activate], [#fight], [#fight]]) `shouldBe` 2

    it "drops only the prefix that breaks the SDR" . gameTest $ \_ -> liftIO do
      -- latest=[#investigate], then [#move], [#fight] form a 3-streak;
      -- oldest [#fight] is excluded
      length (longestUniqueStreak @Action [[#investigate], [#move], [#fight], [#fight]])
        `shouldBe` 3

  describe "pickSDR" do
    it "returns one chosen type per action with all types distinct" . gameTest $ \_ -> liftIO do
      let picks = pickSDR @Action [[#fight, #activate], [#fight, #activate], [#play]]
      length picks `shouldBe` 3
      length picks `shouldBe` length (nub picks)

    it "returns [] when no SDR exists" . gameTest $ \_ -> liftIO do
      pickSDR @Action [[#fight], [#fight]] `shouldBe` []

  context "Reaction trigger" do
    it "fires after #fight+#activate, #fight+#activate, #play (three multi-type actions admitting an SDR)" . gameTest $ \self -> do
      cp <- self `putAssetIntoPlay` Assets.captivatingPerformance3
      duringTurn self $ do
        run $ TakenActions self.id [#fight, #activate]
        run $ TakenActions self.id [#fight, #activate]
        run $ TakenActions self.id [#play]
        useReactionOf cp

module Arkham.Asset.Assets.CaptivatingPerformance3Spec (spec) where

import Arkham.Action (Action (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Action (longestUniqueStreak, pickSDR, sdrExists)
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

    it "stops at 2 across an or-actions fight/evade activate (#4553)" . gameTest $ \_ -> liftIO do
      -- Sword Cane attack chosen as fight records [#activate, #fight], not
      -- [#activate, #fight, #evade]; followed by a plain [#activate] (e.g.
      -- Marie's Psychosis), preceded by an earlier [#fight].
      length (longestUniqueStreak @Action [[#activate], [#activate, #fight], [#fight]])
        `shouldBe` 2

  describe "pickSDR" do
    it "returns one chosen type per action with all types distinct" . gameTest $ \_ -> liftIO do
      let picks = pickSDR @Action [[#fight, #activate], [#fight, #activate], [#play]]
      length picks `shouldBe` 3
      length picks `shouldBe` length (nub picks)

    it "returns [] when no SDR exists" . gameTest $ \_ -> liftIO do
      pickSDR @Action [[#fight], [#fight]] `shouldBe` []

  -- The bonus fourth action is eligible only if it is disjoint from at least
  -- one complete SDR of the streak, i.e. the streak still admits an SDR once the
  -- candidate's types are removed from every group. A weapon/spell "[action]:
  -- Fight" counts as both an activate and a fight action.
  describe "fourth-action eligibility" do
    let canTake groups xs = sdrExists (map (filter (`notElem` xs)) groups)
    it "excludes a multi-type action sharing a type with every SDR (#4834)" . gameTest $ \_ -> liftIO do
      -- streak Play, Move, Activate
      let groups = [[#play], [#move], [#activate]] :: [[Action]]
      canTake groups [#activate, #fight] `shouldBe` False -- Azure Flame
      canTake groups [#activate, #fight, #evade] `shouldBe` False -- Sword Cane
      canTake groups [#activate, #parley] `shouldBe` False -- Knight of the Outer Void's Parley
      canTake groups [#play] `shouldBe` False
      canTake groups [#fight] `shouldBe` True -- basic Fight
      canTake groups [#investigate] `shouldBe` True
      canTake groups [#resource] `shouldBe` True

    it "allows a multi-type action disjoint from one SDR of an ambiguous streak" . gameTest $ \_ -> liftIO do
      -- two SDR sets: {inv,res,activate} and {inv,res,fight}
      let groups = [[#investigate], [#resource], [#activate, #fight]] :: [[Action]]
      canTake groups [#activate, #fight] `shouldBe` False
      canTake groups [#activate, #evade] `shouldBe` True
      canTake groups [#move] `shouldBe` True

  context "Reaction trigger" do
    it "fires after #fight+#activate, #fight+#activate, #play (three multi-type actions admitting an SDR)" . gameTest $ \self -> do
      cp <- self `putAssetIntoPlay` Assets.captivatingPerformance3
      duringTurn self $ do
        run $ TakenActions self.id [#fight, #activate]
        run $ TakenActions self.id [#fight, #activate]
        run $ TakenActions self.id [#play]
        useReactionOf cp

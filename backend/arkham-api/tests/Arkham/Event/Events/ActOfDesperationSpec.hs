module Arkham.Event.Events.ActOfDesperationSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Act of Desperation" do
  it "gains resources when the test is repeated by Live and Learn" . gameTest $ \self -> do
    withProp @"combat" 1 self
    withProp @"resources" 0 self
    location <- testLocation
    enemy <- testEnemy & prop @"fight" 5 & prop @"health" 5
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `putAssetIntoPlay` Assets.fortyOneDerringer
    actOfDesperation <- genCard Events.actOfDesperation
    liveAndLearn <- genCard Events.liveAndLearn
    self `addToHand` actOfDesperation
    self `addToHand` liveAndLearn
    duringTurn self do
      self `playCard` actOfDesperation
      chooseOptionMatching "discard the Derringer to pay the additional cost" \case
        TargetLabel (CardIdTarget _) _ -> True
        _ -> False
      chooseTarget enemy
      -- 1 combat + 3 (the Derringer's printed cost) vs fight 5, failed by 1
      startSkillTest
      applyResults
      self.resources `shouldReturn` 0
      chooseTarget liveAndLearn
      -- 1 + 3 + 2 vs fight 5 succeeds by 1, so the discarded asset's cost is
      -- still gained even though the rider was attached to the original test
      startSkillTest
      applyResults
      -- the attack damage and Act of Desperation's payout are both on-success
      -- effects, so the player orders them; take the resources first
      chooseOptionMatching "gain resources from Act of Desperation" \case
        Label _ (TakeResources {} : _) -> True
        _ -> False
      self.resources `shouldReturn` 3

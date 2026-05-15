module Arkham.Event.Events.RefineSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Refine" do
  it "is limited to once per game per investigator" . gameTest $ \self -> do
    let doSetOwner = setPlayerCardOwner (toId self)
    refine1 <- genPlayerCardWith Events.refine doSetOwner
    refine2 <- genPlayerCardWith Events.refine doSetOwner
    shotcaster <- genPlayerCardWith Assets.hyperphysicalShotcasterTheoreticalDevice doSetOwner
    withProp @"resources" 10 self
    withProp @"hand" [toCard refine1, toCard refine2] self
    withProp @"deck" (Deck [shotcaster]) self
    duringTurn self do
      asDefs self.playableCards `shouldMatchListM` [Events.refine, Events.refine]
      self `playCard` toCard refine1
      chooseOptionMatching "pick Hyperphysical Shotcaster as Refine target" \case
        TargetLabel {} -> True
        _ -> False
      clickLabel "Railshooter"
      asDefs self.playableCards `shouldReturn` []

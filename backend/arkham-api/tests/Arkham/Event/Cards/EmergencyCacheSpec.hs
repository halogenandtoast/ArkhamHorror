module Arkham.Event.Cards.EmergencyCacheSpec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = do
  describe "Emergency Cache" $ do
    it "should increase the investigators resources by 3" . gameTest $ \self -> do
      withProp @"resources" 0 self
      self `playEvent` Events.emergencyCache
      self.resources `shouldReturn` 3

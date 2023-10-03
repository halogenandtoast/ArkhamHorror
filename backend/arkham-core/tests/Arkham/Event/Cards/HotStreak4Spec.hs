module Arkham.Event.Cards.HotStreak4Spec (spec) where

import Arkham.Event.Cards qualified as Events
import TestImport.New

spec :: Spec
spec = describe "Hot Streak (4)" $ do
  it "gain 10 resources" . gameTest $ \self -> do
    withProp @"resources" 0 self
    self `playEvent` Events.hotStreak4
    self.resources `shouldReturn` 10

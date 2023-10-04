module Arkham.Treachery.Cards.ParanoiaSpec (spec) where

import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Paranoia" $ do
  it "discards all of your resources" . gameTest $ \self -> do
    withProp @"resources" 10 self
    self `drawsCard` Treacheries.paranoia
    self.resources `shouldReturn` 0

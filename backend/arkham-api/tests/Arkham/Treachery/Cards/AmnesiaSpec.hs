module Arkham.Treachery.Cards.AmnesiaSpec (spec) where

import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Amnesia" $ do
  it "discards all cards but 1 from your hand" . gameTest $ \self -> do
    cards <- testPlayerCards 4
    withProp @"hand" (map toCard cards) self
    (keep : others) <- shuffleM cards
    self `drawsCard` Treacheries.amnesia
    for_ others chooseTarget
    self.hand `shouldReturn` [toCard keep]
    -- amnesia should be on top, so we drop it
    (drop 1 <$> self.discard) `shouldMatchListM` others

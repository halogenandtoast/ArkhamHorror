module Arkham.Asset.Assets.DendromorphosisSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import TestImport.Lifted

spec :: Spec
spec = describe "Dendromorphosis" $ do
  -- Dendromorphosis takes both hand slots and cannot leave play, so a second copy has no
  -- slot to take and nothing discardable to make room. It enters play unslotted rather
  -- than being discarded to satisfy the slot limit (#5424).
  it "does not discard a copy that cannot leave play when hand slots are full"
    $ gameTest
    $ \investigator -> do
      putCardIntoPlay investigator Assets.dendromorphosis
      putCardIntoPlay investigator Assets.dendromorphosis

      copies <- select $ assetIs Assets.dendromorphosis
      liftIO $ length copies `shouldBe` 2

      discarded <- Assets.dendromorphosis `isInDiscardOf` investigator
      liftIO $ discarded `shouldBe` False

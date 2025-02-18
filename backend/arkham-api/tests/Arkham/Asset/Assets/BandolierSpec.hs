module Arkham.Asset.Assets.BandolierSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import Arkham.Projection
import Arkham.Slot
import Arkham.Trait
import TestImport.Lifted

spec :: Spec
spec = describe "Bandolier" $ do
  it "adds a weapon hand slot"
    $ gameTest
    $ \investigator -> do
      putCardIntoPlay investigator Assets.bandolier
      bandolier <- selectJust $ assetIs Assets.bandolier
      slots <- findWithDefault [] HandSlot <$> field InvestigatorSlots (toId investigator)
      slots
        `shouldSatisfy` elem
          (TraitRestrictedSlot (toSource bandolier) Weapon [])

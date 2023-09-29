module Arkham.Asset.Cards.BandolierSpec (
  spec,
) where

import TestImport.Lifted

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Trait

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

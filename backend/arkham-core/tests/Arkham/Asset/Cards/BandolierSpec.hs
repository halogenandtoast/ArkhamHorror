module Arkham.Asset.Cards.BandolierSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Investigator.Attrs (Field(..))
import Arkham.Trait
import Arkham.Projection

spec :: Spec
spec = describe "Bandolier" $ do
  it "adds a weapon hand slot" $ do
    investigator <- testInvestigator id
    bandolier <- buildAsset "02147" (Just investigator)
    gameTest
        investigator
        [playAsset investigator bandolier]
        (entitiesL . assetsL %~ insertEntity bandolier)
      $ do
          runMessages
          slots <- findWithDefault [] HandSlot <$> field InvestigatorSlots (toId investigator)
          slots `shouldSatisfy` elem
            (TraitRestrictedSlot (toSource bandolier) Weapon Nothing)

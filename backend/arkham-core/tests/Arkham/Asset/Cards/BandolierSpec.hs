module Arkham.Asset.Cards.BandolierSpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Investigator.Attrs qualified as Investigator
import Arkham.Slot
import Arkham.Trait

spec :: Spec
spec = describe "Bandolier" $ do
  it "adds a weapon hand slot" $ do
    investigator <- testInvestigator id
    bandolier <- buildAsset "02147"
    gameTest
        investigator
        [playAsset investigator bandolier]
        (entitiesL . assetsL %~ insertEntity bandolier)
      $ do
          runMessages
          investigator' <- updated investigator
          let
            slots =
              fromMaybe []
                $ toAttrs investigator'
                ^? Investigator.slotsL
                . ix HandSlot
          slots `shouldSatisfy` elem
            (TraitRestrictedSlot (toSource bandolier) Weapon Nothing)

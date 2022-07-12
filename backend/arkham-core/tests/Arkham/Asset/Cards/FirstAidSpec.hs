module Arkham.Asset.Cards.FirstAidSpec
  ( spec
  ) where

import TestImport.Lifted

import qualified Arkham.Asset.Cards as Assets
import qualified Arkham.Investigator.Cards as Investigators
import qualified Arkham.Investigator.Attrs as Investigator
import Arkham.Projection
import Arkham.Asset.Attrs (Field(..))

spec :: Spec
spec = describe "First Aid" $ do
  it
      "uses a supply and heals 1 damage or horror from an investigator at your location"
    $ do
        investigator <- testInvestigator Investigators.jennyBarnes (Investigator.healthDamageL .~ 1)
        investigator2 <- testInvestigator Investigators.rolandBanks (Investigator.sanityDamageL .~ 1)
        investigator3 <- testInvestigator Investigators.daisyWalker (Investigator.sanityDamageL .~ 1)
        (location1, location2) <- testConnectedLocations id id
        firstAid <- buildAsset Assets.firstAid (Just investigator)
        gameTest
            investigator
            [ playAsset investigator firstAid
            , moveTo investigator location1
            , moveTo investigator2 location1
            , moveTo investigator3 location2
            ]
            ( (entitiesL . investigatorsL %~ insertEntity investigator2)
            . (entitiesL . investigatorsL %~ insertEntity investigator3)
            . (entitiesL . locationsL %~ insertEntity location1)
            . (entitiesL . locationsL %~ insertEntity location2)
            . (entitiesL . assetsL %~ insertEntity firstAid)
            )
          $ do
              runMessages
              [useFirstAid] <- field AssetAbilities (toId firstAid)
              pushAndRun $ UseAbility (toId investigator) useFirstAid []
              pending


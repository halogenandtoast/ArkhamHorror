module Arkham.Asset.Cards.FirstAidSpec
  ( spec
  ) where

import TestImport.Lifted hiding (InvestigatorDamage)

import Arkham.Asset.Types ( Field (..) )
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Attrs ( Field (..), healthDamageL, sanityDamageL )
import Arkham.Investigator.Cards ( daisyWalker, jennyBarnes, rolandBanks )
import Arkham.Projection

spec :: Spec
spec = describe "First Aid" $ do
  it
      "uses a supply and heals 1 damage or horror from an investigator at your location"
    $ do
        investigator <- testInvestigator jennyBarnes (healthDamageL .~ 1)
        investigator2 <- testInvestigator rolandBanks (sanityDamageL .~ 1)
        investigator3 <- testInvestigator daisyWalker (sanityDamageL .~ 1)
        firstAid <- buildAsset Assets.firstAid (Just investigator)
        (location1, location2) <- testConnectedLocations id id
        gameTest
            investigator
            [ playAsset investigator firstAid
            , moveTo investigator location1
            , moveTo investigator2 location1
            , moveTo investigator3 location2
            ]
            ((entitiesL . investigatorsL %~ insertEntity investigator2)
            . (entitiesL . investigatorsL %~ insertEntity investigator3)
            . (entitiesL . locationsL %~ insertEntity location1)
            . (entitiesL . locationsL %~ insertEntity location2)
            . (entitiesL . assetsL %~ insertEntity firstAid)
            )
          $ do
              runMessages
              [useFirstAid] <- field AssetAbilities (toId firstAid)
              pushAndRun $ UseAbility (toId investigator) useFirstAid []
              chooseOptionMatching "choose self" $ \case
                TargetLabel (InvestigatorTarget iid) _ ->
                  iid == toId investigator
                _ -> False
              chooseOnlyOption "heal self"
              pushAndRun $ UseAbility (toId investigator) useFirstAid []
              chooseOnlyOption "choose investigator at same location"
              chooseOnlyOption "heal other investigator"
              fieldAssert InvestigatorDamage (== 0) investigator
              fieldAssert InvestigatorHorror (== 0) investigator2
              fieldAssert InvestigatorHorror (== 1) investigator3

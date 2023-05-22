module Arkham.Asset.Cards.FirstAidSpec (
  spec,
) where

import TestImport.Lifted hiding (InvestigatorDamage)

import Arkham.Asset.Cards qualified as Assets
import Arkham.Asset.Types (Field (..))
import Arkham.Investigator.Cards (daisyWalker, rolandBanks)
import Arkham.Investigator.Types (Field (..), healthDamageL, sanityDamageL)
import Arkham.Matcher (assetIs)
import Arkham.Projection

spec :: Spec
spec = describe "First Aid" $ do
  it
    "uses a supply and heals 1 damage or horror from an investigator at your location"
    $ gameTest
    $ \investigator -> do
      updateInvestigator investigator (healthDamageL .~ 1)
      investigator2 <- addInvestigator rolandBanks (sanityDamageL .~ 1)
      investigator3 <- addInvestigator daisyWalker (sanityDamageL .~ 1)
      putCardIntoPlay investigator Assets.firstAid
      firstAid <- selectJust $ assetIs Assets.firstAid
      (location1, location2) <- testConnectedLocations id id
      pushAndRun $ moveTo investigator location1
      pushAndRun $ moveTo investigator2 location1
      pushAndRun $ moveTo investigator3 location2
      [useFirstAid] <- field AssetAbilities firstAid
      pushAndRun $ UseAbility (toId investigator) useFirstAid []
      chooseOptionMatching "choose self" $ \case
        TargetLabel (InvestigatorTarget iid) _ ->
          iid == toId investigator
        _ -> False
      pushAndRun $ UseAbility (toId investigator) useFirstAid []
      chooseOnlyOption "choose investigator at same location"
      fieldAssert InvestigatorDamage (== 0) investigator
      fieldAssert InvestigatorHorror (== 0) investigator2
      fieldAssert InvestigatorHorror (== 1) investigator3

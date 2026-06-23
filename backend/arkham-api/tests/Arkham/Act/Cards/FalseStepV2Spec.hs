module Arkham.Act.Cards.FalseStepV2Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards (rolandBanks)
import Arkham.Matcher
import Arkham.Strategy
import TestImport.New

-- False Step (v. II)'s Forced ability deals X total damage to investigators at
-- Café Luna, "divided as they wish". This is normal (non-direct) damage, so the
-- players must be able to soak it onto their assets. The underlying engine
-- behaviour is the `AmongInvestigators` damage strategy (issue #4904).
spec :: Spec
spec = describe "False Step (v. II)" do
  it "damage dealt to investigators (AmongInvestigators) can be soaked onto assets" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    -- Leather Jacket gives 2 health of soak
    leatherJacket <- self `putAssetIntoPlay` Assets.leatherJacket

    run
      $ InvestigatorAssignDamage
        (toId self)
        (TestSource mempty)
        (AmongInvestigators $ InvestigatorAt $ LocationWithId $ toId location)
        1
        0

    self `assignDamageTo` leatherJacket

    leatherJacket.damage `shouldReturn` 1
    self.damage `shouldReturn` 0

  it "AmongInvestigators damage can be divided across multiple investigators and their assets" . gameTest $ \self -> do
    roland <- addInvestigator rolandBanks
    location <- testLocation
    self `moveTo` location
    roland `moveTo` location
    leatherJacket <- self `putAssetIntoPlay` Assets.leatherJacket

    run
      $ InvestigatorAssignDamage
        (toId self)
        (TestSource mempty)
        (AmongInvestigators $ InvestigatorAt $ LocationWithId $ toId location)
        2
        0

    -- soak one point onto self's jacket, then let the rest fall through
    self `assignDamageTo` leatherJacket
    applyAllDamage

    leatherJacket.damage `shouldReturn` 1

  it "AmongInvestigators damage can still be assigned directly to an investigator" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location

    run
      $ InvestigatorAssignDamage
        (toId self)
        (TestSource mempty)
        (AmongInvestigators $ InvestigatorAt $ LocationWithId $ toId location)
        2
        0

    applyAllDamage

    self.damage `shouldReturn` 2

module Arkham.Treachery.Cards.ClawsOfSteamSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "Claws of Steam" do
  -- Regression for #4876: Claws of Steam assigns 2 damage with assets-first.
  -- When the only damageable asset can soak just 1 of the 2 damage, the overflow
  -- must spill onto the investigator instead of being silently dropped.
  it "spills overflow onto the investigator when the only asset can't soak all the damage" . gameTest $ \self -> do
    -- Aleksey Saburov has 2 health; pre-damage him by 1 so he can absorb only 1 more.
    aleksey <- self `putAssetIntoPlay` Assets.alekseySaburovAlwaysOnTheMend
    run $ DealAssetDamage aleksey (TestSource mempty) 1 0
    aleksey.damage `shouldReturn` 1

    -- Mirrors Claws of Steam's FailedThisSkillTest effect.
    run $ InvestigatorAssignDamage self.id (TestSource mempty) (DamageAssetsFirst AnyAsset) 2 0
    applyAllDamage

    -- Aleksey soaks 1 (and is defeated); the remaining 1 spills onto the investigator.
    self.damage `shouldReturn` 1
    assert $ selectNone $ assetIs Assets.alekseySaburovAlwaysOnTheMend

  -- Guard the common path: an asset that can soak the full amount takes it all and
  -- the investigator is untouched.
  it "keeps all damage on the asset when it can soak the full amount" . gameTest $ \self -> do
    _ <- self `putAssetIntoPlay` Assets.alekseySaburovAlwaysOnTheMend

    run $ InvestigatorAssignDamage self.id (TestSource mempty) (DamageAssetsFirst AnyAsset) 2 0
    applyAllDamage

    self.damage `shouldReturn` 0
    -- 2 damage on 2 health defeats Aleksey
    assert $ selectNone $ assetIs Assets.alekseySaburovAlwaysOnTheMend

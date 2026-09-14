module Arkham.Matcher.AlternatePrintingSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.NightOfTheZealot qualified as Enemies
import Arkham.Matcher
import TestImport.New

-- 'toCardCodePairs' registers every entry of 'cdAlternateCardCodes' as its own
-- 'CardDef' with 'cdCardCode' rewritten to that printing, so a card in play can
-- carry a code the engine-side def never mentions. The '*Is' matchers ask "is
-- this card X", not "is this printing X", so they must see through that (#5596:
-- the Revised Core Baseball Bat never matched the Pinch Hitter achievement's
-- 'assetIs Assets.baseballBat').

printing :: CardCode -> CardDef
printing code = fromJustNote ("missing printing " <> show code) $ lookupCardDef code

spec :: Spec
spec = describe "matching a card across printings" do
  it "assetIs matches a reprinted asset (Baseball Bat 01074 / 01574)" . gameTest $ \self -> do
    bat <- testAssetWithDef (printing "01574") id self
    selectJust (assetIs Assets.baseballBat) `shouldReturn` toId bat

  it "enemyIs matches a reprinted enemy (Mob Enforcer 01101 / 01601)" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    -- Mob Enforcer's prey is OnlyPrey (BearerOf …), so spawning it needs a bearer
    enforcer <- createWeaknessEnemy self (printing "01601")
    enforcer `spawnAt` location
    selectJust (enemyIs Enemies.mobEnforcer) `shouldReturn` toId enforcer

  it "still distinguishes different cards" . gameTest $ \self -> do
    _ <- testAssetWithDef (printing "01574") id self
    assertNone $ assetIs Assets.rabbitsFoot

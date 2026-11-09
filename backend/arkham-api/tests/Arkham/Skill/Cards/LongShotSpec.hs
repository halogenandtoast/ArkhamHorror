module Arkham.Skill.Cards.LongShotSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Skill.Cards qualified as Skills
import TestImport.New

spec :: Spec
spec = describe "Long Shot" do
  -- #5978: a fight performed through a weapon that hooks its own result carries a
  -- ProxyTarget (EnemyTarget ...) (AssetTarget ...), so reading the skill test
  -- target as a bare EnemyTarget threw "invalid target" and locked the game on
  -- Apply Results.
  it "deals its damage on a fight whose skill test target is a proxy" . gameTest $ \self -> do
    withProp @"combat" 2 self
    gun <- self `putAssetIntoPlay` Assets.gatlingGun5
    longShot <- genCard Skills.longShot
    self `addToHand` longShot
    enemy <- testEnemy & prop @"fight" 1 & prop @"health" 5
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    [doFight] <- gun.abilities
    self `useAbility` doFight
    payUpTo 6 1
    chooseTarget enemy
    commit longShot
    startSkillTest
    applyResults
    -- the gun's damage and Long Shot's rider are both on-success options, so the
    -- player orders them; picking either resolves both
    chooseResultOption
    -- 1 from the gun (NoStandardDamage, DamageDealt 1 for the single ammo spent)
    -- + 1 from Long Shot
    enemy.damage `shouldReturn` 2

  it "deals its damage on a basic fight" . gameTest $ \self -> do
    withProp @"combat" 2 self
    longShot <- genCard Skills.longShot
    self `addToHand` longShot
    enemy <- testEnemy & prop @"fight" 1 & prop @"health" 5
    location <- testLocation
    setChaosTokens [Zero]
    enemy `spawnAt` location
    self `moveTo` location
    void $ self `fightEnemy` enemy
    commit longShot
    startSkillTest
    applyResults
    chooseResultOption
    -- 1 standard attack damage + 1 from Long Shot
    enemy.damage `shouldReturn` 2

chooseResultOption :: HasCallStack => TestAppT ()
chooseResultOption = chooseOptionMatching "order the skill test result options" \case
  Label {} -> True
  _ -> False

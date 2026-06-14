module Arkham.Asset.Assets.OnTheirHeels5Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Calculation
import Arkham.Enemy.Types qualified as Enemy
import Arkham.Matcher
import TestImport.New

spec :: Spec
spec = describe "On Their Heels (5)" $ do
  -- Regression for #4813. Timing (proven by Track Shoes' "after you move, but
  -- before enemies engage you"): a plain "after you enter a location" reaction
  -- resolves AFTER enemy engagement, so Zoey's Cross ("after an enemy becomes
  -- engaged with you") resolves before On Their Heels. If Zoey's Cross defeats
  -- the entering enemy, On Their Heels must STILL be triggerable to discover a
  -- clue, because its triggering condition ("entered a location with 1+ enemies")
  -- was satisfied at entry.
  it "can still discover a clue when the entering enemy is defeated first (#4813)" . gameTest $ \self -> do
    withProp @"resources" 1 self
    enemy <- testEnemyWith (Enemy.healthL ?~ Fixed 1)
    location <- testLocation & prop @"clues" 1 & prop @"shroud" 0
    zoeysCross <- self `putAssetIntoPlay` Assets.zoeysCross
    onTheirHeels <- self `putAssetIntoPlay` Assets.onTheirHeels5
    spawnAt enemy location
    self `moveTo` location
    -- Engagement window: Zoey's Cross defeats the 1-health enemy.
    useReactionOf zoeysCross
    assert $ selectNone $ EnemyWithId (toId enemy)
    -- After-entering window: On Their Heels is still available even though the
    -- enemy is gone; the only resolvable effect is discovering the clue.
    useReactionOf onTheirHeels
    self.clues `shouldReturn` 1

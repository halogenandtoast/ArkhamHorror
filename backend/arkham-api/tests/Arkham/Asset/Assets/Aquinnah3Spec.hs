module Arkham.Asset.Assets.Aquinnah3Spec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Matcher (pattern AnyInPlayEnemy)
import TestImport.New

spec :: Spec
spec = describe "Aquinnah (3)" do
  -- "(You still take horror dealt by the attack.)" -- the redirect replaces how
  -- the attack's damage is assigned, so defeating the attacker with it does not
  -- unwind the attack that is already resolving.
  it "deals the attack's horror even when the redirect defeats the attacking enemy" . gameTest $ \self -> do
    enemy <- testEnemy & prop @"healthDamage" 2 & prop @"sanityDamage" 1 & prop @"health" 2

    self `putCardIntoPlay` Assets.aquinnah3
    location <- testLocation
    enemy `spawnAt` location
    self `moveTo` location

    run $ enemyAttack self enemy
    useReaction
    applyAllHorror

    assert $ selectNone AnyInPlayEnemy
    self.horror `shouldReturn` 1
    self.damage `shouldReturn` 0

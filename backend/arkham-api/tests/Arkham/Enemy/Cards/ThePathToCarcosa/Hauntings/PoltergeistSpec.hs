module Arkham.Enemy.Cards.ThePathToCarcosa.Hauntings.PoltergeistSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Constants (pattern AbilityAttack)
import Arkham.Enemy.CardDefs.ThePathToCarcosa.Hauntings qualified as Enemies
import TestImport.New

-- Poltergeist: "Cannot be damaged except by [[Spell]], [[Relic]], or encounter
-- cards." The engine models the printed "or encounter cards" clause by
-- whitelisting Matcher.EncounterCardSource inside
-- Arkham.Helpers.Enemy.sourceCanDamageEnemy.
spec :: Spec
spec = describe "Poltergeist" $ do
  -- Regression for issue #5342: a basic fight's damage source is
  -- UseAbilitySource <iid> (EnemySource <poltergeist>) AbilityAttack (see
  -- Arkham.Investigator.Runner.Damage). Its underlying source is the encounter
  -- enemy itself, so before the fix it satisfied the "or encounter cards"
  -- clause and a basic fight action damaged the Poltergeist.
  it "cannot be damaged by a basic fight action (#5342)" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    poltergeist <- testEnemyWithDef Enemies.poltergeist id
    poltergeist `spawnAt` location

    run $ InvestigatorDamageEnemy self.id poltergeist.id (toAbilitySource poltergeist AbilityAttack)
    poltergeist.damage `shouldReturn` 0

  it "can be damaged by a Spell card" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    poltergeist <- testEnemyWithDef Enemies.poltergeist id
    poltergeist `spawnAt` location

    shrivelling <- self `putAssetIntoPlay` Assets.shrivelling
    run $ InvestigatorDamageEnemy self.id poltergeist.id (toAbilitySource shrivelling 1)
    poltergeist.damage `shouldReturn` 1

  -- The "or encounter cards" clause must survive the #5342 fix. The
  -- Poltergeist's own Parley (ability index 1) is an encounter card source at a
  -- non-basic ability index, and is the card's printed damage route.
  it "can still be damaged by encounter card sources" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    poltergeist <- testEnemyWithDef Enemies.poltergeist id
    poltergeist `spawnAt` location

    run $ InvestigatorDamageEnemy self.id poltergeist.id (toAbilitySource poltergeist 1)
    poltergeist.damage `shouldReturn` 1

module Arkham.Enemy.Cards.TheFeastOfHemlockVale.TheLostSister.CavernMossSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.CardDefs.TheFeastOfHemlockVale.TheLostSister qualified as Enemies
import Arkham.Matcher
import Arkham.Placement
import TestImport.New

-- 'EnemyAt' used to reject every attached enemy, so a Cavern Moss that had
-- attached itself to an Item was invisible to every "enemy at your location"
-- card -- Fake Credentials, Beguile, Alessandra's elder sign (#5852).

spec :: Spec
spec = describe "Cavern Moss" do
  it "is at your location while attached to an Item you control" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    cigaretteCase <- self `putAssetIntoPlay` Assets.luckyCigaretteCase
    cavernMoss <- testEnemyWithDef Enemies.cavernMoss id
    run $ PlaceEnemy (toId cavernMoss) (AttachedToAsset cigaretteCase Nothing)
    assertAny $ enemyIs Enemies.cavernMoss <> enemyAt (toId location)

  it "is at its location while unattached" . gameTest $ \self -> do
    location <- testLocation
    self `moveTo` location
    cavernMoss <- testEnemyWithDef Enemies.cavernMoss id
    cavernMoss `spawnAt` location
    assertAny $ enemyIs Enemies.cavernMoss <> enemyAt (toId location)

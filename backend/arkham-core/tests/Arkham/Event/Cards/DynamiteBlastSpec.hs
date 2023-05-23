module Arkham.Event.Cards.DynamiteBlastSpec
  ( spec
  ) where

import TestImport hiding ( EnemyDamage, InvestigatorDamage )

import Arkham.Enemy.Types ( Field (..), healthL )
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types ( Field (..) )
import Arkham.Investigator.Cards qualified as Investigators

spec :: Spec
spec = describe "Dynamite Blast" $ do
  it "does 3 damage to each enemy and investigator at your location" $ gameTest $ \investigator -> do
    investigator2 <- addInvestigator Investigators.rolandBanks id
    enemy1 <- testEnemy (healthL .~ Static 4)
    enemy2 <- testEnemy (healthL .~ Static 4)
    location <- testLocation id
    pushAndRunAll
        [ enemySpawn location enemy1
        , enemySpawn location enemy2
        , moveTo investigator location
        , moveTo investigator2 location
        ]
    putCardIntoPlay investigator Events.dynamiteBlast
    chooseOnlyOption "choose your location"
    replicateM_ 3 $ chooseOnlyOption "assign Damage"
    replicateM_ 3 $ chooseOnlyOption "assign Damage"
    fieldAssert EnemyDamage (== 3) enemy1
    fieldAssert EnemyDamage (== 3) enemy2
    fieldAssert InvestigatorDamage (== 3) investigator
    fieldAssert InvestigatorDamage (== 3) investigator2

  it
    "does 3 damage to each enemy and investigator at a connected location" $ gameTest $ \investigator -> do
      investigator2 <- addInvestigator Investigators.rolandBanks id
      enemy1 <- testEnemy (healthL .~ Static 4)
      enemy2 <- testEnemy (healthL .~ Static 4)
      (location1, location2) <- testConnectedLocations id id
      pushAndRunAll
          [ enemySpawn location1 enemy1
          , enemySpawn location2 enemy2
          , moveTo investigator location1
          , moveTo investigator2 location2
          ]
      putCardIntoPlay investigator Events.dynamiteBlast
      chooseOptionMatching "choose connected location" $ \case
        TargetLabel (LocationTarget lid) _ -> lid == toId location2
        _ -> False
      replicateM_ 3 $ chooseOnlyOption "assign Damage"
      fieldAssert EnemyDamage (== 0) enemy1
      fieldAssert EnemyDamage (== 3) enemy2
      fieldAssert InvestigatorDamage (== 0) investigator
      fieldAssert InvestigatorDamage (== 3) investigator2

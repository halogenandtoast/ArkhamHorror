module Arkham.EnemyLocation.EnemyLocationSpec (spec) where

import Arkham.EnemyLocation.Cards qualified as EnemyLocations
import Arkham.Event.Cards qualified as Events
import Arkham.GameEnv (getHistoryField)
import Arkham.History
import Arkham.Token qualified as Token
import TestImport.New

-- Living Washroom: fight 3, health PerPlayer 3, evade 3.
livingWashroom :: CardDef
livingWashroom = EnemyLocations.livingWashroomHemlockHouse36

spec :: Spec
spec = describe "enemy-locations" $ do
  context "Evade action" $ do
    it "exhausts the enemy-location and records the evasion" . gameTest $ \self -> do
      withProp @"agility" 3 self
      lid <- placeEnemyLocation livingWashroom
      self `moveTo` lid
      setChaosTokens [Zero]
      sid <- getRandom
      run
        $ EvadeEnemy sid (toId self) (asEnemyLocationEnemy lid) (toSource self) Nothing SkillAgility False
      startSkillTest
      applyResults

      attrs <- enemyLocationAttrs lid
      attrs.exhausted `shouldBe` True
      getHistoryField TurnHistory (toId self) HistorySuccessfulEvasions `shouldReturn` 1

    -- #5789: a rider sets a target, so the evade resolves against
    -- ProxyTarget (EnemyTarget eid) (EventTarget ...). isEnemyTarget compared raw
    -- Targets, so the enemy-location never saw the success and nothing happened.
    it "resolves an evade rider against it (#5789)" . gameTest $ \self -> do
      withProp @"agility" 3 self
      withProp @"combat" 2 self
      lid <- placeEnemyLocation livingWashroom
      self `moveTo` lid
      setChaosTokens [Zero]
      self `putCardIntoPlay` Events.bumsRush
      chooseTarget (asEnemyLocationEnemy lid)
      startSkillTest
      applyResults

      attrs <- enemyLocationAttrs lid
      attrs.exhausted `shouldBe` True
      -- Bums' Rush deals 1 damage when you succeed by 2 or more. An enemy-location is
      -- Elite, so its "move the enemy" half never offers a choice.
      attrs.token Token.Damage `shouldBe` 1

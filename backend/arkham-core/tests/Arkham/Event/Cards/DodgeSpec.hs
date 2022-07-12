module Arkham.Event.Cards.DodgeSpec
  ( spec
  ) where

import TestImport

import Arkham.Attack
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = do
  describe "Dodge" $ do
    it "cancels the attack" $ do
      investigator <- testJenny
        $ \attrs -> attrs { investigatorResources = 1 }
      enemy <- testEnemy id
      location <- testLocation id
      dodge <- genPlayerCard Cards.dodge

      (didRunMessage, logger) <- createMessageMatcher
        (PerformEnemyAttack "00000" (toId enemy) DamageAny RegularAttack)

      gameTestWithLogger
          logger
          investigator
          [ addToHand investigator (PlayerCard dodge)
          , enemySpawn location enemy
          , moveTo investigator location
          , enemyAttack investigator enemy
          ]
          ((entitiesL . enemiesL %~ insertEntity enemy)
          . (entitiesL . locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOptionMatching
              "Play Dodge"
              (\case
                TargetLabel{} -> True
                _ -> False
              )
            didRunMessage `refShouldBe` False

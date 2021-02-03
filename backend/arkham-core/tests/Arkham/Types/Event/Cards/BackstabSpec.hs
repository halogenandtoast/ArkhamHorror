module Arkham.Types.Event.Cards.BackstabSpec
  ( spec
  ) where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorCombat = 1, investigatorAgility = 4 }
      backstab <- buildEvent "01051" investigator
      enemy <- testEnemy
        ((EnemyAttrs.fightL .~ 3) . (EnemyAttrs.healthL .~ Static 4))
      game <-
        runGameTest
          investigator
          [ SetTokens [MinusOne]
          , enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator backstab
          ]
          ((eventsL %~ insertEntity backstab)
          . (locationsL %~ insertEntity location)
          . (enemiesL %~ insertEntity enemy)
          )
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      updated game enemy `shouldSatisfy` hasDamage (3, 0)
      backstab `shouldSatisfy` isInDiscardOf game investigator

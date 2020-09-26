module Arkham.Types.Event.Cards.BackstabSpec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.Helpers
import Arkham.Types.Investigator.Attrs (Attrs(..))
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      scenario' <- testScenario "00000" id
      location <- testLocation "00000" id
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorCombat = 1, investigatorAgility = 4 }
      backstab <- buildEvent "01051" investigator
      enemy <- testEnemy
        (set EnemyAttrs.fight 3 . set EnemyAttrs.health (Static 4))
      game <-
        runGameTest
          investigator
          [ enemySpawn location enemy
          , moveTo investigator location
          , playEvent investigator backstab
          ]
          ((events %~ insertEntity backstab)
          . (locations %~ insertEntity location)
          . (enemies %~ insertEntity enemy)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      enemy `shouldSatisfy` hasDamage game (3, 0)
      backstab `shouldSatisfy` isInDiscardOf game investigator

module Arkham.Types.Event.Cards.BackstabSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Difficulty
import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ do
      theGathering <- newScenario Easy "01104"
      (locationId, study) <- newLocation "01111"
      (investigatorId, investigator) <- testInvestigator "00000"
        $ \stats -> stats { combat = 1, agility = 4 }
      (eventId, backstab) <- buildEvent "01051" investigatorId
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.fight 3 . set EnemyAttrs.health (Static 4))
      game <-
        runGameTest
          investigator
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId eventId
          ]
          ((events %~ insertMap eventId backstab)
          . (locations %~ insertMap locationId study)
          . (enemies %~ insertMap enemyId enemy)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Fight enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      -- We expect the skill check to succeed and the enemy to be defeated
      enemy `shouldSatisfy` hasDamage game (3, 0)
      backstab `shouldSatisfy` isInDiscardOf game investigator

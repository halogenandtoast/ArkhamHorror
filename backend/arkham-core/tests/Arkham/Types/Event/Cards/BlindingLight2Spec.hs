module Arkham.Types.Event.Cards.BlindingLight2Spec
  ( spec
  )
where

import TestImport

import qualified Arkham.Types.Enemy.Attrs as EnemyAttrs
import Arkham.Types.GameValue
import Arkham.Types.Helpers
import Arkham.Types.Token

spec :: Spec
spec = do
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ do
      scenario' <- testScenario "00000" id
      (investigatorId, investigator) <- testInvestigator "00000"
        $ \stats -> stats { willpower = 5, agility = 3 }
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      (blindingLight2Id, blindingLight2) <- buildEvent "01069" investigatorId
      (locationId, location) <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId blindingLight2Id
          ]
          ((events %~ insertMap blindingLight2Id blindingLight2)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap locationId location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 2 damage to the evaded enemy" $ do
      scenario' <- testScenario "00000" id
      (investigatorId, investigator) <- testInvestigator "00000" id
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      (blindingLight2Id, blindingLight2) <- buildEvent "01069" investigatorId
      (locationId, location) <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId blindingLight2Id
          ]
          ((events %~ insertMap blindingLight2Id blindingLight2)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap locationId location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` hasDamage game (2, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action and takes 1 horror"
      $ for_ [Skull]
      $ \token -> do
          scenario' <- testScenario "00000" id
          (investigatorId, investigator) <- testInvestigator "00000" id
          (enemyId, enemy) <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
          (blindingLight2Id, blindingLight2) <- buildEvent
            "01069"
            investigatorId
          (locationId, location) <- testLocation "00000" id
          game <-
            runGameTest
              investigator
              [ EnemySpawn locationId enemyId
              , MoveTo investigatorId locationId
              , InvestigatorPlayEvent investigatorId blindingLight2Id
              ]
              ((events %~ insertMap blindingLight2Id blindingLight2)
              . (enemies %~ insertMap enemyId enemy)
              . (locations %~ insertMap locationId location)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ scenario')
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
            >>= runGameTestOnlyOption "take event damage"
            >>= (\game -> if token == Tablet
                  then runGameTestOnlyOption "take damage" game
                  else pure game
                )
          blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2
          investigator `shouldSatisfy` hasDamage
            game
            (if token == Tablet then 1 else 0, 1)

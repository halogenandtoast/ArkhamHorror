module Arkham.Types.Event.Cards.BlindingLight2Spec
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
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ do
      theGathering <- newScenario Easy "01104"
      (investigatorId, investigator) <- testInvestigator "00000"
        $ \stats -> stats { willpower = 5, agility = 3 }
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      (blindingLight2Id, blindingLight2) <- buildEvent "01069" investigatorId
      (hallwayId, hallway) <- newLocation "01112"
      game <-
        runGameTest
          investigator
          [ EnemySpawn hallwayId enemyId
          , MoveTo investigatorId hallwayId
          , InvestigatorPlayEvent investigatorId blindingLight2Id
          ]
          ((events %~ insertMap blindingLight2Id blindingLight2)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap hallwayId hallway)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight2 `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 2 damage to the evaded enemy" $ do
      theGathering <- newScenario Easy "01104"
      (investigatorId, investigator) <- testInvestigator "00000" id
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
      (blindingLight2Id, blindingLight2) <- buildEvent "01069" investigatorId
      (hallwayId, hallway) <- newLocation "01112"
      game <-
        runGameTest
          investigator
          [ EnemySpawn hallwayId enemyId
          , MoveTo investigatorId hallwayId
          , InvestigatorPlayEvent investigatorId blindingLight2Id
          ]
          ((events %~ insertMap blindingLight2Id blindingLight2)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap hallwayId hallway)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
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
          theDevourerBelow <- newScenario Easy "01142"
          (investigatorId, investigator) <- testInvestigator "00000" id
          (enemyId, enemy) <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 3))
          (blindingLight2Id, blindingLight2) <- buildEvent
            "01069"
            investigatorId
          (hallwayId, hallway) <- newLocation "01112"
          game <-
            runGameTest
              investigator
              [ EnemySpawn hallwayId enemyId
              , MoveTo investigatorId hallwayId
              , InvestigatorPlayEvent investigatorId blindingLight2Id
              ]
              ((events %~ insertMap blindingLight2Id blindingLight2)
              . (enemies %~ insertMap enemyId enemy)
              . (locations %~ insertMap hallwayId hallway)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ theDevourerBelow)
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

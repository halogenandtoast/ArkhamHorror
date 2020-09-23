module Arkham.Types.Event.Cards.BlindingLightSpec
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
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      theGathering <- newScenario Easy "01104"
      (investigatorId, investigator) <- testInvestigator "00000"
        $ \stats -> stats { willpower = 5, agility = 3 }
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
      (hallwayId, hallway) <- newLocation "01112"
      game <-
        runGameTest
          investigator
          [ EnemyMove enemyId "00000" hallwayId
          , MoveTo investigatorId hallwayId
          , InvestigatorPlayEvent investigatorId blindingLightId
          ]
          ((events %~ insertMap blindingLightId blindingLight)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap hallwayId hallway)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 1 damage to the evaded enemy" $ do
      theGathering <- newScenario Easy "01104"
      (investigatorId, investigator) <- testInvestigator "01004" id
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
      (hallwayId, hallway) <- newLocation "01112"
      game <-
        runGameTest
          investigator
          [ EnemySpawn hallwayId enemyId
          , MoveTo investigatorId hallwayId
          , InvestigatorPlayEvent investigatorId blindingLightId
          ]
          ((events %~ insertMap blindingLightId blindingLight)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap hallwayId hallway)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ theGathering)
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` hasDamage game (1, 0)

    it
        "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> do
          theDevourerBelow <- newScenario Easy "01142"
          (investigatorId, investigator) <- testInvestigator "01004" id
          (enemyId, enemy) <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
          (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
          (hallwayId, hallway) <- newLocation "01112"
          game <-
            runGameTest
              investigator
              [ EnemySpawn hallwayId enemyId
              , MoveTo investigatorId hallwayId
              , InvestigatorPlayEvent investigatorId blindingLightId
              ]
              ((events %~ insertMap blindingLightId blindingLight)
              . (enemies %~ insertMap enemyId enemy)
              . (locations %~ insertMap hallwayId hallway)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ theDevourerBelow)
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
          blindingLight `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2

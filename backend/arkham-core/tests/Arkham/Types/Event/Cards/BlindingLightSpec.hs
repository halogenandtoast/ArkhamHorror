module Arkham.Types.Event.Cards.BlindingLightSpec
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
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ do
      scenario' <- testScenario "00000" id
      (investigatorId, investigator) <- testInvestigator "00000"
        $ \stats -> stats { willpower = 5, agility = 3 }
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
      (locationId, location) <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ EnemyMove enemyId "00000" locationId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId blindingLightId
          ]
          ((events %~ insertMap blindingLightId blindingLight)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap locationId location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
          )
        >>= runGameTestOnlyOption "Evade enemy"
        >>= runGameTestOnlyOption "Run skill check"
        >>= runGameTestOnlyOption "Apply results"
      blindingLight `shouldSatisfy` isInDiscardOf game investigator
      enemy `shouldSatisfy` evadedBy game investigator

    it "deals 1 damage to the evaded enemy" $ do
      scenario' <- testScenario "00000" id
      (investigatorId, investigator) <- testInvestigator "01004" id
      (enemyId, enemy) <- testEnemy
        (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
      (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
      (locationId, location) <- testLocation "00000" id
      game <-
        runGameTest
          investigator
          [ EnemySpawn locationId enemyId
          , MoveTo investigatorId locationId
          , InvestigatorPlayEvent investigatorId blindingLightId
          ]
          ((events %~ insertMap blindingLightId blindingLight)
          . (enemies %~ insertMap enemyId enemy)
          . (locations %~ insertMap locationId location)
          . (chaosBag .~ Bag [MinusOne])
          . (scenario ?~ scenario')
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
          scenario' <- testScenario "00000" id
          (investigatorId, investigator) <- testInvestigator "01004" id
          (enemyId, enemy) <- testEnemy
            (set EnemyAttrs.evade 4 . set EnemyAttrs.health (Static 2))
          (blindingLightId, blindingLight) <- buildEvent "01066" investigatorId
          (locationId, location) <- testLocation "00000" id
          game <-
            runGameTest
              investigator
              [ EnemySpawn locationId enemyId
              , MoveTo investigatorId locationId
              , InvestigatorPlayEvent investigatorId blindingLightId
              ]
              ((events %~ insertMap blindingLightId blindingLight)
              . (enemies %~ insertMap enemyId enemy)
              . (locations %~ insertMap locationId location)
              . (chaosBag .~ Bag [token])
              . (scenario ?~ scenario')
              )
            >>= runGameTestOnlyOption "Evade enemy"
            >>= runGameTestOnlyOption "Run skill check"
            >>= runGameTestOnlyOption "Apply results"
          blindingLight `shouldSatisfy` isInDiscardOf game investigator
          investigator `shouldSatisfy` hasRemainingActions game 2

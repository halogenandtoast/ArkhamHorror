module Arkham.Event.Cards.BlindingLight2Spec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..), willpowerL)
import Arkham.Projection

spec :: Spec
spec = do
  describe "Blinding Light 2" $ do
    it "Uses willpower to evade an enemy" $ gameTest $ \investigator -> do
      updateInvestigator investigator $ \attrs ->
        attrs {investigatorWillpower = 5, investigatorAgility = 3}
      enemy <-
        testEnemy
          ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 3))
      location <- testLocation id
      pushAndRunAll
        [ SetChaosTokens [MinusOne]
        , enemySpawn location enemy
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.blindingLight2
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"
      assert $ isInDiscardOf investigator Events.blindingLight2
      assert $ fieldP EnemyEngagedInvestigators null (toId enemy)

    it "deals 2 damage to the evaded enemy" $ gameTest $ \investigator -> do
      updateInvestigator investigator (willpowerL .~ 5)
      enemy <-
        testEnemy
          ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 3))
      location <- testLocation id
      pushAndRunAll
        [ SetChaosTokens [MinusOne]
        , enemySpawn location enemy
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.blindingLight2
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"
      assert $ isInDiscardOf investigator Events.blindingLight2
      assert $ fieldP EnemyDamage (== 2) (toId enemy)

    it
      "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action and takes 1 horror"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> gameTest $ \investigator -> do
        updateInvestigator investigator (willpowerL .~ 5)
        enemy <-
          testEnemy
            ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 3))
        location <- testLocation id
        pushAndRunAll
          [ SetChaosTokens [token]
          , enemySpawn location enemy
          , moveTo investigator location
          ]
        putCardIntoPlay investigator Events.blindingLight2
        chooseOnlyOption "Evade enemy"
        chooseOnlyOption "Run skill check"
        chooseOnlyOption "Apply results"
        chooseOnlyOption "take event damage"
        assert $ isInDiscardOf investigator Events.blindingLight2
        fieldAssert InvestigatorRemainingActions (== 2) investigator
        fieldAssert InvestigatorHorror (== 1) investigator

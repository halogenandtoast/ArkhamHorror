module Arkham.Event.Cards.BlindingLightSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (InvestigatorAttrs (..), willpowerL)

spec :: Spec
spec = do
  describe "Blinding Light" $ do
    it "Uses willpower to evade an enemy" $ gameTest $ \investigator -> do
      updateInvestigator investigator $ \attrs ->
        attrs {investigatorWillpower = 5, investigatorAgility = 3}
      enemy <- testEnemyWith ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 2))
      location <- testLocationWith id
      pushAndRunAll
        [ SetChaosTokens [MinusOne]
        , spawnAt enemy location
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.blindingLight
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"

      assert $ Events.blindingLight `isInDiscardOf` investigator
      assert $ evadedBy investigator enemy

    it "deals 1 damage to the evaded enemy" $ gameTest $ \investigator -> do
      updateInvestigator investigator (willpowerL .~ 5)
      enemy <- testEnemyWith ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 2))
      location <- testLocationWith id
      pushAndRunAll
        [ SetChaosTokens [MinusOne]
        , spawnAt enemy location
        , moveTo investigator location
        ]
      putCardIntoPlay investigator Events.blindingLight
      chooseOnlyOption "Evade enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"

      assert $ Events.blindingLight `isInDiscardOf` investigator
      fieldAssert EnemyDamage (== 1) enemy

    it
      "On Skull, Cultist, Tablet, ElderThing, or AutoFail the investigator loses an action"
      $ for_ [Skull, Cultist, Tablet, ElderThing, AutoFail]
      $ \token -> gameTest $ \investigator -> do
        updateInvestigator investigator (willpowerL .~ 5)
        enemy <- testEnemyWith ((EnemyAttrs.evadeL ?~ 4) . (EnemyAttrs.healthL .~ Static 2))
        location <- testLocationWith id
        pushAndRunAll
          [ SetChaosTokens [token]
          , spawnAt enemy location
          , moveTo investigator location
          ]
        putCardIntoPlay investigator Events.blindingLight
        chooseOnlyOption "Evade enemy"
        chooseOnlyOption "Run skill check"
        chooseOnlyOption "Apply results"

        assert $ Events.blindingLight `isInDiscardOf` investigator
        getRemainingActions investigator `shouldReturn` 2

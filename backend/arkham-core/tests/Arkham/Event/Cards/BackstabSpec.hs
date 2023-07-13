module Arkham.Event.Cards.BackstabSpec (
  spec,
) where

import TestImport.Lifted hiding (EnemyDamage)

import Arkham.Enemy.Types (Field (..))
import Arkham.Enemy.Types qualified as EnemyAttrs
import Arkham.Event.Cards qualified as Events
import Arkham.Investigator.Types (Field (..), InvestigatorAttrs (..))
import Arkham.Matcher (cardIs)
import Arkham.Projection

spec :: Spec
spec = do
  describe "Backstab" $ do
    it "should use agility and do +2 damage" $ gameTest $ \investigator -> do
      location <- testLocation id
      updateInvestigator investigator $
        \attrs -> attrs {investigatorCombat = 1, investigatorAgility = 4}
      enemy <-
        testEnemy
          ((EnemyAttrs.fightL .~ 3) . (EnemyAttrs.healthL .~ Static 4))
      pushAndRun $ SetChaosTokens [MinusOne]
      pushAndRun $ enemySpawn location enemy
      pushAndRun $ moveTo investigator location
      putCardIntoPlay investigator Events.backstab
      chooseOnlyOption "Fight enemy"
      chooseOnlyOption "Run skill check"
      chooseOnlyOption "Apply results"
      assert $ fieldP EnemyDamage (== 3) (toId enemy)
      assert $ fieldP InvestigatorDiscard (any (`cardMatch` cardIs Events.backstab)) (toId investigator)

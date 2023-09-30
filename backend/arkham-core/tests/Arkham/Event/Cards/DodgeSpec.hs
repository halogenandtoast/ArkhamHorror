module Arkham.Event.Cards.DodgeSpec (
  spec,
) where

import TestImport

import Arkham.Attack qualified as Attack
import Arkham.Event.Cards qualified as Cards
import Arkham.Investigator.Types (InvestigatorAttrs (..))
import Arkham.Token

spec :: Spec
spec = do
  describe "Dodge" $ do
    it "cancels the attack" $ gameTest $ \investigator -> do
      updateInvestigator investigator $ \attrs -> attrs {investigatorTokens = setTokens Resource 1 mempty}
      enemy <- testEnemyWith id
      location <- testLocationWith id
      dodge <- genCard Cards.dodge

      didRunMessage <-
        createMessageMatcher $
          PerformEnemyAttack $
            Attack.enemyAttack
              (toId enemy)
              enemy
              investigator

      pushAndRunAll
        [ addToHand (toId investigator) dodge
        , spawnAt enemy location
        , moveTo investigator location
        , enemyAttack investigator enemy
        ]

      chooseOptionMatching
        "Play Dodge"
        ( \case
            TargetLabel {} -> True
            _ -> False
        )
      didRunMessage `refShouldBe` False

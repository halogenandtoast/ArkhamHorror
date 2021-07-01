module Arkham.Types.Event.Cards.DodgeSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (InvestigatorAttrs(..))

spec :: Spec
spec = do
  describe "Dodge" $ do
    it "cancels the attack" $ do
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorResources = 1 }
      enemy <- testEnemy id
      location <- testLocation id
      dodge <- buildPlayerCard "01023"

      (didRunMessage, logger) <- createMessageMatcher
        (PerformEnemyAttack "00000" (toId enemy))

      gameTestWithLogger
          logger
          investigator
          [ addToHand investigator (PlayerCard dodge)
          , enemyAttack investigator enemy
          ]
          ((enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            runMessages
            chooseOptionMatching
              "Play Dodge"
              (\case
                Run{} -> True
                _ -> False
              )
            didRunMessage `refShouldBe` False

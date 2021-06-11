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
      location <- testLocation "00000" id
      dodge <- buildPlayerCard "01023"
      runGameTest
          investigator
          [ addToHand investigator (PlayerCard dodge)
          , enemyAttack investigator enemy
          ]
          ((enemiesL %~ insertEntity enemy)
          . (locationsL %~ insertEntity location)
          )
        $ do
            (didRunMessage, logger) <- createMessageMatcher
              (PerformEnemyAttack "00000" (toId enemy))
            runMessagesNoLogging
            runGameTestOptionMatchingWithLogger
              "Play Dodge"
              logger
              (\case
                Run{} -> True
                _ -> False
              )
            didRunMessage `refShouldBe` False

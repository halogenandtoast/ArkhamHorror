module Arkham.Types.Event.Cards.DodgeSpec
  ( spec
  ) where

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
      (didRunMessage, logger) <- createMessageMatcher
        (PerformEnemyAttack "00000" (toId enemy))
      void
        $ runGameTest
            investigator
            [ addToHand investigator (PlayerCard dodge)
            , enemyAttack investigator enemy
            ]
            ((enemiesL %~ insertEntity enemy)
            . (locationsL %~ insertEntity location)
            )
        >>= runGameTestOptionMatchingWithLogger
              "Play Dodge"
              logger
              (\case
                Run{} -> True
                _ -> False
              )
      readIORef didRunMessage `shouldReturn` False

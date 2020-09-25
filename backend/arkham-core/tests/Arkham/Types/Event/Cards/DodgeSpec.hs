module Arkham.Types.Event.Cards.DodgeSpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs (Attrs(..))

spec :: Spec
spec = do
  describe "Dodge" $ do
    it "cancels the attack" $ do
      investigator <- testInvestigator "00000"
        $ \attrs -> attrs { investigatorResources = 1 }
      enemy <- testEnemy "00000" id
      location <- testLocation "00000" id
      dodge <- buildPlayerCard "01023"
      game <-
        runGameTest
            investigator
            [ addToHand investigator (PlayerCard dodge)
            , enemyAttack investigator enemy
            ]
            ((enemies %~ insertEntity enemy)
            . (locations %~ insertEntity location)
            )
          >>= runGameTestOptionMatching
                "Play Dodge"
                (\case
                  Run{} -> True
                  _ -> False
                )
      game `shouldSatisfy` not . hasProcessedMessage
        (PerformEnemyAttack "00000" (getId () enemy))

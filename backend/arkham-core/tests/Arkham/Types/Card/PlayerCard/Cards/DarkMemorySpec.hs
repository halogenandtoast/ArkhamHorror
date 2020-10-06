module Arkham.Types.Card.PlayerCard.Cards.DarkMemorySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Investigator.Attrs

spec :: Spec
spec = describe "Dark Memory" $ do
  it "deals 2 horror if in your hand at the end of your turn" $ do
    darkMemory <- buildPlayerCard "01013"
    investigator <- testInvestigator "00000"
      $ \attrs -> attrs { investigatorHand = [PlayerCard darkMemory] }
    game <-
      runGameTest investigator [ChooseEndTurn $ getId () investigator] id
      >>= runGameTestOnlyOption "apply forced effect"
      >>= runGameTestOnlyOption "apply horror"
      >>= runGameTestOnlyOption "apply horror"
    updated game investigator `shouldSatisfy` hasDamage (0, 2)


module Arkham.Types.Treachery.Cards.FinalRhapsodySpec
  ( spec
  ) where

import TestImport.Lifted

import Arkham.Treachery.Cards qualified as Cards

spec :: Spec
spec = describe "Final Rhapsody" $ do
  it "does 1 damage per skull and autofail revealed" $ do
    investigator <- testInvestigator "00000" id
    finalRhapsody <- genPlayerCard Cards.finalRhapsody
    gameTest
        investigator
        [ SetTokens [Skull, Skull, AutoFail, Zero, Cultist]
        , loadDeck investigator [finalRhapsody]
        , drawCards investigator 1
        ]
        id
      $ do
          runMessages
          chooseFirstOption "acknowledge damage"
          chooseFirstOption "take damage"
          chooseFirstOption "take damage"
          chooseFirstOption "take damage"
          chooseFirstOption "take horror"
          chooseFirstOption "take horror"
          chooseFirstOption "take horror"
          updated investigator `shouldSatisfyM` hasDamage (3, 3)
          isInDiscardOf investigator finalRhapsody `shouldReturn` True

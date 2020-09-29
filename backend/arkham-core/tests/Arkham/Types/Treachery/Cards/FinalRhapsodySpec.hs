module Arkham.Types.Treachery.Cards.FinalRhapsodySpec
  ( spec
  )
where

import TestImport

import Arkham.Types.Token

spec :: Spec
spec = describe "Final Rhapsody" $ do
  it "does 1 damage per skull and autofail revealed" $ do
    investigator <- testInvestigator "00000" id
    finalRhapsody <- buildPlayerCard "02013"
    game <-
      runGameTest
        investigator
        [ SetTokens [Skull, Skull, AutoFail, Zero, Cultist]
        , loadDeck investigator [finalRhapsody]
        , drawCards investigator 1
        ]
        id
      >>= runGameTestFirstOption "take damage"
      >>= runGameTestFirstOption "take damage"
      >>= runGameTestFirstOption "take damage"
      >>= runGameTestFirstOption "take horror"
      >>= runGameTestFirstOption "take horror"
      >>= runGameTestFirstOption "take horror"
    updated game investigator `shouldSatisfy` hasDamage (3, 3)
    finalRhapsody `shouldSatisfy` isInDiscardOf game investigator

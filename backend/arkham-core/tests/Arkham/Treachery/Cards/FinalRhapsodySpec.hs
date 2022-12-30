module Arkham.Treachery.Cards.FinalRhapsodySpec
  ( spec
  ) where

import TestImport.Lifted hiding (InvestigatorDamage)

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Investigator.Types (Field(..))

spec :: Spec
spec = describe "Final Rhapsody" $ do
  it "does 1 damage per skull and autofail revealed" $ do
    investigator <- testJenny id
    finalRhapsody <- genPlayerCard Cards.finalRhapsody
    drawing <- drawCards (toId investigator) investigator 1
    gameTest
        investigator
        [ SetTokens [Skull, Skull, AutoFail, Zero, Cultist]
        , loadDeck investigator [finalRhapsody]
        , drawing
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
          fieldAssert InvestigatorDamage (== 3) investigator
          fieldAssert InvestigatorHorror (== 3) investigator
          fieldAssert InvestigatorDiscard (== [finalRhapsody]) investigator

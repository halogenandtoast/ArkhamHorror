module Arkham.Treachery.Cards.FinalRhapsodySpec (spec) where

import Arkham.Treachery.Cards qualified as Cards
import TestImport.New hiding (InvestigatorDamage)

spec :: Spec
spec = describe "Final Rhapsody" $ do
  it "does 1 damage per skull and autofail revealed" $ gameTest $ \self -> do
    setChaosTokens [Skull, Skull, AutoFail, Zero, Cultist]
    loadDeck self [Cards.finalRhapsody]
    self `drawCards` 1
    chooseFirstOption "acknowledge damage"
    applyAllDamage
    applyAllHorror
    self.damage `shouldReturn` 3
    self.horror `shouldReturn` 3
    assert $ Cards.finalRhapsody `isInDiscardOf` self

module Arkham.UltimatumsAndBoons.UltimatumOfTheBrokenVeilSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Treachery.Cards qualified as Treacheries
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of the Broken Veil" $ do
  it "weaknesses discarded from the top of the deck are shuffled back in" . gameTest $ \self -> do
    withUltimatums [UltimatumOfTheBrokenVeil]
    withDeck self [Treacheries.paranoia, Assets.leatherCoat, Assets.leatherCoat]
    run $ DiscardTopOfDeck (toId self) 2 (TestSource mempty) (Just TestTarget)
    deckDefs <- asDefs self.deck
    deckDefs `shouldMatchList` [Treacheries.paranoia, Assets.leatherCoat]
    hasDiscardPile self [Assets.leatherCoat]

  -- SUSPECTED BUG: the hook listens for DiscardedTopOfDeck, which the
  -- investigator runner only emits when the mill has a target. The standard
  -- lifted discardTopOfDeck helper (and e.g. Short Supply) passes Nothing, so
  -- those mills bypass the ultimatum entirely. Expected red until fixed.
  it "also applies to mills that don't report their results" . gameTest $ \self -> do
    withUltimatums [UltimatumOfTheBrokenVeil]
    withDeck self [Treacheries.paranoia, Assets.leatherCoat, Assets.leatherCoat]
    run $ DiscardTopOfDeck (toId self) 2 (TestSource mempty) Nothing
    deckDefs <- asDefs self.deck
    deckDefs `shouldMatchList` [Treacheries.paranoia, Assets.leatherCoat]
    hasDiscardPile self [Assets.leatherCoat]

  it "has no effect while ultimatums and boons are disabled" . gameTest $ \self -> do
    withUltimatumsDisabled [UltimatumOfTheBrokenVeil]
    withDeck self [Treacheries.paranoia, Assets.leatherCoat, Assets.leatherCoat]
    run $ DiscardTopOfDeck (toId self) 2 (TestSource mempty) (Just TestTarget)
    deckDefs <- asDefs self.deck
    deckDefs `shouldMatchList` [Assets.leatherCoat]
    hasDiscardPile self [Assets.leatherCoat, Treacheries.paranoia]

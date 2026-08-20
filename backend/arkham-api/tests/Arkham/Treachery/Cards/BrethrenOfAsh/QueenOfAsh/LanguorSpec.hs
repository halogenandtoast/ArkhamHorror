module Arkham.Treachery.Cards.BrethrenOfAsh.QueenOfAsh.LanguorSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Matcher
import Arkham.Placement (Placement (InThreatArea))
import Arkham.Treachery.CardDefs.BrethrenOfAsh.QueenOfAsh qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Languor" $ do
  it "draws a discarded weakness asset instead of restricting assets" . gameTest $ \self -> do
    languor <- genCard Treacheries.languor
    (_, createLanguor) <- Helpers.createTreacheryAt languor (InThreatArea self.id)
    run createLanguor
    self `loadDeck` [Assets.theNecronomiconDrakeTranslation]
    duringTurn self useForcedAbility
    -- Drawing the weakness resolves its revelation, so The Necronomicon lands in
    -- the threat area rather than the hand. What #5419 needs is that it leaves
    -- the discard pile at all, and that Languor's "otherwise" branch does not
    -- also fire. The Necronomicon grants its own CannotPlay #asset, so
    -- CannotCommitCards #asset is what distinguishes Languor's restriction.
    assertAny $ assetIs Assets.theNecronomiconDrakeTranslation <> assetControlledBy self.id
    asDefs self.discard `shouldReturn` []
    getModifiers self `shouldNotContainM` [CannotCommitCards #asset]

  it "restricts the discarded card's type when it is not a weakness" . gameTest $ \self -> do
    languor <- genCard Treacheries.languor
    (_, createLanguor) <- Helpers.createTreacheryAt languor (InThreatArea self.id)
    run createLanguor
    self `loadDeck` [Assets.leoDeLuca]
    duringTurn self useForcedAbility
    asDefs self.hand `shouldReturn` []
    asDefs self.discard `shouldReturn` [Assets.leoDeLuca]
    getModifiers self `shouldContainM` [CannotPlay #asset, CannotCommitCards #asset]

module Arkham.Treachery.Cards.LanguorSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Helpers.Message qualified as Helpers
import Arkham.Placement (Placement (InThreatArea))
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport.New

spec :: Spec
spec = describe "Languor" $ do
  it "draws a discarded weakness asset instead of restricting assets" . gameTest $ \self -> do
    languor <- genCard Treacheries.languor
    (_, createLanguor) <- Helpers.createTreacheryAt languor (InThreatArea self.id)
    run createLanguor
    self `loadDeck` [Assets.theNecronomiconDrakeTranslation]
    duringTurn self $ pure ()
    asDefs self.hand `shouldReturn` [Assets.theNecronomiconDrakeTranslation]
    asDefs self.discard `shouldReturn` []
    getModifiers self `shouldNotContainM` [CannotPlay #asset]

  it "restricts the discarded card's type when it is not a weakness" . gameTest $ \self -> do
    languor <- genCard Treacheries.languor
    (_, createLanguor) <- Helpers.createTreacheryAt languor (InThreatArea self.id)
    run createLanguor
    self `loadDeck` [Assets.leoDeLuca]
    duringTurn self $ pure ()
    asDefs self.hand `shouldReturn` []
    asDefs self.discard `shouldReturn` [Assets.leoDeLuca]
    getModifiers self `shouldContainM` [CannotPlay #asset, CannotCommitCards #asset]

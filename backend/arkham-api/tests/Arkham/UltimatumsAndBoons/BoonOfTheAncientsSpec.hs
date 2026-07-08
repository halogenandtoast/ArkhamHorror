module Arkham.UltimatumsAndBoons.BoonOfTheAncientsSpec (spec) where

import Arkham.Projection (field)
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Boon of the Ancients" $ do
  context "in a campaign" $ do
    it "each investigator begins the campaign with 5 additional experience" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfTheAncients]
      asCampaign
      deck <- Deck <$> testPlayerCards 5
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing deck
      field InvestigatorXp (toId self) `shouldReturn` 5

  context "standalone" $ do
    it "grants no experience" . gameTest $ \self -> do
      withUltimatumsAndBoons [BoonOfTheAncients]
      deck <- Deck <$> testPlayerCards 5
      run $ InitDeck $ InitDeckAttrs (toId self) Nothing Nothing deck
      field InvestigatorXp (toId self) `shouldReturn` 0

module Arkham.UltimatumsAndBoons.UltimatumOfTheScreamSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Classes.HasGame (getGame)
import Arkham.Game.Settings (settingsScreamedAllies)
import Arkham.Matcher qualified as Matcher
import Helpers.UltimatumsAndBoons
import TestImport.New

spec :: Spec
spec = describe "Ultimatum of the Scream" $ do
  context "in a campaign" $ do
    it "a defeated unique ally is removed from the game and recorded" . gameTest $ \self -> do
      withUltimatums [UltimatumOfTheScream]
      asCampaign
      leo <- self `putAssetIntoPlay` Assets.leoDeLuca
      run $ DealAssetDamage leo (TestSource mempty) 2 0
      -- removed from the game, not discarded
      assertNone $ Matcher.assetIs Assets.leoDeLuca
      self.discard `shouldReturn` []
      (settingsScreamedAllies . gameSettings <$> getGame)
        `shouldReturn` singletonSet "01048"
      -- the screamed ally can no longer be played (tick so preloaded
      -- modifiers pick up the updated settings)
      tick
      getModifiers self
        `shouldContainM` [CannotPlay (Matcher.CardWithCardCode "01048")]

    -- Cleanup lives on the campaign's NextCampaignStep handler: EndOfScenario
    -- clears the queue, and campaign transitions can run without a scenario
    -- to dispatch through.
    it "removes screamed allies from every deck at the end of the scenario" . gameTest $ \self -> do
      withUltimatums [UltimatumOfTheScream]
      asCampaign
      leo <- self `putAssetIntoPlay` Assets.leoDeLuca
      run $ DealAssetDamage leo (TestSource mempty) 2 0
      removedFromDeck <- createMessageChecker \case
        RemoveCampaignCardFromDeck _ def -> def == Assets.leoDeLuca
        _ -> False
      run $ EndOfScenario Nothing
      removedFromDeck `refShouldBe` True

  context "standalone" $ do
    it "has no effect" . gameTest $ \self -> do
      withUltimatums [UltimatumOfTheScream]
      leo <- self `putAssetIntoPlay` Assets.leoDeLuca
      run $ DealAssetDamage leo (TestSource mempty) 2 0
      hasDiscardPile self [Assets.leoDeLuca]
      (settingsScreamedAllies . gameSettings <$> getGame) `shouldReturn` mempty

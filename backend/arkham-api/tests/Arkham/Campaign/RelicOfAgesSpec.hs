module Arkham.Campaign.RelicOfAgesSpec (spec) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Campaigns.TheForgottenAge (relicOwnedBy, theForgottenAge)
import Arkham.Campaign.Types (storyCardsL)
import Arkham.Difficulty
import Arkham.Treachery.Cards qualified as Treacheries
import TestImport

-- The Forgotten Age re-homes the Relic of Ages when its owner leaves the campaign
-- (Threads of Fate, "The investigators found the missing relic"). The lookup matches on
-- title so it holds for whichever printing the campaign has reached; if a future printing
-- ever changes the base name, or another story card picks up the title, these break.
spec :: Spec
spec = describe "relicOwnedBy" do
  let attrsWith cards =
        toAttrs (theForgottenAge Easy) & storyCardsL .~ singletonMap "04004" cards

  for_ relicPrintings \def ->
    it ("finds " <> show (toCardCode def)) $ gameTest \_ -> do
      card <- genCard def
      liftIO $ (toCardCode <$> relicOwnedBy "04004" (attrsWith [card])) `shouldBe` Just (toCardCode def)

  it "ignores the other cards the same investigator earned" $ gameTest \_ -> do
    cards <- traverse genCard [Assets.ichtacaTheForgottenGuardian, Treacheries.doomed, Treacheries.poisoned]
    liftIO $ relicOwnedBy "04004" (attrsWith cards) `shouldBe` Nothing

  it "picks the relic out of a mixed pile" $ gameTest \_ -> do
    cards <-
      traverse
        genCard
        [ Assets.ichtacaTheForgottenGuardian
        , Assets.relicOfAgesADeviceOfSomeSort
        , Treacheries.doomed
        ]
    liftIO $ (toCardCode <$> relicOwnedBy "04004" (attrsWith cards)) `shouldBe` Just "04061"

  it "ignores a relic recorded against a different investigator" $ gameTest \_ -> do
    card <- genCard Assets.relicOfAgesADeviceOfSomeSort
    liftIO $ relicOwnedBy "04003" (attrsWith [card]) `shouldBe` Nothing
 where
  relicPrintings =
    [ Assets.relicOfAgesADeviceOfSomeSort
    , Assets.relicOfAgesForestallingTheFuture
    , Assets.relicOfAgesRepossessThePast
    , Assets.relicOfAgesUnleashTheTimestream
    ]

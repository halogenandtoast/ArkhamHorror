module Arkham.Homebrew.DarkMatter.Assets.ErwinSimmonsFading (erwinSimmonsFading) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.DarkMatter.CardDefs.Assets qualified as Cards
import Arkham.Homebrew.DarkMatter.Helpers (placeFacedownInThreatArea)
import Arkham.Matcher

newtype ErwinSimmonsFading = ErwinSimmonsFading AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

erwinSimmonsFading :: AssetCard ErwinSimmonsFading
erwinSimmonsFading = asset ErwinSimmonsFading Cards.erwinSimmonsFading

{- | "Forced - At the end of the round or when you would resign, if there are
face-down encounter cards in any investigator's threat area: Shuffle Erwin
Simmons with all face-down encounter cards and distribute them back into each
investigator's threat area, as evenly as possible."
-}
instance HasAbilities ErwinSimmonsFading where
  getAbilities (ErwinSimmonsFading a) =
    [controlled a 1 (exists $ TreacheryFacedownInThreatAreaOf Anyone) $ forced $ RoundEnds #when]

instance RunMessage ErwinSimmonsFading where
  runMessage msg a@(ErwinSimmonsFading attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      putCardIntoPlay iid attrs
      pure a
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- redistribute every face-down card, as evenly as possible
      facedown <- select $ TreacheryFacedownInThreatAreaOf Anyone
      investigators <- select UneliminatedInvestigator
      unless (null investigators) do
        let total = length facedown
            n = length investigators
            share i = total `div` n + (if i < total `mod` n then 1 else 0)
        for_ (zip [0 ..] investigators) \(i, iid) -> placeFacedownInThreatArea iid (share i)
      pure a
    _ -> ErwinSimmonsFading <$> liftRunMessage msg attrs

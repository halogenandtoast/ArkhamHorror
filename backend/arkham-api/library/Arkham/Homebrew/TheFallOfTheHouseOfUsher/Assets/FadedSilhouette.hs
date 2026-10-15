module Arkham.Homebrew.TheFallOfTheHouseOfUsher.Assets.FadedSilhouette (fadedSilhouette) where

import Arkham.Ability
import Arkham.Asset.Import.Lifted
import Arkham.Card (genCard)
import Arkham.Deck qualified as Deck
import Arkham.Homebrew.TheFallOfTheHouseOfUsher.CardDefs.Assets qualified as Cards
import Arkham.Matcher

newtype FadedSilhouette = FadedSilhouette AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fadedSilhouette :: AssetCard FadedSilhouette
fadedSilhouette = asset FadedSilhouette Cards.fadedSilhouette

instance HasAbilities FadedSilhouette where
  getAbilities (FadedSilhouette a) = [controlled a 1 NoRestriction $ forced $ GameBegins #when]

instance RunMessage FadedSilhouette where
  runMessage msg a@(FadedSilhouette attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      madeline <- genCard Cards.madelineUsher
      shuffleCardsIntoDeck Deck.EncounterDeck [madeline]
      pure a
    _ -> FadedSilhouette <$> liftRunMessage msg attrs

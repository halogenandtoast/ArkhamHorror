module Arkham.Asset.Assets.DeckOfPossibilitiesTychokineticImplement (deckOfPossibilitiesTychokineticImplement) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted

newtype DeckOfPossibilitiesTychokineticImplement = DeckOfPossibilitiesTychokineticImplement AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deckOfPossibilitiesTychokineticImplement :: AssetCard DeckOfPossibilitiesTychokineticImplement
deckOfPossibilitiesTychokineticImplement = asset DeckOfPossibilitiesTychokineticImplement Cards.deckOfPossibilitiesTychokineticImplement

instance RunMessage DeckOfPossibilitiesTychokineticImplement where
  runMessage msg (DeckOfPossibilitiesTychokineticImplement attrs) = runQueueT $ case msg of
    _ -> DeckOfPossibilitiesTychokineticImplement <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.Courage (
  courage,
  Courage (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card (replaceCard, toCard)

newtype Courage = Courage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courage :: AssetCard Courage
courage = assetWith Courage Cards.courage (sanityL ?~ 2)

instance RunMessage Courage where
  runMessage msg (Courage attrs) = case msg of
    -- "Discard this card if it leaves play for any reason." Courage is only a
    -- treatment applied while the card is in play; once it leaves play the card
    -- reverts to its true identity. The asset's `toCard` already reverts the
    -- stored zone copies via the original card code, but the gameCards registry
    -- still maps this card id to Courage, so a later by-id lookup (getCard) would
    -- resurrect Courage when the card is redrawn and played. Restore the registry
    -- entry to the original card too.
    RemovedFromPlay (isSource attrs -> True) -> do
      replaceCard attrs.cardId (toCard attrs)
      Courage <$> runMessage msg attrs
    _ -> Courage <$> runMessage msg attrs

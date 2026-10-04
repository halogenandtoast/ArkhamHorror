module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.RingOfTheDeep (ringOfTheDeep) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

newtype RingOfTheDeep = RingOfTheDeep AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Inside a Crate of Goods. Revelation: the investigator at your location with the
lowest [willpower] takes control of it. You get -1 [willpower] and -1 sanity.
Discarding 3 [willpower] icons' worth of cards removes it from the game.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
ringOfTheDeep :: AssetCard RingOfTheDeep
ringOfTheDeep = asset RingOfTheDeep Cards.ringOfTheDeep

instance RunMessage RingOfTheDeep where
  runMessage msg (RingOfTheDeep attrs) = runQueueT $ RingOfTheDeep <$> liftRunMessage msg attrs

module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.CrimsonLedger (crimsonLedger) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

newtype CrimsonLedger = CrimsonLedger AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Inside a Crate of Goods. Revelation: take control of it. Remove it from the game
and take X horror, where X is a non-Elite enemy's remaining health, to discard
that enemy.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
crimsonLedger :: AssetCard CrimsonLedger
crimsonLedger = asset CrimsonLedger Cards.crimsonLedger

instance RunMessage CrimsonLedger where
  runMessage msg (CrimsonLedger attrs) = runQueueT $ CrimsonLedger <$> liftRunMessage msg attrs

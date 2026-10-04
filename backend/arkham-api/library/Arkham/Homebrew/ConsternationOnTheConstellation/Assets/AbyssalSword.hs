module Arkham.Homebrew.ConsternationOnTheConstellation.Assets.AbyssalSword (abyssalSword) where

import Arkham.Asset.Import.Lifted
import Arkham.Homebrew.ConsternationOnTheConstellation.CardDefs.Assets qualified as Cards

newtype AbyssalSword = AbyssalSword AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

{- | Inside a Crate of Goods. Revelation: an investigator at your location takes
control of it. You get +1 [combat]. Its Fight action adds +1 [combat] and +1
damage, and 2 more damage on a symbol token.
-}

-- TODO: the printed text is not implemented yet. The full transcription is in
-- @docs/homebrew/data/consternation-on-the-constellation-card-text.md@.
abyssalSword :: AssetCard AbyssalSword
abyssalSword = asset AbyssalSword Cards.abyssalSword

instance RunMessage AbyssalSword where
  runMessage msg (AbyssalSword attrs) = runQueueT $ AbyssalSword <$> liftRunMessage msg attrs

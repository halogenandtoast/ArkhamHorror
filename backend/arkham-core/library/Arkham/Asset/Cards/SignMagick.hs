module Arkham.Asset.Cards.SignMagick (
  signMagick,
  SignMagick (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Trait

newtype SignMagick = SignMagick AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

signMagick :: AssetCard SignMagick
signMagick =
  asset SignMagick Cards.signMagick

instance RunMessage SignMagick where
  runMessage msg (SignMagick attrs) = case msg of
    CardEnteredPlay iid card | toCardId card == toCardId attrs -> do
      push
        $ AddSlot iid ArcaneSlot
        $ RestrictedSlot (toSource attrs) (CardWithOneOf [CardWithTrait Spell, CardWithTrait Ritual]) []
      SignMagick <$> runMessage msg attrs
    _ -> SignMagick <$> runMessage msg attrs

module Arkham.Asset.Assets.CharonsObol1 (charonsObol1, CharonsObol1 (..)) where

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Modifier

newtype CharonsObol1 = CharonsObol1 AssetAttrs
  deriving anyclass (IsAsset, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

charonsObol1 :: AssetCard CharonsObol1
charonsObol1 = asset CharonsObol1 Cards.charonsObol1

instance RunMessage CharonsObol1 where
  runMessage msg (CharonsObol1 attrs) = runQueueT $ case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      gameModifiers attrs iid [KilledIfDefeated, XPModifier "Charon's Obol" 2]
      CharonsObol1 <$> liftRunMessage msg attrs
    _ -> CharonsObol1 <$> liftRunMessage msg attrs

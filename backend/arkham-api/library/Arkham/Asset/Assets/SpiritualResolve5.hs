module Arkham.Asset.Assets.SpiritualResolve5 (spiritualResolve5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher

newtype SpiritualResolve5 = SpiritualResolve5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

spiritualResolve5 :: AssetCard SpiritualResolve5
spiritualResolve5 = assetWith SpiritualResolve5 Cards.spiritualResolve5 $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasAbilities SpiritualResolve5 where
  getAbilities (SpiritualResolve5 attrs) =
    [ controlled attrs 1 (thisExists attrs $ oneOf [AssetWithDamage, AssetWithHorror])
        $ FastAbility
        $ HandDiscardCost 1
        $ basic
        $ cardIs Cards.spiritualResolve5
    ]

instance RunMessage SpiritualResolve5 where
  runMessage msg a@(SpiritualResolve5 attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      healAllDamageAndHorror (attrs.ability 1) attrs
      pure a
    _ -> SpiritualResolve5 <$> liftRunMessage msg attrs

module Arkham.Asset.Cards.SpiritualResolve5 (
  spiritualResolve5,
  SpiritualResolve5 (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype SpiritualResolve5 = SpiritualResolve5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

spiritualResolve5 :: AssetCard SpiritualResolve5
spiritualResolve5 = assetWith SpiritualResolve5 Cards.spiritualResolve5 $ (healthL ?~ 3) . (sanityL ?~ 3)

instance HasAbilities SpiritualResolve5 where
  getAbilities (SpiritualResolve5 attrs) =
    [ restrictedAbility attrs 1 ControlsThis
        $ FastAbility
        $ HandDiscardCost 1
        $ cardIs Cards.spiritualResolve5
    ]

instance RunMessage SpiritualResolve5 where
  runMessage msg a@(SpiritualResolve5 attrs) = case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      push $ HealAllDamageAndHorror (toTarget attrs) (toAbilitySource attrs 1)
      pure a
    _ -> SpiritualResolve5 <$> runMessage msg attrs

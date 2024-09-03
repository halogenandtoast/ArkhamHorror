module Arkham.Asset.Cards.SinEater3 (sinEater3, SinEater3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted)
import Arkham.Asset.Uses
import Arkham.Helpers.Modifiers (ModifierType (..), withoutModifier)
import Arkham.Matcher

newtype SinEater3 = SinEater3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sinEater3 :: AssetCard SinEater3
sinEater3 = asset SinEater3 Cards.sinEater3

instance HasAbilities SinEater3 where
  getAbilities (SinEater3 x) =
    [ controlledAbility x 1 (exists $ AssetControlledBy You <> AssetWithAnyDoom <> not_ (be x))
        $ FastAbility (exhaust x)
    , controlledAbility x 2 (exists $ AssetWithAnyDoom <> be x) actionAbility
    ]

instance RunMessage SinEater3 where
  runMessage msg a@(SinEater3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      assets <- select $ assetControlledBy iid <> AssetWithAnyDoom <> not_ (be attrs)
      chooseOrRunOneToHandle iid (attrs.ability 1) assets
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (AssetTarget aid) -> do
      push $ MoveTokens (attrs.ability 1) (toSource aid) (toTarget attrs) Doom 1
      canReady <- andM [aid <=~> AssetExhausted, withoutModifier iid ControlledAssetsCannotReady]
      canHaveCharge <- aid <=~> AssetCanHaveUses Charge

      when (canReady || canHaveCharge) do
        chooseOrRunOne iid
          $ [Label "Ready Asset" [Ready (toTarget aid)] | canReady]
          <> [Label "Add Charge" [AddUses (attrs.ability 1) aid Charge 1] | canHaveCharge]

      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      push $ RemoveAllDoom (attrs.ability 2) (toTarget attrs)
      pure a
    _ -> SinEater3 <$> liftRunMessage msg attrs

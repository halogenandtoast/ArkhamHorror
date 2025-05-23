module Arkham.Asset.Assets.Painkillers (painkillers) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator
import Arkham.Matcher hiding (FastPlayerWindow)

newtype Painkillers = Painkillers AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

painkillers :: AssetCard Painkillers
painkillers = asset Painkillers Cards.painkillers

instance HasAbilities Painkillers where
  getAbilities (Painkillers a) =
    [ controlled a 1 (exists (HealableInvestigator (toSource a) #damage You))
        $ FastAbility (assetUseCost a Supply 1 <> exhaust a <> HorrorCost (toSource a) YouTarget 1)
    ]

instance RunMessage Painkillers where
  runMessage msg a@(Painkillers attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      whenM (canHaveDamageHealed (attrs.ability 1) iid) $ healDamage iid (attrs.ability 1) 1
      pure a
    _ -> Painkillers <$> liftRunMessage msg attrs

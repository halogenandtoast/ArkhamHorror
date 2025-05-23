module Arkham.Asset.Assets.ForbiddenKnowledge (forbiddenKnowledge) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses

newtype ForbiddenKnowledge = ForbiddenKnowledge AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forbiddenKnowledge :: AssetCard ForbiddenKnowledge
forbiddenKnowledge = assetWith ForbiddenKnowledge Cards.forbiddenKnowledge discardWhenNoUses

instance HasAbilities ForbiddenKnowledge where
  getAbilities (ForbiddenKnowledge a) =
    [ restricted a 1 ControlsThis
        $ FastAbility (assetUseCost a Secret 1 <> HorrorCost (toSource a) YouTarget 1 <> exhaust a)
    ]

instance RunMessage ForbiddenKnowledge where
  runMessage msg a@(ForbiddenKnowledge attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      gainResources iid (attrs.ability 1) 1
      pure a
    _ -> ForbiddenKnowledge <$> liftRunMessage msg attrs

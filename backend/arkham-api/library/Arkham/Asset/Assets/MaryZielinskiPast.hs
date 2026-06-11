module Arkham.Asset.Assets.MaryZielinskiPast (maryZielinskiPast) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Investigate

newtype MaryZielinskiPast = MaryZielinskiPast AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maryZielinskiPast :: AssetCard MaryZielinskiPast
maryZielinskiPast = ally MaryZielinskiPast Cards.maryZielinskiPast (2, 2)

instance HasAbilities MaryZielinskiPast where
  getAbilities (MaryZielinskiPast a) = [investigateAbility a 1 (exhaust a) OnSameLocation]

instance RunMessage MaryZielinskiPast where
  runMessage msg a@(MaryZielinskiPast attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      gainClues iid (attrs.ability 1) 1
      pure a
    _ -> MaryZielinskiPast <$> liftRunMessage msg attrs

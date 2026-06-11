module Arkham.Asset.Assets.MaryZielinskiFuture (maryZielinskiFuture) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelect)
import Arkham.Investigate
import Arkham.Matcher

newtype MaryZielinskiFuture = MaryZielinskiFuture AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

maryZielinskiFuture :: AssetCard MaryZielinskiFuture
maryZielinskiFuture = ally MaryZielinskiFuture Cards.maryZielinskiFuture (1, 2)

instance HasModifiersFor MaryZielinskiFuture where
  getModifiersFor (MaryZielinskiFuture a) =
    modifySelect a (InvestigatorAt $ locationWithAsset a) [SkillModifier #intellect 1]

instance HasAbilities MaryZielinskiFuture where
  getAbilities (MaryZielinskiFuture a) = [investigateAbility a 1 (exhaust a) OnSameLocation]

instance RunMessage MaryZielinskiFuture where
  runMessage msg a@(MaryZielinskiFuture attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      pushM $ mkInvestigate sid iid (attrs.ability 1) <&> setTarget attrs
      pure a
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ -> do
      gainClues iid (attrs.ability 1) 1
      pure a
    _ -> MaryZielinskiFuture <$> liftRunMessage msg attrs

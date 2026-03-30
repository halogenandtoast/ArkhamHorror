module Arkham.Asset.Assets.ScrollOfThePharaohs (scrollOfThePharaohs) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.CampaignLogKey
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Log

newtype ScrollOfThePharaohs = ScrollOfThePharaohs AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

scrollOfThePharaohs :: AssetCard ScrollOfThePharaohs
scrollOfThePharaohs = asset ScrollOfThePharaohs Cards.scrollOfThePharaohs

instance HasAbilities ScrollOfThePharaohs where
  getAbilities (ScrollOfThePharaohs a) =
    [controlled_ a 1 $ actionAbilityWithCost (exhaust a <> assetUseCost a Secret 1)]

instance RunMessage ScrollOfThePharaohs where
  runMessage msg a@(ScrollOfThePharaohs attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      investigators <- select $ InvestigatorAt (locationWithInvestigator iid)
      allies <- select $ #ally <> AssetAt (locationWithInvestigator iid)
      chooseOrRunOneM iid do
        targets investigators \iid' -> assignHorror iid' source 1
        targets allies \x -> dealAssetDirectHorror x source 1
      when (attrs.use #secret == 0) do
        toDiscardBy iid source attrs
        drawCards iid source 3
        record YouHaveUnearthedTheSecretsOfThePharaohs
      pure a
    _ -> ScrollOfThePharaohs <$> liftRunMessage msg attrs

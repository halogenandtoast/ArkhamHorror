module Arkham.Asset.Assets.FeedTheMind3 (feedTheMind3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Investigator (getExcessInHandCount)

newtype FeedTheMind3 = FeedTheMind3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind3 :: AssetCard FeedTheMind3
feedTheMind3 = asset FeedTheMind3 Cards.feedTheMind3

instance HasAbilities FeedTheMind3 where
  getAbilities (FeedTheMind3 a) =
    [ skillTestAbility $ controlled_ a 1 $ actionAbilityWithCost $ exhaust a <> assetUseCost a Secret 1
    ]

instance RunMessage FeedTheMind3 where
  runMessage msg a@(FeedTheMind3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 0)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      drawCards iid (attrs.ability 1) n
      doStep 1 msg
      pure a
    DoStep 1 (PassedThisSkillTest iid (isAbilitySource attrs 1 -> True)) -> do
      excess <- getExcessInHandCount iid
      when (excess > 0) $ assignHorror iid (attrs.ability 1) excess
      pure a
    _ -> FeedTheMind3 <$> liftRunMessage msg attrs

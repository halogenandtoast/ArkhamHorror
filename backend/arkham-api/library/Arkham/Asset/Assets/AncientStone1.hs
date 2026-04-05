module Arkham.Asset.Assets.AncientStone1 (ancientStone1) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.CampaignLogKey
import Arkham.Helpers.Location (withLocationOf)
import Arkham.Helpers.SkillTest (getSkillTestDifficulty, withSkillTest)
import Arkham.Message.Lifted.Log
import Arkham.Modifier

newtype AncientStone1 = AncientStone1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientStone1 :: AssetCard AncientStone1
ancientStone1 = asset AncientStone1 Cards.ancientStone1

instance HasAbilities AncientStone1 where
  getAbilities (AncientStone1 a) = [controlled_ a 1 investigateAction_]

instance RunMessage AncientStone1 where
  runMessage msg a@(AncientStone1 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      withLocationOf iid \lid -> skillTestModifiers sid attrs lid [ShroudModifier 3]
      investigate sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      difficulty <- fromJustNote "missing" <$> getSkillTestDifficulty
      withSkillTest \sid -> priority $ skillTestModifier sid (attrs.ability 1) iid (DiscoveredClues 1)
      toDiscardBy iid (attrs.ability 1) attrs
      recordCount YouHaveIdentifiedTheStone difficulty
      pure a
    _ -> AncientStone1 <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.Chainsaw4 (chainsaw4) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Chainsaw4 = Chainsaw4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chainsaw4 :: AssetCard Chainsaw4
chainsaw4 = asset Chainsaw4 Cards.chainsaw4

instance HasAbilities Chainsaw4 where
  getAbilities (Chainsaw4 a) = [controlled_ a 1 $ fightAction $ assetUseCost a Supply 1]

instance RunMessage Chainsaw4 where
  runMessage msg a@(Chainsaw4 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #combat 2, DamageDealt 2]
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      mEnemy <- runMaybeT $ selectMaybeT . EnemyWithId =<< MaybeT getSkillTestTargetedEnemy
      chooseOneM iid $ withI18n do
        labeled "cards.chainsaw4.place1SupplyOnChainsaw" do
          addUses (attrs.ability 1) (toId attrs) Supply 1
        for_ mEnemy \eid -> do
          labeled "cards.chainsaw4.deal1DamageToTheAttackedEnemy" do
            nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1 eid
      pure a
    _ -> Chainsaw4 <$> liftRunMessage msg attrs

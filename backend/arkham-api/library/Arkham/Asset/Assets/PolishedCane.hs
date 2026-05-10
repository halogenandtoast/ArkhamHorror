module Arkham.Asset.Assets.PolishedCane (polishedCane) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose

newtype PolishedCane = PolishedCane AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

polishedCane :: AssetCard PolishedCane
polishedCane = asset PolishedCane Cards.polishedCane

instance HasAbilities PolishedCane where
  getAbilities (PolishedCane a) = [controlled_ a 1 fightAction_]

instance RunMessage PolishedCane where
  runMessage msg a@(PolishedCane attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      addSkillValue sid source iid #agility
      chooseFightEnemy sid iid source
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | n >= 2 -> do
      runMaybeT_ do
        guard attrs.ready
        eid <- MaybeT getSkillTestTargetedEnemy
        liftGuardM $ eid <=~> (NonEliteEnemy <> EnemyCanBeEvadedBy (attrs.ability 1))
        lift $ chooseOneM iid do
          (cardI18n $ labeled' "polishedCane.exhaustPolishedCaneToAutomaticallyEvadeThisEnemy") do
            exhaustThis attrs
            automaticallyEvadeEnemy iid eid
          (cardI18n $ labeled' "polishedCane.doNotEvade") nothing
      pure a
    _ -> PolishedCane <$> liftRunMessage msg attrs

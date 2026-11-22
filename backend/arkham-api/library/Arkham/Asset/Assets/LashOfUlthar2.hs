module Arkham.Asset.Assets.LashOfUlthar2 (lashOfUlthar2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.Dreaming.Lifted (flipAnAssetFacedown)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.I18n
import Arkham.Matcher hiding (EnemyEvaded)
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype LashOfUlthar2 = LashOfUlthar2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lashOfUlthar2 :: AssetCard LashOfUlthar2
lashOfUlthar2 = asset LashOfUlthar2 Cards.lashOfUlthar2

instance HasAbilities LashOfUlthar2 where
  getAbilities (LashOfUlthar2 a) = [fightAbility a 1 mempty ControlsThis]

instance RunMessage LashOfUlthar2 where
  runMessage msg a@(LashOfUlthar2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      flipAnAssetFacedown iid 2
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      runMaybeT_ do
        guard attrs.ready
        eid <- MaybeT getSkillTestTargetedEnemy
        liftGuardM $ eid <=~> (NonEliteEnemy <> EnemyCanBeEvadedBy (attrs.ability 1))
        lift $ chooseOneM iid do
          (cardI18n $ labeled "lashOfUlthar2.exhaustToEvade") do
            exhaustThis attrs
            automaticallyEvadeEnemy iid eid
          labeledI "doNotExhaust" nothing
      pure a
    _ -> LashOfUlthar2 <$> liftRunMessage msg attrs

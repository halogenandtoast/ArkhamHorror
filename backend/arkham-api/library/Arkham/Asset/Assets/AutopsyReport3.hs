module Arkham.Asset.Assets.AutopsyReport3 (autopsyReport3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (EnemyDefeated)
import Arkham.Helpers.Enemy (getDefeatedEnemyHealth)
import Arkham.Helpers.Window (defeatedEnemy)
import Arkham.Matcher
import Arkham.Modifier

newtype AutopsyReport3 = AutopsyReport3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

autopsyReport3 :: AssetCard AutopsyReport3
autopsyReport3 = asset AutopsyReport3 Cards.autopsyReport3

instance HasAbilities AutopsyReport3 where
  getAbilities (AutopsyReport3 a) =
    [controlled_ a 1 $ triggered (EnemyDefeated #after Anyone ByAny (EnemyAt YourLocation)) (exhaust a)]

instance RunMessage AutopsyReport3 where
  runMessage msg a@(AutopsyReport3 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (defeatedEnemy -> eid) _ -> do
      getDefeatedEnemyHealth eid >>= traverse_ \health -> do
        let bonus = min 5 health
        sid <- getRandom
        skillTestModifier sid attrs iid (SkillModifier #intellect bonus)
        investigate sid iid (attrs.ability 1)
      pure a
    _ -> AutopsyReport3 <$> liftRunMessage msg attrs

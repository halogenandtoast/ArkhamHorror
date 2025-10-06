module Arkham.Asset.Assets.FortyFiveThompsonRogue3 (fortyFiveThompsonRogue3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Enemy.Types (Field (..))
import Arkham.Helpers.Modifiers (withoutModifier)
import Arkham.Helpers.SkillTest (getSkillTestTargetedEnemy)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier hiding (EnemyFight)
import Arkham.Projection

newtype FortyFiveThompsonRogue3 = FortyFiveThompsonRogue3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompsonRogue3 :: AssetCard FortyFiveThompsonRogue3
fortyFiveThompsonRogue3 = asset FortyFiveThompsonRogue3 Cards.fortyFiveThompsonRogue3

instance HasAbilities FortyFiveThompsonRogue3 where
  getAbilities (FortyFiveThompsonRogue3 a) =
    [restricted a 1 ControlsThis $ fightAction (assetUseCost a Ammo 1)]

instance RunMessage FortyFiveThompsonRogue3 where
  runMessage msg a@(FortyFiveThompsonRogue3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2]
      chooseFightEnemy sid iid source
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n | attrs.use Ammo > 0 -> do
      whenJustM getSkillTestTargetedEnemy \eid -> do
        fightValue <- fieldJust EnemyFight eid
        when (n >= fightValue) $ do
          enemies <- select $ enemyAtLocationWith iid <> NotEnemy (EnemyWithId eid)
          canDealDamage <- withoutModifier iid CannotDealDamage
          chooseOrRunOneM iid do
            labeled "Do not damage any enemies" nothing
            when canDealDamage do
              targets enemies \eid' -> do
                spendUses (attrs.ability 1) attrs Ammo 1
                push $ InvestigatorDamageEnemy iid eid' (toSource attrs)
      pure a
    _ -> FortyFiveThompsonRogue3 <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.GatlingGun5 (gatlingGun5) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.DamageEffect
import Arkham.Enemy.Types (Field (EnemyFight))
import Arkham.Fight.Types
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier (ModifierType (DamageDealt, NoStandardDamage, SkillModifier))

newtype GatlingGun5 = GatlingGun5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gatlingGun5 :: AssetCard GatlingGun5
gatlingGun5 = asset GatlingGun5 Cards.gatlingGun5

instance HasAbilities GatlingGun5 where
  getAbilities (GatlingGun5 a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ UseCostUpTo (AssetWithId $ toId a) Ammo 1 6]

instance RunMessage GatlingGun5 where
  runMessage msg a@(GatlingGun5 attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUsesPayment -> n) -> do
      sid <- getRandom
      skillTestModifiers
        sid
        (attrs.ability 1)
        iid
        [SkillModifier #combat n, NoStandardDamage, DamageDealt n]
      chooseFightEnemyEdit sid iid (attrs.ability 1) \x -> do
        setTarget attrs
          $ x
            { chooseFightDifficulty =
                CalculatedChooseFightDifficulty
                  $ SumEnemyMaybeFieldCalculation (at_ $ locationWithInvestigator iid) EnemyFight
            }
      pure $ GatlingGun5 $ attrs & setMetaKey "gatlingGun5_ammo" n
    Successful (Action.Fight, EnemyTarget _) iid _ (isTarget attrs -> True) _ -> do
      let x = getMetaKeyDefault "gatlingGun5_ammo" 0 attrs
      damage <- damageValueFor x iid DamageForEnemy
      doStep damage msg
      pure a
    DoStep n msg'@(Successful (Action.Fight, EnemyTarget _) iid _ (isTarget attrs -> True) _) -> do
      if n == 0
        then do
          let enemies :: [EnemyId] = getMetaKeyDefault "gatlingGun5_damaged" [] attrs
          for_ enemies $ checkDefeated (attrs.ability 1)
        else do
          enemies <- select $ enemy_ $ at_ $ locationWithInvestigator iid
          chooseOrRunOneM iid $ targets enemies $ handleTarget iid (attrs.ability 1)
          doStep (n - 1) msg'
      pure a
    HandleTargetChoice _iid (isAbilitySource attrs 1 -> True) (EnemyTarget eid) -> do
      let enemies = getMetaKeyDefault "gatlingGun5_damaged" [] attrs
      push $ EnemyDamage eid $ delayDamage $ isDirect $ attack (attrs.ability 1) 1
      pure $ GatlingGun5 $ attrs & setMetaKey "gatlingGun5_damaged" (eid : enemies)
    _ -> GatlingGun5 <$> liftRunMessage msg attrs

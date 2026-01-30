module Arkham.Asset.Assets.CosmicFlame5 (cosmicFlame5) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CosmicFlame5 = CosmicFlame5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicFlame5 :: AssetCard CosmicFlame5
cosmicFlame5 = asset CosmicFlame5 Cards.cosmicFlame5

instance HasAbilities CosmicFlame5 where
  getAbilities (CosmicFlame5 a) =
    [skillTestAbility $ controlled_ a 1 $ fightActionWith_ #willpower]

instance RunMessage CosmicFlame5 where
  runMessage msg a@(CosmicFlame5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #skull attrs attrs $ doStep 1 msg
      skillTestModifiers sid (attrs.ability 1) iid [SkillModifier #willpower 2, DamageDealt 1]
      chooseFightEnemyWith #willpower sid iid (attrs.ability 1)
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      if attrs.use #charge == 0
        then do
          assignDamage iid (attrs.ability 1) 1
          toDiscardBy iid (attrs.ability 1) attrs
        else removeTokens (attrs.ability 1) attrs Charge 1
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (attrs.use #charge > 0) do
        enemies <-
          select $ EnemyAt (locationWithInvestigator iid) <> EnemyCanBeDamagedBySource (attrs.ability 1)
        unless (null enemies) do
          chooseOneM iid do
            labeled "Spend 1 charge to deal 1 damage to an enemy at your location" do
              spendUses (attrs.ability 1) attrs Charge 1
              chooseTargetM iid enemies $ nonAttackEnemyDamage (Just iid) (attrs.ability 1) 1
            withI18n skip_
      pure a
    _ -> CosmicFlame5 <$> liftRunMessage msg attrs

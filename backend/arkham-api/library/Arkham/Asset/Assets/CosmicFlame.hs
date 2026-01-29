module Arkham.Asset.Assets.CosmicFlame (cosmicFlame) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.I18n
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype CosmicFlame = CosmicFlame AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cosmicFlame :: AssetCard CosmicFlame
cosmicFlame = asset CosmicFlame Cards.cosmicFlame

instance HasAbilities CosmicFlame where
  getAbilities (CosmicFlame a) =
    [skillTestAbility $ controlled_ a 1 $ fightActionWith_ #willpower]

instance RunMessage CosmicFlame where
  runMessage msg a@(CosmicFlame attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #skull attrs attrs $ doStep 1 msg
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
        withSkillTest \sid ->
          chooseOneM iid do
            labeled "Spend 1 charge to deal +1 damage" do
              removeTokens (attrs.ability 1) attrs Charge 1
              skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
            withI18n skip_
      pure a
    _ -> CosmicFlame <$> liftRunMessage msg attrs

module Arkham.Asset.Assets.RealityFracture3 (realityFracture3) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Asset.Uses
import Arkham.Helpers.Dreaming.Lifted (dreamOrElse)
import Arkham.I18n
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype RealityFracture3 = RealityFracture3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

realityFracture3 :: AssetCard RealityFracture3
realityFracture3 = asset RealityFracture3 Cards.realityFracture3

instance HasAbilities RealityFracture3 where
  getAbilities (RealityFracture3 a) =
    [skillTestAbility $ controlled_ a 1 $ fightActionWith_ #willpower]

instance RunMessage RealityFracture3 where
  runMessage msg a@(RealityFracture3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      onRevealChaosTokenEffect sid #cultist attrs attrs $ doStep 1 msg
      chooseFightEnemyWith #willpower sid iid (attrs.ability 1)
      pure a
    {- The charge buys "the attacked enemy gets -2 fight", so the offer waits for
    the enemy to be chosen -- still inside activation, before the test begins. -}
    ChoseEnemy sid iid (isAbilitySource attrs 1 -> True) eid -> do
      whenM (attrs.id <=~> AssetWithSpendableUses (atLeast 1) Charge) do
        chooseOneM iid $ cardI18n $ scope "realityFracture3" do
          labeled "spendChargeForFightAndDamage" do
            spendUses (attrs.ability 1) attrs Charge 1
            skillTestModifier sid (attrs.ability 1) eid (EnemyFight (-2))
            skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
          withI18n skip_
      pure a
    DoStep 1 (UseThisAbility iid (isSource attrs -> True) 1) -> do
      cardI18n $ scope "realityFracture3" $ dreamOrElse iid (attrs.ability 1) "takeDamageAndDiscard" do
        assignDamage iid (attrs.ability 1) 1
        toDiscardBy iid (attrs.ability 1) attrs
      pure a
    _ -> RealityFracture3 <$> liftRunMessage msg attrs

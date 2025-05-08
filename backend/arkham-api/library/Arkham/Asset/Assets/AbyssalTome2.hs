module Arkham.Asset.Assets.AbyssalTome2 (abyssalTome2) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Fight
import Arkham.Helpers.Modifiers (ModifierType (..), maybeModified_)
import Arkham.Helpers.SkillTest (isSkillTestSource)
import Arkham.Message.Lifted.Choose
import Arkham.Projection

newtype AbyssalTome2 = AbyssalTome2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abyssalTome2 :: AssetCard AbyssalTome2
abyssalTome2 = asset AbyssalTome2 Cards.abyssalTome2

instance HasAbilities AbyssalTome2 where
  getAbilities (AbyssalTome2 a) = [restricted a 1 ControlsThis $ fightAction (exhaust a)]

instance HasModifiersFor AbyssalTome2 where
  getModifiersFor (AbyssalTome2 a) = for_ a.controller \iid -> do
    maybeModified_ a iid do
      liftGuardM $ isSkillTestSource (a.ability 1)
      doom <- lift $ field AssetDoom a.id
      pure [AnySkillValue doom, DamageDealt doom]

instance RunMessage AbyssalTome2 where
  runMessage msg a@(AbyssalTome2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAmounts iid "Amount of Doom to Place" (MaxAmountTarget 3) [("Doom", (0, 3))] attrs

      sid <- getRandom
      chooseOneM iid $ for_ [#intellect, #willpower, #combat] \sType -> do
        skillLabeled sType $ chooseFightEnemyEdit sid iid (attrs.ability 1) (withSkillType sType)
      pure a
    ResolveAmounts _ (getChoiceAmount "Doom" -> n) (isTarget attrs -> True) -> do
      placeDoom (attrs.ability 1) attrs n
      pure a
    _ -> AbyssalTome2 <$> liftRunMessage msg attrs

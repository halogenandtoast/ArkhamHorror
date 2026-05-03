module Arkham.Asset.Assets.CrystalPendulum (crystalPendulum, crystalPendulumEffect) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.Modifiers
import Arkham.Matcher

newtype CrystalPendulum = CrystalPendulum AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPendulum :: AssetCard CrystalPendulum
crystalPendulum = asset CrystalPendulum Cards.crystalPendulum

instance HasModifiersFor CrystalPendulum where
  getModifiersFor (CrystalPendulum a) = controllerGets a [SkillModifier #willpower 1]

instance HasAbilities CrystalPendulum where
  getAbilities (CrystalPendulum a) =
    [ controlled_ a 1
        $ triggered
          (InitiatedSkillTest #after (colocatedWithMatch You) AnySkillType AnySkillTestValue #any)
          (exhaust a)
    ]

instance RunMessage CrystalPendulum where
  runMessage msg a@(CrystalPendulum attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      chooseAmounts iid "Name a number" (MaxAmountTarget 1000) [("Number", (0, 1000))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Number" -> n) (isTarget attrs -> True) -> do
      createCardEffect Cards.crystalPendulum (effectInt n) (attrs.ability 1) iid
      pure a
    _ -> CrystalPendulum <$> liftRunMessage msg attrs

newtype CrystalPendulumEffect = CrystalPendulumEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPendulumEffect :: EffectArgs -> CrystalPendulumEffect
crystalPendulumEffect = cardEffect CrystalPendulumEffect Cards.crystalPendulum

instance RunMessage CrystalPendulumEffect where
  runMessage msg e@(CrystalPendulumEffect attrs) = runQueueT $ case msg of
    PassedThisSkillTestBy _ _ n | Just (EffectInt n) == attrs.meta -> do
      case attrs.target of
        InvestigatorTarget iid -> do
          drawCards iid (toAbilitySource attrs.source 1) 1
          disableReturn e
        _ -> pure e
    FailedThisSkillTestBy _ _ n | Just (EffectInt n) == attrs.meta -> do
      case attrs.target of
        InvestigatorTarget iid -> do
          drawCards iid (toAbilitySource attrs.source 1) 1
          disableReturn e
        _ -> pure e
    SkillTestEnds {} -> disableReturn e
    _ -> CrystalPendulumEffect <$> liftRunMessage msg attrs

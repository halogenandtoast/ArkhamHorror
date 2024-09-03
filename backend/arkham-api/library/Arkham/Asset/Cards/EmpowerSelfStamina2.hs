module Arkham.Asset.Cards.EmpowerSelfStamina2 (empowerSelfStamina2, EmpowerSelfStamina2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype EmpowerSelfStamina2 = EmpowerSelfStamina2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empowerSelfStamina2 :: AssetCard EmpowerSelfStamina2
empowerSelfStamina2 = asset EmpowerSelfStamina2 Cards.empowerSelfStamina2

instance HasModifiersFor EmpowerSelfStamina2 where
  getModifiersFor (InvestigatorTarget iid) (EmpowerSelfStamina2 a) | controlledBy a iid = do
    pure $ toModifiers a [CanIgnoreAspect $ AspectIs $ InsteadOfAspect $ #willpower `InsteadOf` #combat]
  getModifiersFor target (EmpowerSelfStamina2 a) | a `is` target = do
    pure $ toModifiers a [SharesSlotWith 3 "Empower Self"]
  getModifiersFor _ _ = pure []

instance HasAbilities EmpowerSelfStamina2 where
  getAbilities (EmpowerSelfStamina2 a) = [controlledAbility a 1 DuringAnySkillTest (FastAbility $ exhaust a)]

instance RunMessage EmpowerSelfStamina2 where
  runMessage msg a@(EmpowerSelfStamina2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      pure a
    _ -> EmpowerSelfStamina2 <$> runMessage msg attrs

module Arkham.Asset.Cards.EmpowerSelfAcuity2 (empowerSelfAcuity2, EmpowerSelfAcuity2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype EmpowerSelfAcuity2 = EmpowerSelfAcuity2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empowerSelfAcuity2 :: AssetCard EmpowerSelfAcuity2
empowerSelfAcuity2 = asset EmpowerSelfAcuity2 Cards.empowerSelfAcuity2

instance HasModifiersFor EmpowerSelfAcuity2 where
  getModifiersFor (InvestigatorTarget iid) (EmpowerSelfAcuity2 a) | controlledBy a iid = do
    pure
      $ toModifiers a [CanIgnoreAspect $ AspectIs $ InsteadOfAspect $ #willpower `InsteadOf` #intellect]
  getModifiersFor target (EmpowerSelfAcuity2 a) | a `is` target = do
    pure $ toModifiers a [SharesSlotWith 3 "Empower Self"]
  getModifiersFor _ _ = pure []

instance HasAbilities EmpowerSelfAcuity2 where
  getAbilities (EmpowerSelfAcuity2 a) =
    [controlledAbility a 1 DuringAnySkillTest (FastAbility $ exhaust a)]

instance RunMessage EmpowerSelfAcuity2 where
  runMessage msg a@(EmpowerSelfAcuity2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #intellect 2)
      pure a
    _ -> EmpowerSelfAcuity2 <$> runMessage msg attrs

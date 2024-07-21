module Arkham.Asset.Cards.EmpowerSelfAlacrity2 (empowerSelfAlacrity2, EmpowerSelfAlacrity2 (..)) where

import Arkham.Ability
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype EmpowerSelfAlacrity2 = EmpowerSelfAlacrity2 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

empowerSelfAlacrity2 :: AssetCard EmpowerSelfAlacrity2
empowerSelfAlacrity2 =
  asset EmpowerSelfAlacrity2 Cards.empowerSelfAlacrity2

instance HasModifiersFor EmpowerSelfAlacrity2 where
  getModifiersFor (InvestigatorTarget iid) (EmpowerSelfAlacrity2 a) | controlledBy a iid = do
    pure
      $ toModifiers a [CanIgnoreAspect $ AspectIs $ InsteadOfAspect $ #willpower `InsteadOf` #agility]
  getModifiersFor target (EmpowerSelfAlacrity2 a) | a `is` target = do
    pure $ toModifiers a [SharesSlotWith 3 "Empower Self"]
  getModifiersFor _ _ = pure []

instance HasAbilities EmpowerSelfAlacrity2 where
  getAbilities (EmpowerSelfAlacrity2 a) =
    [ controlledAbility a 1 DuringAnySkillTest (FastAbility $ exhaust a)
    ]

instance RunMessage EmpowerSelfAlacrity2 where
  runMessage msg a@(EmpowerSelfAlacrity2 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      withSkillTest \sid ->
        push $ skillTestModifier sid (attrs.ability 1) iid (SkillModifier #agility 2)
      pure a
    _ -> EmpowerSelfAlacrity2 <$> runMessage msg attrs

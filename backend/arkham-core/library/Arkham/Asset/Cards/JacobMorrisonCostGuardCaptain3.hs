module Arkham.Asset.Cards.JacobMorrisonCostGuardCaptain3 (
  jacobMorrisonCostGuardCaptain3,
  JacobMorrisonCostGuardCaptain3 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted hiding (AssetExhausted, RevealChaosToken)
import Arkham.Helpers.Modifiers (ModifierType (..))
import Arkham.Matcher

newtype JacobMorrisonCostGuardCaptain3 = JacobMorrisonCostGuardCaptain3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jacobMorrisonCostGuardCaptain3 :: AssetCard JacobMorrisonCostGuardCaptain3
jacobMorrisonCostGuardCaptain3 = ally JacobMorrisonCostGuardCaptain3 Cards.jacobMorrisonCostGuardCaptain3 (2, 2)

instance HasModifiersFor JacobMorrisonCostGuardCaptain3 where
  getModifiersFor target (JacobMorrisonCostGuardCaptain3 a) | a `is` target = do
    pure $ toModifiers a [DoesNotReadyDuringUpkeep]
  getModifiersFor _ _ = pure []

instance HasAbilities JacobMorrisonCostGuardCaptain3 where
  getAbilities (JacobMorrisonCostGuardCaptain3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (WouldHaveSkillTestResult #when You #any #failure) (exhaust a)
    , controlledAbility a 1 (DuringSkillTest (YourSkillTest #any) <> exists (be a <> AssetExhausted))
        $ ReactionAbility (RevealChaosToken #when Anyone #bless) (exhaust a)
    ]

instance RunMessage JacobMorrisonCostGuardCaptain3 where
  runMessage msg a@(JacobMorrisonCostGuardCaptain3 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      skillTestModifier attrs iid (AnySkillValue 2)
      push RecalculateSkillTestResults
      pure a
    UseThisAbility _iid (isSource attrs -> True) 2 -> do
      push $ Ready (toTarget attrs)
      pure a
    _ -> JacobMorrisonCostGuardCaptain3 <$> liftRunMessage msg attrs

module Arkham.Asset.Cards.CrystalPendulum (
  crystalPendulum,
  crystalPendulumEffect,
  CrystalPendulum (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype CrystalPendulum = CrystalPendulum AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPendulum :: AssetCard CrystalPendulum
crystalPendulum = asset CrystalPendulum Cards.crystalPendulum

instance HasModifiersFor CrystalPendulum where
  getModifiersFor (InvestigatorTarget iid) (CrystalPendulum a) =
    pure
      [ toModifier a $ SkillModifier SkillWillpower 1
      | controlledBy a iid
      ]
  getModifiersFor _ _ = pure []

instance HasAbilities CrystalPendulum where
  getAbilities (CrystalPendulum a) =
    [ restrictedAbility a 1 ControlsThis $
        ReactionAbility
          (InitiatedSkillTest Timing.After (InvestigatorAt YourLocation) AnySkillType AnySkillTestValue)
          (ExhaustCost $ toTarget a)
    ]

instance RunMessage CrystalPendulum where
  runMessage msg a@(CrystalPendulum attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ chooseAmounts iid "Name a number" (MaxAmountTarget 1000) [("Number", (0, 1000))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Number" -> n) (isTarget attrs -> True) -> do
      push $ createCardEffect Cards.crystalPendulum (Just $ EffectInt n) attrs iid
      pure a
    _ -> CrystalPendulum <$> runMessage msg attrs

newtype CrystalPendulumEffect = CrystalPendulumEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

crystalPendulumEffect :: EffectArgs -> CrystalPendulumEffect
crystalPendulumEffect = cardEffect CrystalPendulumEffect Cards.crystalPendulum

instance RunMessage CrystalPendulumEffect where
  runMessage msg e@(CrystalPendulumEffect attrs@EffectAttrs {..}) = case msg of
    PassedSkillTest _ _ _ SkillTestInitiatorTarget {} _ n | Just (EffectInt n) == effectMetadata ->
      do
        case effectTarget of
          InvestigatorTarget iid -> do
            drawing <- drawCards iid (AbilitySource effectSource 1) 1
            e
              <$ pushAll
                [ drawing
                , DisableEffect effectId
                ]
          _ -> error "Invalid target"
    FailedSkillTest _ _ _ SkillTestInitiatorTarget {} _ n | Just (EffectInt n) == effectMetadata ->
      do
        case effectTarget of
          InvestigatorTarget iid -> do
            drawing <- drawCards iid (AbilitySource effectSource 1) 1
            e
              <$ pushAll
                [ drawing
                , DisableEffect effectId
                ]
          _ -> error "Invalid target"
    SkillTestEnds {} -> do
      push $ DisableEffect effectId
      pure e
    _ -> CrystalPendulumEffect <$> runMessage msg attrs

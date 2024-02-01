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
import Arkham.Timing qualified as Timing

newtype CrystalPendulum = CrystalPendulum AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

crystalPendulum :: AssetCard CrystalPendulum
crystalPendulum = asset CrystalPendulum Cards.crystalPendulum

instance HasModifiersFor CrystalPendulum where
  getModifiersFor (InvestigatorTarget iid) (CrystalPendulum a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #willpower 1]
  getModifiersFor _ _ = pure []

instance HasAbilities CrystalPendulum where
  getAbilities (CrystalPendulum a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest Timing.After (InvestigatorAt YourLocation) AnySkillType AnySkillTestValue #any)
          (exhaust a)
    ]

instance RunMessage CrystalPendulum where
  runMessage msg a@(CrystalPendulum attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      push $ chooseAmounts player "Name a number" (MaxAmountTarget 1000) [("Number", (0, 1000))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Number" -> n) (isTarget attrs -> True) -> do
      push $ createCardEffect Cards.crystalPendulum (Just $ EffectInt n) (toAbilitySource attrs 1) iid
      pure a
    _ -> CrystalPendulum <$> runMessage msg attrs

newtype CrystalPendulumEffect = CrystalPendulumEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

crystalPendulumEffect :: EffectArgs -> CrystalPendulumEffect
crystalPendulumEffect = cardEffect CrystalPendulumEffect Cards.crystalPendulum

instance RunMessage CrystalPendulumEffect where
  runMessage msg e@(CrystalPendulumEffect attrs) = case msg of
    PassedThisSkillTestBy _ _ n | Just (EffectInt n) == effectMetadata attrs -> do
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          drawing <- drawCards iid (toAbilitySource (effectSource attrs) 1) 1
          pushAll [drawing, DisableEffect $ toId attrs]
        _ -> error "Invalid target"
      pure e
    FailedThisSkillTestBy _ _ n | Just (EffectInt n) == effectMetadata attrs -> do
      case effectTarget attrs of
        InvestigatorTarget iid -> do
          drawing <- drawCards iid (toAbilitySource (effectSource attrs) 1) 1
          pushAll [drawing, DisableEffect $ toId attrs]
        _ -> error "Invalid target"
      pure e
    SkillTestEnds {} -> do
      push $ DisableEffect $ toId attrs
      pure e
    _ -> CrystalPendulumEffect <$> runMessage msg attrs

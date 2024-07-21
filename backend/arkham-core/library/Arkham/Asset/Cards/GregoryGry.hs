module Arkham.Asset.Cards.GregoryGry (gregoryGry, gregoryGryEffect, GregoryGry (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype GregoryGry = GregoryGry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gregoryGry :: AssetCard GregoryGry
gregoryGry = ally GregoryGry Cards.gregoryGry (1, 2)

instance HasAbilities GregoryGry where
  getAbilities (GregoryGry a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest #when You AnySkillType AnySkillTestValue #any)
          (UseCostUpTo (AssetWithId $ toId a) Resource 1 3)
    ]

totalUses :: Payment -> Int
totalUses (Payments ps) = sum $ map totalUses ps
totalUses (UsesPayment n) = n
totalUses _ = 0

instance RunMessage GregoryGry where
  runMessage msg a@(GregoryGry attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ (totalUses -> uses) -> do
      push $ createCardEffect Cards.gregoryGry (Just $ EffectInt uses) (toAbilitySource attrs 1) iid
      pure a
    _ -> GregoryGry <$> runMessage msg attrs

newtype GregoryGryEffect = GregoryGryEffect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

gregoryGryEffect :: EffectArgs -> GregoryGryEffect
gregoryGryEffect = cardEffect GregoryGryEffect Cards.gregoryGry

instance RunMessage GregoryGryEffect where
  runMessage msg e@(GregoryGryEffect attrs) = case msg of
    PassedSkillTest {} -> do
      case effectMetadata attrs of
        Just (EffectInt n) -> do
          let
            iid = fromJustNote "Wrong Type" $ preview _InvestigatorTarget (effectTarget attrs)
          pushAll [disable attrs, takeResources iid attrs.source n]
        _ -> error "Wrong metadata"
      pure e
    SkillTestEnds _ _ _ -> do
      push $ disable attrs
      pure e
    _ -> GregoryGryEffect <$> runMessage msg attrs

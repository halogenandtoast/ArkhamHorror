module Arkham.Asset.Cards.Pantalone (
  pantalone,
  Pantalone (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.SkillTest
import Arkham.SkillType
import Arkham.Timing qualified as Timing

newtype Pantalone = Pantalone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

pantalone :: AssetCard Pantalone
pantalone = asset Pantalone Cards.pantalone

instance HasAbilities Pantalone where
  getAbilities (Pantalone a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility
          (AssetEntersPlay Timing.After $ AssetWithId $ toId a)
          Free
    , restrictedAbility a 2 ControlsThis
        $ ReactionAbility
          (InitiatedSkillTest Timing.When You (NotSkillType SkillIntellect) AnySkillTestValue #any)
          (DiscardCost FromPlay $ toTarget a)
    ]

instance RunMessage Pantalone where
  runMessage msg a@(Pantalone attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      pushM $ drawCards iid (toAbilitySource attrs 1) 2
      pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      skillTest <- getJustSkillTest
      let
        newBase =
          case skillTestBaseValue skillTest of
            SkillBaseValue _ -> SkillBaseValue #intellect
            AndSkillBaseValue _ -> SkillBaseValue #intellect
            HalfResourcesOf x -> HalfResourcesOf x
            StaticBaseValue x -> StaticBaseValue x
      push $ ChangeSkillTestType (SkillSkillTest #intellect) newBase
      pure a
    _ -> Pantalone <$> runMessage msg attrs

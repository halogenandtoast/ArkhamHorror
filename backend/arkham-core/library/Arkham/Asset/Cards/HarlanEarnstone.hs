module Arkham.Asset.Cards.HarlanEarnstone (
  harlanEarnstone,
  HarlanEarnstone (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype HarlanEarnstone = HarlanEarnstone AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

harlanEarnstone :: AssetCard HarlanEarnstone
harlanEarnstone = asset HarlanEarnstone Cards.harlanEarnstone

instance HasAbilities HarlanEarnstone where
  getAbilities (HarlanEarnstone a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility
          [Action.Parley]
          (ActionCost 1 <> DiscardTopOfDeckCost 3)
    ]

instance RunMessage HarlanEarnstone where
  runMessage msg a@(HarlanEarnstone attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ parley iid source attrs SkillWillpower 4
      pure a
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
          pure a
    _ -> HarlanEarnstone <$> runMessage msg attrs

module Arkham.Asset.Cards.HenryDeveau (
  henryDeveau,
  HenryDeveau (..),
)
where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.SkillType

newtype HenryDeveau = HenryDeveau AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

henryDeveau :: AssetCard HenryDeveau
henryDeveau =
  asset HenryDeveau Cards.henryDeveau

instance HasAbilities HenryDeveau where
  getAbilities (HenryDeveau a) =
    [ restrictedAbility a 1 OnSameLocation $
        ActionAbility
          (Just Action.Parley)
          (ActionCost 1 <> DiscardFromCost 1 (FromHandOf You) AnyCard)
    ]

instance RunMessage HenryDeveau where
  runMessage msg a@(HenryDeveau attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ parley iid source attrs SkillIntellect 3
      pure a
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
          pure a
    _ -> HenryDeveau <$> runMessage msg attrs

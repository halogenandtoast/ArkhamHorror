module Arkham.Asset.Cards.MariaDeSilva (
  mariaDeSilva,
  MariaDeSilva (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.SkillType

newtype MariaDeSilva = MariaDeSilva AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mariaDeSilva :: AssetCard MariaDeSilva
mariaDeSilva = asset MariaDeSilva Cards.mariaDeSilva

instance HasAbilities MariaDeSilva where
  getAbilities (MariaDeSilva a) =
    [ restrictedAbility a 1 OnSameLocation $
        ActionAbility
          (Just Action.Parley)
          (ActionCost 1 <> ResourceCost 1)
    ]

instance RunMessage MariaDeSilva where
  runMessage msg a@(MariaDeSilva attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ parley iid source attrs SkillIntellect 3
      pure a
    PassedSkillTest _ _ source SkillTestInitiatorTarget {} _ _
      | isSource attrs source -> do
          push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
          pure a
    _ -> MariaDeSilva <$> runMessage msg attrs

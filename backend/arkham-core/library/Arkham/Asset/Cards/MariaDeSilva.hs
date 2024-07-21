module Arkham.Asset.Cards.MariaDeSilva (
  mariaDeSilva,
  MariaDeSilva (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype MariaDeSilva = MariaDeSilva AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mariaDeSilva :: AssetCard MariaDeSilva
mariaDeSilva = asset MariaDeSilva Cards.mariaDeSilva

instance HasAbilities MariaDeSilva where
  getAbilities (MariaDeSilva a) =
    [ restrictedAbility a 1 OnSameLocation
        $ ActionAbility
          [Action.Parley]
          (ActionCost 1 <> ResourceCost 1)
    ]

instance RunMessage MariaDeSilva where
  runMessage msg a@(MariaDeSilva attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 3)
      pure a
    PassedThisSkillTest _ (isAbilitySource attrs 1 -> True) -> do
      push $ PlaceClues (attrs.ability 1) (toTarget attrs) 1
      pure a
    _ -> MariaDeSilva <$> runMessage msg attrs

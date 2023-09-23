module Arkham.Asset.Cards.DarioElAmin (
  darioElAmin,
  DarioElAmin (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Projection
import Arkham.SkillType

newtype DarioElAmin = DarioElAmin AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darioElAmin :: AssetCard DarioElAmin
darioElAmin = ally DarioElAmin Cards.darioElAmin (2, 2)

instance HasModifiersFor DarioElAmin where
  getModifiersFor (InvestigatorTarget iid) (DarioElAmin attrs)
    | attrs `controlledBy` iid = do
        resources <- field InvestigatorResources iid
        pure
          $ toModifiers attrs
          $ if resources >= 10
            then [SkillModifier SkillWillpower 1, SkillModifier SkillIntellect 1]
            else []
  getModifiersFor _ _ = pure []

instance HasAbilities DarioElAmin where
  getAbilities (DarioElAmin attrs) =
    [ restrictedAbility
        attrs
        1
        (ControlsThis <> LocationExists (YourLocation <> LocationWithoutEnemies))
        $ ActionAbility Nothing
        $ ExhaustCost
        $ toTarget attrs
    ]

instance RunMessage DarioElAmin where
  runMessage msg a@(DarioElAmin attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ TakeResources iid 2 (toAbilitySource attrs 1) False
      pure a
    _ -> DarioElAmin <$> runMessage msg attrs

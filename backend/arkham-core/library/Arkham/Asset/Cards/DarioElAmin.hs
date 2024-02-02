module Arkham.Asset.Cards.DarioElAmin (darioElAmin, DarioElAmin (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Projection

newtype DarioElAmin = DarioElAmin AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks, NFData)

darioElAmin :: AssetCard DarioElAmin
darioElAmin = ally DarioElAmin Cards.darioElAmin (2, 2)

instance HasModifiersFor DarioElAmin where
  getModifiersFor (InvestigatorTarget iid) (DarioElAmin attrs) | attrs `controlledBy` iid = do
    resources <- field InvestigatorResources iid
    pure
      $ toModifiers attrs
      $ guard (resources >= 10)
      *> [SkillModifier #willpower 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities DarioElAmin where
  getAbilities (DarioElAmin attrs) =
    [ controlledAbility attrs 1 (exists $ YourLocation <> LocationWithoutEnemies)
        $ actionAbilityWithCost
        $ exhaust attrs
    ]

instance RunMessage DarioElAmin where
  runMessage msg a@(DarioElAmin attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ TakeResources iid 2 (attrs.ability 1) False
      pure a
    _ -> DarioElAmin <$> runMessage msg attrs

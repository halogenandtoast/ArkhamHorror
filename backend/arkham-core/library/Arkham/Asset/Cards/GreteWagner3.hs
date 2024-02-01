module Arkham.Asset.Cards.GreteWagner3 (
  greteWagner3,
  GreteWagner3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Discover
import Arkham.Matcher

newtype GreteWagner3 = GreteWagner3 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

greteWagner3 :: AssetCard GreteWagner3
greteWagner3 = ally GreteWagner3 Cards.greteWagner3 (4, 2)

instance HasModifiersFor GreteWagner3 where
  getModifiersFor (InvestigatorTarget iid) (GreteWagner3 a) | controlledBy a iid = do
    pure $ toModifiers a [SkillModifier #combat 1, SkillModifier #intellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GreteWagner3 where
  getAbilities (GreteWagner3 a) =
    [ controlledAbility
        a
        1
        (ClueOnLocation <> exists (You <> InvestigatorCanDiscoverCluesAt YourLocation))
        $ ReactionAbility
          (EnemyDefeated #after You ByAny AnyEnemy)
          (exhaust a <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner3 where
  runMessage msg a@(GreteWagner3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ discoverAtYourLocation iid (toAbilitySource attrs 1) 1
      pure a
    _ -> GreteWagner3 <$> runMessage msg attrs

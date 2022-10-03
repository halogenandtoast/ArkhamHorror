module Arkham.Asset.Cards.GreteWagner3
  ( greteWagner3
  , GreteWagner3(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding ( EnemyDefeated )
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype GreteWagner3 = GreteWagner3 AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner3 :: AssetCard GreteWagner3
greteWagner3 = ally GreteWagner3 Cards.greteWagner3 (4, 2)

instance HasModifiersFor GreteWagner3 where
  getModifiersFor (InvestigatorTarget iid) (GreteWagner3 a)
    | controlledBy a iid = pure $ toModifiers
      a
      [SkillModifier SkillCombat 1, SkillModifier SkillIntellect 1]
  getModifiersFor _ _ = pure []

instance HasAbilities GreteWagner3 where
  getAbilities (GreteWagner3 a) =
    [ restrictedAbility
          a
          1
          (ControlsThis <> ClueOnLocation <> InvestigatorExists
            (You <> InvestigatorCanDiscoverCluesAt YourLocation)
          )
        $ ReactionAbility
            (EnemyDefeated Timing.After You AnyEnemy)
            (ExhaustCost (toTarget a) <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner3 where
  runMessage msg a@(GreteWagner3 attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure a
    _ -> GreteWagner3 <$> runMessage msg attrs

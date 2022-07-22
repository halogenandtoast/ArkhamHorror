module Arkham.Asset.Cards.GreteWagner
  ( greteWagner
  , GreteWagner(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (EnemyDefeated)
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Target
import Arkham.Timing qualified as Timing

newtype GreteWagner = GreteWagner AssetAttrs
  deriving anyclass IsAsset
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

greteWagner :: AssetCard GreteWagner
greteWagner = ally GreteWagner Cards.greteWagner (3, 2)

instance HasModifiersFor GreteWagner where
  getModifiersFor _ (InvestigatorTarget iid) (GreteWagner a)
    | controlledBy a iid = pure $ toModifiers a [SkillModifier SkillCombat 1]
  getModifiersFor _ _ _ = pure []

instance HasAbilities GreteWagner where
  getAbilities (GreteWagner a) =
    [ restrictedAbility a 1 (ControlsThis <> ClueOnLocation) $ ReactionAbility
        (EnemyDefeated Timing.After You AnyEnemy)
        (ExhaustCost (toTarget a) <> DamageCost (toSource a) (toTarget a) 1)
    ]

instance RunMessage GreteWagner where
  runMessage msg a@(GreteWagner attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      push $ InvestigatorDiscoverCluesAtTheirLocation iid 1 Nothing
      pure a
    _ -> GreteWagner <$> runMessage msg attrs

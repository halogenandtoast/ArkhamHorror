module Arkham.Asset.Cards.FleshWard
  ( fleshWard
  , FleshWard(..)
  ) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Cost
import Arkham.Criteria
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype FleshWard = FleshWard AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fleshWard :: AssetCard FleshWard
fleshWard = asset FleshWard Cards.fleshWard

instance HasAbilities FleshWard where
  getAbilities (FleshWard a) =
    [ restrictedAbility a 1 ControlsThis $ ReactionAbility
        (OrWindowMatcher
          [ DealtDamage Timing.When (SourceIsEnemyAttack AnyEnemy) You
          , DealtHorror Timing.When (SourceIsEnemyAttack AnyEnemy) You
          ]
        )
        (ExhaustCost (toTarget a) <> UseCost (AssetWithId $ toId a) Charge 1)
    ]

instance RunMessage FleshWard where
  runMessage msg (FleshWard attrs) = FleshWard <$> runMessage msg attrs

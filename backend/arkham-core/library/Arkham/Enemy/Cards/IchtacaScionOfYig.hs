module Arkham.Enemy.Cards.IchtacaScionOfYig
  ( ichtacaScionOfYig
  , IchtacaScionOfYig(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype IchtacaScionOfYig = IchtacaScionOfYig EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

ichtacaScionOfYig :: EnemyCard IchtacaScionOfYig
ichtacaScionOfYig =
  enemy IchtacaScionOfYig Cards.ichtacaScionOfYig (4, PerPlayer 6, 4) (2, 1)

instance RunMessage IchtacaScionOfYig where
  runMessage msg (IchtacaScionOfYig attrs) =
    IchtacaScionOfYig <$> runMessage msg attrs

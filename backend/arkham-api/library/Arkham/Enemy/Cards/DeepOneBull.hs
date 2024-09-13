module Arkham.Enemy.Cards.DeepOneBull
  ( deepOneBull
  , DeepOneBull(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype DeepOneBull = DeepOneBull EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

deepOneBull :: EnemyCard DeepOneBull
deepOneBull = enemy DeepOneBull Cards.deepOneBull (4, Static 5, 2) (2, 0)

instance RunMessage DeepOneBull where
  runMessage msg (DeepOneBull attrs) =
    DeepOneBull <$> runMessage msg attrs

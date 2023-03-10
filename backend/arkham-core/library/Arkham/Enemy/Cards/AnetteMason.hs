module Arkham.Enemy.Cards.AnetteMason
  ( anetteMason
  , AnetteMason(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype AnetteMason = AnetteMason EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

anetteMason :: EnemyCard AnetteMason
anetteMason = enemy AnetteMason Cards.anetteMason (4, PerPlayer 4, 4) (1, 1)

instance RunMessage AnetteMason where
  runMessage msg (AnetteMason attrs) =
    AnetteMason <$> runMessage msg attrs

module Arkham.Enemy.Cards.BoaConstrictor
  ( boaConstrictor
  , BoaConstrictor(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Enemy.Cards as Cards
import Arkham.Classes
import Arkham.Enemy.Runner

newtype BoaConstrictor = BoaConstrictor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

boaConstrictor :: EnemyCard BoaConstrictor
boaConstrictor = enemy BoaConstrictor Cards.boaConstrictor (4, Static 4, 2) (1, 1)

instance RunMessage BoaConstrictor where
  runMessage msg (BoaConstrictor attrs) =
    BoaConstrictor <$> runMessage msg attrs

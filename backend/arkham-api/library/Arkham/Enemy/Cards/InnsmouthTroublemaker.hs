module Arkham.Enemy.Cards.InnsmouthTroublemaker
  ( innsmouthTroublemaker
  , InnsmouthTroublemaker(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype InnsmouthTroublemaker = InnsmouthTroublemaker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

innsmouthTroublemaker :: EnemyCard InnsmouthTroublemaker
innsmouthTroublemaker = enemy InnsmouthTroublemaker Cards.innsmouthTroublemaker (4, Static 3, 2) (2, 0)

instance RunMessage InnsmouthTroublemaker where
  runMessage msg (InnsmouthTroublemaker attrs) =
    InnsmouthTroublemaker <$> runMessage msg attrs

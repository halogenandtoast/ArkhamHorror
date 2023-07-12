module Arkham.Enemy.Cards.JeromeDavids
  ( jeromeDavids
  , JeromeDavids(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype JeromeDavids = JeromeDavids EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

jeromeDavids :: EnemyCard JeromeDavids
jeromeDavids = enemy JeromeDavids Cards.jeromeDavids (4, Static 4, 4) (1, 1)

instance RunMessage JeromeDavids where
  runMessage msg (JeromeDavids attrs) =
    JeromeDavids <$> runMessage msg attrs

module Arkham.Enemy.Cards.CultistOfTheEnclave
  ( cultistOfTheEnclave
  , CultistOfTheEnclave(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype CultistOfTheEnclave = CultistOfTheEnclave EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

cultistOfTheEnclave :: EnemyCard CultistOfTheEnclave
cultistOfTheEnclave = enemy CultistOfTheEnclave Cards.cultistOfTheEnclave (3, Static 2, 3) (1, 0)

instance RunMessage CultistOfTheEnclave where
  runMessage msg (CultistOfTheEnclave attrs) =
    CultistOfTheEnclave <$> runMessage msg attrs

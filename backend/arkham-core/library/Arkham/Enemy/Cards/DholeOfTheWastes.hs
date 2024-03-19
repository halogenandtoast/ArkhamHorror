module Arkham.Enemy.Cards.DholeOfTheWastes
  ( dholeOfTheWastes
  , DholeOfTheWastes(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype DholeOfTheWastes = DholeOfTheWastes EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dholeOfTheWastes :: EnemyCard DholeOfTheWastes
dholeOfTheWastes = enemy DholeOfTheWastes Cards.dholeOfTheWastes (6, Static 6, 2) (2, 1)

instance RunMessage DholeOfTheWastes where
  runMessage msg (DholeOfTheWastes attrs) =
    DholeOfTheWastes <$> runMessage msg attrs

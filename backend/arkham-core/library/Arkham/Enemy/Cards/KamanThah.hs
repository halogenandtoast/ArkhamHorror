module Arkham.Enemy.Cards.KamanThah
  ( kamanThah
  , KamanThah(..)
  )
where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner

newtype KamanThah = KamanThah EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

kamanThah :: EnemyCard KamanThah
kamanThah = enemy KamanThah Cards.kamanThah (2, Static 3, 2) (1, 0)

instance RunMessage KamanThah where
  runMessage msg (KamanThah attrs) =
    KamanThah <$> runMessage msg attrs

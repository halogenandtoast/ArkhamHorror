module Arkham.Enemy.Cards.RookieCop (rookieCop, RookieCop(..)) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword qualified as Keyword
import Arkham.Trait

newtype RookieCop = RookieCop EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rookieCop :: EnemyCard RookieCop
rookieCop = enemy RookieCop Cards.rookieCop (2, Static 2, 2) (1, 1)

instance HasModifiersFor RookieCop where
  getModifiersFor (RookieCop a) =
    modifySelf a [AddKeyword Keyword.Aloof, AddKeyword Keyword.Hunter]

instance HasAbilities RookieCop where
  getAbilities = enemyAbilities

instance RunMessage RookieCop where
  runMessage msg (RookieCop attrs) = RookieCop <$> runMessage msg attrs

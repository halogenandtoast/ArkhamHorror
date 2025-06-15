module Arkham.Enemy.Cards.EnragedGug (
  enragedGug,
  EnragedGug (..),
) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword
import Arkham.Asset.Cards qualified as Assets
import Arkham.Strategy

newtype EnragedGug = EnragedGug EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

enragedGug :: EnemyCard EnragedGug
enragedGug = enemy EnragedGug Cards.enragedGug (3, Static 3, 2) (1, 1)

instance HasModifiersFor EnragedGug where
  getModifiersFor (EnragedGug a) =
    modifySelf a
      [ SetAttackDamageStrategy (DamageFirst Assets.partyGuest.cardDef)
      , AddKeyword Alert
      , AddKeyword Hunter
      ]

instance HasAbilities EnragedGug where
  getAbilities = enemyAbilities

instance RunMessage EnragedGug where
  runMessage msg (EnragedGug attrs) =
    EnragedGug <$> runMessage msg attrs

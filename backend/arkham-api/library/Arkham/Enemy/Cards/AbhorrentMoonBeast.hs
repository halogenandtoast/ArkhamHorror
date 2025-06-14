module Arkham.Enemy.Cards.AbhorrentMoonBeast (
  abhorrentMoonBeast,
  AbhorrentMoonBeast(..),
) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Keyword
import Arkham.Asset.Cards qualified as Assets
import Arkham.Strategy

newtype AbhorrentMoonBeast = AbhorrentMoonBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

abhorrentMoonBeast :: EnemyCard AbhorrentMoonBeast
abhorrentMoonBeast = enemy AbhorrentMoonBeast Cards.abhorrentMoonBeast (2, Static 3, 4) (1, 1)

instance HasModifiersFor AbhorrentMoonBeast where
  getModifiersFor (AbhorrentMoonBeast a) =
    modifySelf a
      [ SetAttackDamageStrategy (DamageFirst Assets.partyGuest.cardDef)
      , AddKeyword Hunter
      , AddKeyword Retaliate
      ]

instance HasAbilities AbhorrentMoonBeast where
  getAbilities = enemyAbilities

instance RunMessage AbhorrentMoonBeast where
  runMessage msg (AbhorrentMoonBeast attrs) =
    AbhorrentMoonBeast <$> runMessage msg attrs

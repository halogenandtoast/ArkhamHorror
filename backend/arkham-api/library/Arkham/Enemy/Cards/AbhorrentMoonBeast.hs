module Arkham.Enemy.Cards.AbhorrentMoonBeast (abhorrentMoonBeast) where

import Arkham.Asset.Cards qualified as Assets
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Keyword
import Arkham.Strategy

newtype AbhorrentMoonBeast = AbhorrentMoonBeast EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

abhorrentMoonBeast :: EnemyCard AbhorrentMoonBeast
abhorrentMoonBeast = enemy AbhorrentMoonBeast Cards.abhorrentMoonBeast (2, Static 3, 4) (1, 1)

instance HasModifiersFor AbhorrentMoonBeast where
  getModifiersFor (AbhorrentMoonBeast a) =
    modifySelf
      a
      [ SetAttackDamageStrategy (DamageFirst Assets.partyGuest)
      , AddKeyword Hunter
      , AddKeyword Retaliate
      ]

instance RunMessage AbhorrentMoonBeast where
  runMessage msg (AbhorrentMoonBeast attrs) =
    AbhorrentMoonBeast <$> runMessage msg attrs

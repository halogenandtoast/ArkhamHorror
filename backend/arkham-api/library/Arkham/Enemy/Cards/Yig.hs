module Arkham.Enemy.Cards.Yig (yig, Yig (..)) where

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Trait (Trait (Serpent))

newtype Yig = Yig EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yig :: EnemyCard Yig
yig = enemy Yig Cards.yig (4, Static 6, 4) (3, 3)

instance HasModifiersFor Yig where
  getModifiersFor (Yig a) = do
    n <- perPlayer 6
    cannotBeDamaged <-
      selectAny $ ReadyEnemy <> withTrait Serpent <> at_ (locationWithEnemy a) <> not_ (be a)
    self <- modifySelf a $ HealthModifier n : [CannotBeDamaged | cannotBeDamaged]
    enemies <-
      modifySelect
        a
        (ReadyEnemy <> EnemyWithTrait Serpent <> at_ (locationWithEnemy a))
        [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]
    pure $ self <> enemies

instance RunMessage Yig where
  runMessage msg (Yig attrs) = Yig <$> runMessage msg attrs

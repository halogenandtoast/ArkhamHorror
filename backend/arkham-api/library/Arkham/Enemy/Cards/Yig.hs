module Arkham.Enemy.Cards.Yig (yig) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
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
    modifySelf a $ HealthModifier n : [CannotBeDamaged | cannotBeDamaged]
    modifySelect
      a
      (ReadyEnemy <> withTrait Serpent <> at_ (locationWithEnemy a) <> not_ (be a))
      [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]

instance RunMessage Yig where
  runMessage msg (Yig attrs) = Yig <$> runMessage msg attrs

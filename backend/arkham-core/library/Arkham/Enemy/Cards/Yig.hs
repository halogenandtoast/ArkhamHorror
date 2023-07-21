module Arkham.Enemy.Cards.Yig (
  yig,
  Yig (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Keyword qualified as Keyword
import Arkham.Matcher
import Arkham.Trait (Trait (Serpent))

newtype Yig = Yig EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

yig :: EnemyCard Yig
yig = enemy Yig Cards.yig (4, Static 6, 4) (3, 3)

instance HasModifiersFor Yig where
  getModifiersFor target (Yig a) | isTarget a target = do
    n <- getPlayerCountValue (PerPlayer 6)
    cannotBeDamaged <-
      selectAny $
        ReadyEnemy
          <> EnemyWithTrait Serpent
          <> EnemyAt (LocationWithEnemy $ EnemyWithId $ toId a)
          <> NotEnemy (EnemyWithId $ toId a)
    pure $
      toModifiers a $
        [HealthModifier n]
          <> [CannotBeDamaged | cannotBeDamaged]
  getModifiersFor (EnemyTarget eid) (Yig a) = do
    isModified <-
      eid
        <=~> ( ReadyEnemy
                <> EnemyWithTrait Serpent
                <> EnemyAt
                  (LocationWithEnemy $ EnemyWithId $ toId a)
             )
    pure $ toModifiers a $ do
      guard isModified
      [AddKeyword Keyword.Alert, AddKeyword Keyword.Retaliate]
  getModifiersFor _ _ = pure []

instance RunMessage Yig where
  runMessage msg (Yig attrs) = Yig <$> runMessage msg attrs

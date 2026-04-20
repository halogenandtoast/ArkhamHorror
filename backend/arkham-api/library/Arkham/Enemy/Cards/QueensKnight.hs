module Arkham.Enemy.Cards.QueensKnight (queensKnight) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Matcher

newtype QueensKnight = QueensKnight EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

queensKnight :: EnemyCard QueensKnight
queensKnight = enemy QueensKnight Cards.queensKnight (2, Static 5, 2) (2, 0)

instance HasAbilities QueensKnight where
  getAbilities (QueensKnight attrs) =
    withBaseAbilities
      attrs
      [ mkAbility attrs 1
          $ ForcedAbility
          $ EnemyTakeDamage #when AnyDamageEffect (EnemyWithId $ toId attrs) AnyValue AnySource
      ]

instance RunMessage QueensKnight where
  runMessage msg e@(QueensKnight attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      -- Find Elokoss and deal 1 damage
      elokoss <- selectOne $ EnemyWithTitle "Elokoss"
      for_ elokoss \eid' -> do
        nonAttackEnemyDamage Nothing (attrs.ability 1) 1 eid'
      pure e
    _ -> QueensKnight <$> liftRunMessage msg attrs

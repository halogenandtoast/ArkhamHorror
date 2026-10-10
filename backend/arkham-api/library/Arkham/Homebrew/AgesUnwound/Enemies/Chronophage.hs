module Arkham.Homebrew.AgesUnwound.Enemies.Chronophage (chronophage) where

import Arkham.Ability
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Homebrew.AgesUnwound.CardDefs.Enemies qualified as Cards
import Arkham.Matcher

newtype Chronophage = Chronophage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chronophage :: EnemyCard Chronophage
chronophage = enemy Chronophage Cards.chronophage

-- | "Forced - After Chronophage attacks you: Lose an action."
instance HasAbilities Chronophage where
  getAbilities (Chronophage a) =
    extend1 a $ mkAbility a 1 $ forced $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage Chronophage where
  runMessage msg e@(Chronophage attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      loseStandardActions iid (attrs.ability 1) 1
      pure e
    _ -> Chronophage <$> liftRunMessage msg attrs

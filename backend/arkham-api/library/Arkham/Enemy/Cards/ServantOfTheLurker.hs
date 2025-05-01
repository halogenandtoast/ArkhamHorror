module Arkham.Enemy.Cards.ServantOfTheLurker (servantOfTheLurker) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker =
  enemy ServantOfTheLurker Cards.servantOfTheLurker (4, Static 5, 2) (2, 2)
    & setPrey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities ServantOfTheLurker where
  getAbilities (ServantOfTheLurker x) =
    extend x [mkAbility x 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be x)]

instance RunMessage ServantOfTheLurker where
  runMessage msg e@(ServantOfTheLurker attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeck iid (attrs.ability 1) 2
      pure e
    _ -> ServantOfTheLurker <$> liftRunMessage msg attrs

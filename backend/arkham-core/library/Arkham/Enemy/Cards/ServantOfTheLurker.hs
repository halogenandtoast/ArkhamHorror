module Arkham.Enemy.Cards.ServantOfTheLurker (servantOfTheLurker, ServantOfTheLurker (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype ServantOfTheLurker = ServantOfTheLurker EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfTheLurker :: EnemyCard ServantOfTheLurker
servantOfTheLurker =
  enemyWith ServantOfTheLurker Cards.servantOfTheLurker (4, Static 5, 2) (2, 2)
    $ preyL
    .~ Prey (InvestigatorWithLowestSkill #agility UneliminatedInvestigator)

instance HasAbilities ServantOfTheLurker where
  getAbilities (ServantOfTheLurker x) =
    extend x [mkAbility x 1 $ forced $ EnemyAttacks #when You AnyEnemyAttack (be x)]

instance RunMessage ServantOfTheLurker where
  runMessage msg e@(ServantOfTheLurker attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ DiscardTopOfDeck iid 2 (attrs.ability 1) Nothing
      pure e
    _ -> ServantOfTheLurker <$> runMessage msg attrs

module Arkham.Enemy.Cards.ElderThingScavenger (elderThingScavenger) where

import Arkham.Ability
import Arkham.Card
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ElderThingScavenger = ElderThingScavenger EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

elderThingScavenger :: EnemyCard ElderThingScavenger
elderThingScavenger = enemy ElderThingScavenger Cards.elderThingScavenger (3, Static 3, 3) (1, 1)

instance HasAbilities ElderThingScavenger where
  getAbilities (ElderThingScavenger a) =
    extend1 a
      $ forcedAbility a 1
      $ EnemyAttacks #when You AnyEnemyAttack (be a)

instance RunMessage ElderThingScavenger where
  runMessage msg e@(ElderThingScavenger attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      discardTopOfDeckAndHandle iid (attrs.ability 1) 3 attrs
      pure e
    DiscardedTopOfDeck iid cards (isAbilitySource attrs 1 -> True) (isTarget attrs -> True) -> do
      let weaknesses = filterCards WeaknessCard cards
      focusCards_ weaknesses $ chooseOneAtATimeM iid $ targets weaknesses $ drawCard iid
      pure e
    _ -> ElderThingScavenger <$> liftRunMessage msg attrs

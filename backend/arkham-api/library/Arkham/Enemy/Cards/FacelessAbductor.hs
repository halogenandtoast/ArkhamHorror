module Arkham.Enemy.Cards.FacelessAbductor (facelessAbductor) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move
import Arkham.Trait (Trait (Prison))

newtype FacelessAbductor = FacelessAbductor EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

facelessAbductor :: EnemyCard FacelessAbductor
facelessAbductor =
  enemyWith FacelessAbductor Cards.facelessAbductor
    $ spawnAtL
    ?~ SpawnAt (NearestLocationToYou $ LocationWithTrait Prison)

instance HasAbilities FacelessAbductor where
  getAbilities (FacelessAbductor a) =
    extend1 a $ forcedAbility a 1 $ EnemyAttacks #after You AnyEnemyAttack (be a)

instance RunMessage FacelessAbductor where
  runMessage msg e@(FacelessAbductor attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      prisons <- select $ LocationWithTrait Prison
      chooseOrRunOneM iid do
        targets prisons \lid -> moveTo (attrs.ability 1) iid lid
      pure e
    _ -> FacelessAbductor <$> liftRunMessage msg attrs

module Arkham.Enemy.Cards.ServantOfManyMouths (servantOfManyMouths) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyDefeated)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose

newtype ServantOfManyMouths = ServantOfManyMouths EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

servantOfManyMouths :: EnemyCard ServantOfManyMouths
servantOfManyMouths =
  enemy ServantOfManyMouths Cards.servantOfManyMouths (3, Static 2, 1) (2, 0)
    & setSpawnAt EmptyLocation

instance HasAbilities ServantOfManyMouths where
  getAbilities (ServantOfManyMouths a) =
    extend1 a
      $ restricted a 1 (exists $ LocationWithDiscoverableCluesBy You)
      $ freeReaction
      $ EnemyDefeated #after You ByAny (be a)

instance RunMessage ServantOfManyMouths where
  runMessage msg e@(ServantOfManyMouths attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      locationsWithClues <- select $ locationWithDiscoverableCluesBy iid
      chooseTargetM iid locationsWithClues \lid ->
        discoverAt NotInvestigate iid (attrs.ability 1) lid 1
      pure e
    _ -> ServantOfManyMouths <$> liftRunMessage msg attrs

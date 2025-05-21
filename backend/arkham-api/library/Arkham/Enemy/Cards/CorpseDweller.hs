module Arkham.Enemy.Cards.CorpseDweller (corpseDweller) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait

newtype CorpseDweller = CorpseDweller EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

corpseDweller :: EnemyCard CorpseDweller
corpseDweller =
  enemyWith CorpseDweller Cards.corpseDweller (3, Static 5, 4) (2, 1)
    $ ( spawnAtL
          ?~ SpawnAt
            ( LocationWithEnemy
                (EnemyWithTrait Humanoid <> EnemyIfReturnTo (not_ (enemyIs Cards.theManInThePallidMask)) AnyEnemy)
            )
      )
    . (surgeIfUnableToSpawnL .~ True)

instance RunMessage CorpseDweller where
  runMessage msg (CorpseDweller attrs) = runQueueT $ case msg of
    EnemySpawn details | details.enemy == attrs.id && not details.overridden -> do
      for_ details.location \lid -> do
        iid <- maybe getLead pure details.investigator
        humanoids <-
          select
            $ EnemyWithTrait Humanoid
            <> EnemyIfReturnTo (not_ (enemyIs Cards.theManInThePallidMask)) AnyEnemy
            <> enemyAt lid
        chooseOneM iid $ targets humanoids (toDiscard attrs)
      CorpseDweller <$> liftRunMessage msg attrs
    _ -> CorpseDweller <$> liftRunMessage msg attrs

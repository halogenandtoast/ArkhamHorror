module Arkham.Enemy.Cards.DimensionalDisplacerA (dimensionalDisplacerA) where

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted
import Arkham.Helpers.Query (getLead)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Trait (Trait (Casino))

newtype DimensionalDisplacerA = DimensionalDisplacerA EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, HasAbilities)

dimensionalDisplacerA :: EnemyCard DimensionalDisplacerA
dimensionalDisplacerA =
  enemyWith DimensionalDisplacerA Cards.dimensionalDisplacerA (5, Static 2, 3) (0, 2)
    $ ( spawnAtL
          ?~ SpawnAt
            ( NearestLocationToYou $ LocationWithEnemy
                (EnemyWithTrait Casino <> not_ UniqueEnemy <> EnemyWithoutAttachedEncounterCard)
            )
      )
    . (surgeIfUnableToSpawnL .~ True)

instance RunMessage DimensionalDisplacerA where
  runMessage msg (DimensionalDisplacerA attrs) = runQueueT $ case msg of
    EnemySpawn details | details.enemy == attrs.id && not details.overridden -> do
      for_ details.location \lid -> do
        iid <- maybe getLead pure details.investigator
        casino <-
          select
            $ EnemyWithTrait Casino
            <> not_ UniqueEnemy
            <> EnemyWithoutAttachedEncounterCard
            <> enemyAt lid
        chooseOneM iid $ targets casino (toDiscard attrs)
      DimensionalDisplacerA <$> liftRunMessage msg attrs
    _ -> DimensionalDisplacerA <$> liftRunMessage msg attrs

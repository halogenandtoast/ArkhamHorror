module Arkham.Enemy.Cards.MalformedSkeleton (malformedSkeleton) where

import Arkham.Ability
import Arkham.Enemy.Cards qualified as Cards
import Arkham.Enemy.Import.Lifted hiding (EnemyAttacks)
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Modifiers (ModifierType (..), modifySelf)
import Arkham.Helpers.Query
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Message.Lifted.Move

newtype MalformedSkeleton = MalformedSkeleton EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

malformedSkeleton :: EnemyCard MalformedSkeleton
malformedSkeleton = enemy MalformedSkeleton Cards.malformedSkeleton (4, Static 4, 1) (3, 3)

instance HasModifiersFor MalformedSkeleton where
  getModifiersFor (MalformedSkeleton a) = modifySelf a [AttackDealsEitherDamageOrHorror]

instance HasAbilities MalformedSkeleton where
  getAbilities (MalformedSkeleton x) =
    extend
      x
      [ restricted
          x
          1
          ( notExists (InvestigatorAt $ LocationWithDistanceFromAtMost 2 (locationWithEnemy x) Anywhere)
              <> exists (location_ "Catacombs")
          )
          $ forced
          $ WouldMoveFromHunter #when (be x)
      , mkAbility x 2
          $ forced
          $ EnemyAttacks #when You AnyEnemyAttack (be x)
      ]

instance RunMessage MalformedSkeleton where
  runMessage msg e@(MalformedSkeleton attrs) = runQueueT $ case msg of
    UseThisAbility _ (isSource attrs -> True) 1 -> do
      traverse_ cancelBatch =<< getCurrentBatchId
      lead <- getLead
      chooseSelectM lead (NearestLocationToAny "Catacombs") (enemyMoveTo attrs)
      pure e
    UseThisAbility _ (isSource attrs -> True) 2 -> do
      pure e
    _ -> MalformedSkeleton <$> liftRunMessage msg attrs

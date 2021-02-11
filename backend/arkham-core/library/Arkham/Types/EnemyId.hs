module Arkham.Types.EnemyId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype EnemyId = EnemyId { unEnemyId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype StoryEnemyId = StoryEnemyId { unStoryEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ClosestEnemyId = ClosestEnemyId { unClosestEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype FarthestEnemyId = FarthestEnemyId { unFarthestEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype UniqueEnemyId = UniqueEnemyId { unUniqueEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype AloofEnemyId = AloofEnemyId { unAloofEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype UnengagedEnemyId = UnengagedEnemyId { unUnengagedEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ExhaustedEnemyId = ExhaustedEnemyId { unExhaustedEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype FightableEnemyId = FightableEnemyId { unFightableEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

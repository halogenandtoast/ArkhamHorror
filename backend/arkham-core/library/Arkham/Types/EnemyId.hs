module Arkham.Types.EnemyId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype EnemyId = EnemyId { unEnemyId :: CardId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Random)

newtype StoryEnemyId = StoryEnemyId { unStoryEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ClosestEnemyId = ClosestEnemyId { unClosestEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype FarthestEnemyId = FarthestEnemyId { unFarthestEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UniqueEnemyId = UniqueEnemyId { unUniqueEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype AloofEnemyId = AloofEnemyId { unAloofEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype UnengagedEnemyId = UnengagedEnemyId { unUnengagedEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype ExhaustedEnemyId = ExhaustedEnemyId { unExhaustedEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

newtype FightableEnemyId = FightableEnemyId { unFightableEnemyId :: EnemyId }
  deriving newtype (Ord, Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

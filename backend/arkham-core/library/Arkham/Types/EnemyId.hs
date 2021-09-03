module Arkham.Types.EnemyId where

import Arkham.Prelude

import Arkham.Types.Card.Id

newtype EnemyId = EnemyId { unEnemyId :: CardId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable, Random)

newtype ClosestEnemyId = ClosestEnemyId { unClosestEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype FarthestEnemyId = FarthestEnemyId { unFarthestEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype FightableEnemyId = FightableEnemyId { unFightableEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

module Arkham.Types.EnemyId where

import ClassyPrelude
import Data.Aeson
import Data.UUID

newtype EnemyId = EnemyId { unEnemyId :: UUID }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ClosestEnemyId = ClosestEnemyId { unClosestEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype AloofEnemyId = AloofEnemyId { unAloofEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype UnengagedEnemyId = UnengagedEnemyId { unUnengagedEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

newtype ExhaustedEnemyId = ExhaustedEnemyId { unExhaustedEnemyId :: EnemyId }
  deriving newtype (Show, Eq, ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

module Arkham.Message.Type where

import Arkham.Prelude

data MessageType
  = RevelationMessage
  | AttackMessage
  | DrawChaosTokenMessage
  | RevealChaosTokenMessage
  | ResolveChaosTokenMessage
  | CheckWindowMessage
  | EnemySpawnMessage
  | DrawEnemyMessage
  | EnemyDefeatedMessage
  | InvestigatorDefeatedMessage
  | DamageMessage
  | AssetDamageMessage
  | DrawEncounterCardMessage
  | ExploreMessage
  deriving stock (Ord, Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

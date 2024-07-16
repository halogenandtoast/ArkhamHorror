module Arkham.Message.Type where

import Arkham.Prelude

data MessageType
  = RevelationMessage
  | AttackMessage
  | DrawChaosTokenMessage
  | RevealChaosTokenMessage
  | ResolveChaosTokenMessage
  | RunWindowMessage
  | EnemySpawnMessage
  | DrawEnemyMessage
  | EnemyDefeatedMessage
  | InvestigatorDefeatedMessage
  | DamageMessage
  | DrawEncounterCardMessage
  | ExploreMessage
  deriving stock (Eq, Show, Generic, Data)
  deriving anyclass (ToJSON, FromJSON)

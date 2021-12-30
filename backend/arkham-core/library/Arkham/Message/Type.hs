module Arkham.Message.Type where

import Arkham.Prelude

data MessageType
    = RevelationMessage
    | AttackMessage
    | DrawTokenMessage
    | RevealTokenMessage
    | ResolveTokenMessage
    | RunWindowMessage
    | EnemySpawnMessage
    | EnemyDefeatedMessage
    | DamageMessage
    | DrawEncounterCardMessage
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

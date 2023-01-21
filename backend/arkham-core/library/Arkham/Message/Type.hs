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
    | DrawEnemyMessage
    | EnemyDefeatedMessage
    | InvestigatorDefeatedMessage
    | DamageMessage
    | DrawEncounterCardMessage
    | ExploreMessage
    deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

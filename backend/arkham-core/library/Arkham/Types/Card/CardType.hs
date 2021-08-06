module Arkham.Types.Card.CardType where

import Arkham.Prelude

data CardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  | PlayerEnemyType
  | TreacheryType
  | EnemyType
  | LocationType
  | EncounterAssetType
  | ActType
  | AgendaType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

encounterCardTypes :: [CardType]
encounterCardTypes =
  [TreacheryType, EnemyType, LocationType, EncounterAssetType]

playerCardTypes :: [CardType]
playerCardTypes =
  [AssetType, EventType, SkillType, PlayerTreacheryType, PlayerEnemyType]

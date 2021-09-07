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
  | StoryType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

encounterCardTypes :: [CardType]
encounterCardTypes =
  [TreacheryType, EnemyType, LocationType, EncounterAssetType]

playerCardTypes :: [CardType]
playerCardTypes =
  [AssetType, EventType, SkillType, PlayerTreacheryType, PlayerEnemyType]

module Arkham.Card.CardType where

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
  | InvestigatorType
  | ScenarioType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

data CardSubType = Weakness | BasicWeakness
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

encounterCardTypes :: [CardType]
encounterCardTypes =
  [ TreacheryType
  , EnemyType
  , LocationType
  , EncounterAssetType
  , StoryType
  , ActType
  , AgendaType
  ]

playerCardTypes :: [CardType]
playerCardTypes =
  [AssetType, EventType, SkillType, PlayerTreacheryType, PlayerEnemyType]

module Arkham.Card.CardType where

import Arkham.Prelude

data CardType
  = AssetType
  | EventType
  | SkillType
  | TreacheryType
  | EnemyType
  | LocationType
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

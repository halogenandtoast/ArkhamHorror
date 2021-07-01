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
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

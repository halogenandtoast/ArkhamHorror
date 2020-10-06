module Arkham.Types.Card.PlayerCard.Type where

import ClassyPrelude

import Arkham.Json

data PlayerCardType
  = AssetType
  | EventType
  | SkillType
  | PlayerTreacheryType
  | PlayerEnemyType
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)


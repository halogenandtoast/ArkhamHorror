{-# LANGUAGE TemplateHaskell #-}
module Arkham.Card.CardType where

import Arkham.Prelude

import Data.Aeson.TH

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
  deriving stock (Eq, Show, Ord)

data CardSubType = Weakness | BasicWeakness
  deriving stock (Eq, Show, Ord)

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

$(deriveJSON defaultOptions ''CardSubType)
$(deriveJSON defaultOptions ''CardType)

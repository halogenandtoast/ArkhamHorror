{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.CardType where

import Arkham.Prelude

import Control.Monad (fail)
import Data.Aeson.TH
import GHC.Generics

data SCardType a where
  SActType :: SCardType 'ActType
  SAgendaType :: SCardType 'AgendaType
  SAssetType :: SCardType 'AssetType
  SEnemyType :: SCardType 'EnemyType
  SEventType :: SCardType 'EventType
  SInvestigatorType :: SCardType 'InvestigatorType
  SLocationType :: SCardType 'LocationType
  SScenarioType :: SCardType 'ScenarioType
  SSkillType :: SCardType 'SkillType
  SStoryType :: SCardType 'StoryType
  STreacheryType :: SCardType 'TreacheryType

deriving stock instance Show (SCardType a)
deriving stock instance Eq (SCardType a)
deriving stock instance Ord (SCardType a)

data SomeSCardType where
  SomeSCardType :: SCardType a -> SomeSCardType

instance FromJSON SomeSCardType where
  parseJSON = withText "SomeSCardType" $ \case
    "SAssetType" -> pure $ SomeSCardType SAssetType
    "SEventType" -> pure $ SomeSCardType SEventType
    "SSkillType" -> pure $ SomeSCardType SSkillType
    "STreacheryType" -> pure $ SomeSCardType STreacheryType
    "SEnemyType" -> pure $ SomeSCardType SEnemyType
    "SLocationType" -> pure $ SomeSCardType SLocationType
    "SActType" -> pure $ SomeSCardType SActType
    "SAgendaType" -> pure $ SomeSCardType SAgendaType
    "SStoryType" -> pure $ SomeSCardType SStoryType
    "SInvestigatorType" -> pure $ SomeSCardType SInvestigatorType
    "SScenarioType" -> pure $ SomeSCardType SScenarioType
    _ -> fail "Unknown SCardType"

instance Generic (SCardType 'AssetType) where
  type Rep (SCardType 'AssetType) = Rep ()
  from _ = from ()
  to = const SAssetType

instance Generic (SCardType 'EventType) where
  type Rep (SCardType 'EventType) = Rep ()
  from _ = from ()
  to = const SEventType

instance Generic (SCardType 'SkillType) where
  type Rep (SCardType 'SkillType) = Rep ()
  from _ = from ()
  to = const SSkillType

instance Generic (SCardType 'TreacheryType) where
  type Rep (SCardType 'TreacheryType) = Rep ()
  from _ = from ()
  to = const STreacheryType

instance Generic (SCardType 'EnemyType) where
  type Rep (SCardType 'EnemyType) = Rep ()
  from _ = from ()
  to = const SEnemyType

instance Generic (SCardType 'LocationType) where
  type Rep (SCardType 'LocationType) = Rep ()
  from _ = from ()
  to = const SLocationType

instance Generic (SCardType 'ActType) where
  type Rep (SCardType 'ActType) = Rep ()
  from _ = from ()
  to = const SActType

instance Generic (SCardType 'AgendaType) where
  type Rep (SCardType 'AgendaType) = Rep ()
  from _ = from ()
  to = const SAgendaType

instance Generic (SCardType 'StoryType) where
  type Rep (SCardType 'StoryType) = Rep ()
  from _ = from ()
  to = const SStoryType

instance Generic (SCardType 'InvestigatorType) where
  type Rep (SCardType 'InvestigatorType) = Rep ()
  from _ = from ()
  to = const SInvestigatorType

instance Generic (SCardType 'ScenarioType) where
  type Rep (SCardType 'ScenarioType) = Rep ()
  from _ = from ()
  to = const SScenarioType

instance
  ( GToJSON Zero (Rep (SCardType a))
  , GToJSON' Encoding Zero (Rep (SCardType a))
  , Generic (SCardType a)
  )
  => ToJSON (SCardType a)
  where
  toJSON = genericToJSON defaultOptions
  toEncoding = genericToEncoding defaultOptions

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
  | EncounterEventType
  | ActType
  | AgendaType
  | StoryType
  | InvestigatorType
  | ScenarioType
  deriving stock (Eq, Show, Ord, Data)

data CardSubType = Weakness | BasicWeakness
  deriving stock (Eq, Show, Ord, Data)

encounterCardTypes :: [CardType]
encounterCardTypes =
  [ TreacheryType
  , EnemyType
  , LocationType
  , EncounterAssetType
  , EncounterEventType
  , StoryType
  , ActType
  , AgendaType
  ]

playerCardTypes :: [CardType]
playerCardTypes =
  [AssetType, EventType, SkillType, PlayerTreacheryType, PlayerEnemyType]

$(deriveJSON defaultOptions ''CardSubType)
$(deriveJSON defaultOptions ''CardType)

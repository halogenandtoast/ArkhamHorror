module Arkham.Types.Card where

import Data.Text
import GHC.Generics
import Json
import Prelude (Show)

newtype ArkhamCardFront = ArkhamCardFront { arkhamCardFrontUrl :: Text }
  deriving stock (Show,Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCardFront") ArkhamCardFront

newtype ArkhamCardBack = ArkhamCardBack { arkhamCardBackUrl :: Text }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCardBack") ArkhamCardBack

data ArkhamCard = ArkhamCard
  { arkhamCardFront :: ArkhamCardFront
  , arkhamCardBack :: ArkhamCardBack
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamCard") ArkhamCard

data ArkhamAgendaCard
  = ArkhamAgendaCardSideA ArkhamAgendaCardSideAData
  | ArkhamAgendaCardSideB ArkhamAgendaCardSideBData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "agendaCard" ArkhamAgendaCard

data ArkhamAgendaCardSideAData = ArkhamAgendaCardSideAData
  { arkhamAgendaCardSideADataName :: Text
  , arkhamAgendaCardSideADataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgendaCardSideAData") ArkhamAgendaCardSideAData

data ArkhamAgendaCardSideBData = ArkhamAgendaCardSideBData
  { arkhamAgendaCardSideBDataName :: Text
  , arkhamAgendaCardSideBDataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgendaCardSideBData") ArkhamAgendaCardSideBData

data ArkhamActCard
  = ArkhamActCardSideA ArkhamActCardSideAData
  | ArkhamActCardSideB ArkhamActCardSideBData
  deriving stock (Generic, Show)
  deriving (FromJSON, ToJSON) via TaggedJson "actCard" ArkhamActCard

data ArkhamActCardSideAData = ArkhamActCardSideAData
  { arkhamActCardSideADataName :: Text
  , arkhamActCardSideADataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamActCardSideAData") ArkhamActCardSideAData

data ArkhamActCardSideBData = ArkhamActCardSideBData
  { arkhamActCardSideBDataName :: Text
  , arkhamActCardSideBDataImageUrl :: Text
  }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamActCardSideBData") ArkhamActCardSideBData

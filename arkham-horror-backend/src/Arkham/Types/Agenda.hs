module Arkham.Types.Agenda where

import Arkham.Types.Card
import GHC.Generics
import Json
import Prelude (Show)

newtype ArkhamAgenda = ArkhamAgenda { arkhamAgendaCurrentCard :: ArkhamAgendaCard }
  deriving stock (Show, Generic)
  deriving (FromJSON, ToJSON) via Codec (Drop "arkhamAgenda") ArkhamAgenda

module Arkham.Types.Agenda
  ( ArkhamAgenda(..)
  )
where

import Arkham.Types.Card
import ClassyPrelude
import Json

data ArkhamAgenda = ArkhamAgenda { aagendaCardCode :: ArkhamCardCode, aagendaImage :: Text, aagendaImageBack :: Text, aagendaDoom :: Int }
  deriving stock (Show, Generic)

instance ToJSON ArkhamAgenda where
  toJSON = genericToJSON $ aesonOptions $ Just "aagenda"
  toEncoding = genericToEncoding $ aesonOptions $ Just "aagenda"

instance FromJSON ArkhamAgenda where
  parseJSON = genericParseJSON $ aesonOptions $ Just "aagenda"

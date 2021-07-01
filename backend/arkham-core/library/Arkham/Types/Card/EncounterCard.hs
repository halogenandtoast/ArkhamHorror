module Arkham.Types.Card.EncounterCard where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.Id

newtype DiscardedEncounterCard = DiscardedEncounterCard { unDiscardedEncounterCard :: EncounterCard }

data EncounterCard = MkEncounterCard
  { ecId :: CardId
  , ecDef :: CardDef
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance HasCardDef EncounterCard where
  defL = lens ecDef $ \m x -> m { ecDef = x }

instance ToJSON EncounterCard where
  toJSON = genericToJSON $ aesonOptions $ Just "ec"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ec"

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ec"

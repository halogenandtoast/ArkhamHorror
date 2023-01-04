module Arkham.Card.EncounterCard where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Json
import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Id

newtype DiscardedEncounterCard = DiscardedEncounterCard { unDiscardedEncounterCard :: EncounterCard }

data EncounterCard = MkEncounterCard
  { ecId :: CardId
  , ecCardCode :: CardCode
  , ecOriginalCardCode :: CardCode
  , ecIsFlipped :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance HasCardCode EncounterCard where
  toCardCode = ecCardCode

instance HasCardDef EncounterCard where
  toCardDef c = case lookup (ecCardCode c) allEncounterCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for encounter card " <> show (ecCardCode c)

instance HasOriginalCardCode EncounterCard where
  toOriginalCardCode = ecOriginalCardCode

instance ToJSON EncounterCard where
  toJSON = genericToJSON $ aesonOptions $ Just "ec"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ec"

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ec"

lookupEncounterCard :: CardDef -> CardId -> EncounterCard
lookupEncounterCard cardDef cardId = MkEncounterCard
  { ecId = cardId
  , ecCardCode = toCardCode cardDef
  , ecOriginalCardCode = toCardCode cardDef
  , ecIsFlipped =
    Just $ isJust (cdRevealedName cardDef) && cdDoubleSided cardDef
  }

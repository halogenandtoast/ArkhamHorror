module Arkham.Types.Card.EncounterCard where

import Arkham.Prelude

import Arkham.EncounterCard
import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardDef
import Arkham.Types.Card.Id

newtype DiscardedEncounterCard = DiscardedEncounterCard { unDiscardedEncounterCard :: EncounterCard }

data EncounterCard = MkEncounterCard
  { ecId :: CardId
  , ecCardCode :: CardCode
  , ecOriginalCardCode :: CardCode
  , ecIsFlipped :: Maybe Bool
  }
  deriving stock (Ord, Show, Eq, Generic)

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

genEncounterCard :: MonadRandom m => CardDef -> m EncounterCard
genEncounterCard cardDef = lookupEncounterCard cardDef <$> getRandom

lookupEncounterCard :: CardDef -> CardId -> EncounterCard
lookupEncounterCard cardDef cardId = MkEncounterCard
  { ecId = cardId
  , ecCardCode = toCardCode cardDef
  , ecOriginalCardCode = toCardCode cardDef
  , ecIsFlipped = Just $ isJust (cdRevealedName cardDef)
  }

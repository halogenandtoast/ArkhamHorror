{-# LANGUAGE TemplateHaskell #-}

module Arkham.Card.EncounterCard where

import Arkham.Prelude

import Arkham.Card.CardCode
import Arkham.Card.CardDef
import Arkham.Card.Id
import Arkham.EncounterCard
import Arkham.Id
import Arkham.Json
import Arkham.Name
import Data.Aeson.TH
import GHC.Records

newtype DiscardedEncounterCard = DiscardedEncounterCard {unDiscardedEncounterCard :: EncounterCard}

data EncounterCard = MkEncounterCard
  { ecId :: CardId
  , ecCardCode :: CardCode
  , ecOriginalCardCode :: CardCode
  , ecIsFlipped :: Maybe Bool
  , ecAddedPeril :: Bool
  , ecOwner :: Maybe InvestigatorId
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "title" EncounterCard Text where
  getField = toTitle
  {-# INLINE getField #-}

instance HasField "name" EncounterCard Name where
  getField = toName
  {-# INLINE getField #-}

instance HasField "id" EncounterCard CardId where
  getField = ecId
  {-# INLINE getField #-}

instance HasField "cardCode" EncounterCard CardCode where
  getField = ecCardCode
  {-# INLINE getField #-}

instance HasField "victoryPoints" EncounterCard (Maybe Int) where
  getField = (.victoryPoints) . toCardDef

instance HasCardCode EncounterCard where
  toCardCode = ecCardCode

instance HasCardDef EncounterCard where
  toCardDef c = case lookup (ecCardCode c) allEncounterCards of
    Just def -> def
    Nothing ->
      error $ "missing card def for encounter card " <> show (ecCardCode c)

instance Named EncounterCard where
  toName = toName . toCardDef

instance HasOriginalCardCode EncounterCard where
  toOriginalCardCode = ecOriginalCardCode

lookupEncounterCard :: CardDef -> CardId -> EncounterCard
lookupEncounterCard cardDef cardId =
  MkEncounterCard
    { ecId = cardId
    , ecCardCode = toCardCode cardDef
    , ecOriginalCardCode = toCardCode cardDef
    , ecIsFlipped =
        Just $ isJust (cdRevealedName cardDef) && cdDoubleSided cardDef
    , ecAddedPeril = False
    , ecOwner = Nothing
    }

$(deriveJSON (aesonOptions $ Just "ec") ''EncounterCard)

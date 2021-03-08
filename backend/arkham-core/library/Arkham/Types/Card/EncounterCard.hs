module Arkham.Types.Card.EncounterCard
  ( module Arkham.Types.Card.EncounterCard
  , module Arkham.Types.Card.EncounterCardMatcher
  , module Arkham.Types.Card.EncounterCardType
  ) where

import Arkham.Prelude

import Arkham.Json
import Arkham.Types.Card.CardCode
import Arkham.Types.Card.EncounterCardMatcher
import Arkham.Types.Card.EncounterCardType
import Arkham.Types.Card.Id
import Arkham.Types.Keyword (Keyword)
import Arkham.Types.Name
import Arkham.Types.Trait

newtype DiscardedEncounterCard = DiscardedEncounterCard { unDiscardedEncounterCard :: EncounterCard }

data EncounterCard = MkEncounterCard
  { ecCardCode :: CardCode
  , ecName :: Name
  , ecCardType :: EncounterCardType
  , ecTraits :: HashSet Trait
  , ecKeywords :: HashSet Keyword
  , ecId :: CardId
  , ecVictoryPoints :: Maybe Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass Hashable

instance ToJSON EncounterCard where
  toJSON = genericToJSON $ aesonOptions $ Just "ec"
  toEncoding = genericToEncoding $ aesonOptions $ Just "ec"

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON $ aesonOptions $ Just "ec"

encounterCardMatch :: EncounterCardMatcher -> EncounterCard -> Bool
encounterCardMatch (EncounterCardMatchByType (cardType, mtrait)) MkEncounterCard {..}
  = ecCardType == cardType && maybe True (`elem` ecTraits) mtrait
encounterCardMatch (EncounterCardMatchByCardCode cardCode) card =
  ecCardCode card == cardCode

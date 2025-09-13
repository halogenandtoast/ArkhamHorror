module Arkham.Card (module Arkham.Card, module Arkham.Card.CardCode, module Arkham.Card.Id) where

import Arkham.Prelude

import Arkham.Card.CardCode
import {-# SOURCE #-} Arkham.Card.CardDef
import {-# SOURCE #-} Arkham.Card.EncounterCard
import Arkham.Card.Id
import {-# SOURCE #-} Arkham.Card.PlayerCard
import Arkham.Customization
import Arkham.Id
import Arkham.Taboo.Types
import Arkham.Trait
import GHC.Records

data Card
  = PlayerCard PlayerCard
  | EncounterCard EncounterCard
  | VengeanceCard Card

instance Data Card
instance Show Card
instance Eq Card
instance Ord Card
instance ToJSON Card
instance FromJSON Card
instance HasField "id" Card CardId

class MonadRandom m => CardGen m where
  genEncounterCard :: HasCardDef a => a -> m EncounterCard
  genPlayerCard :: HasCardDef a => a -> m PlayerCard
  replaceCard :: CardId -> Card -> m ()
  removeCard :: CardId -> m ()
  clearCardCache :: m ()

class (HasTraits a, HasCardDef a, HasCardCode a) => IsCard a where
  toCard :: HasCallStack => a -> Card
  toCardId :: a -> CardId
  toCardOwner :: a -> Maybe InvestigatorId
  toCustomizations :: a -> Customizations
  toCustomizations _ = mempty
  toTabooList :: a -> Maybe TabooList
  toTabooList _ = Nothing
  toMutated :: a -> Maybe Text
  toMutated _ = Nothing

instance IsCard Card
instance IsCard PlayerCard
instance IsCard EncounterCard

lookupCardDef :: HasCardCode cardCode => cardCode -> Maybe CardDef
isEncounterCard :: Card -> Bool

module Arkham.Campaigns.TheScarletKeys.Concealed.Types where

import {-# SOURCE #-} Arkham.Placement
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Classes.Entity
import Arkham.Prelude
import Arkham.Id
import Arkham.Source
import Arkham.Target
import GHC.Records

data ConcealedCard = ConcealedCard
  { concealedCardKind :: ConcealedCardKind
  , concealedCardId :: ConcealedCardId
  , concealedCardPlacement :: Placement
  , concealedCardFlipped :: Bool
  }
  deriving stock (Show, Eq, Ord, Data)

instance HasField "placement" ConcealedCard Placement where
  getField = concealedCardPlacement

instance HasField "kind" ConcealedCard ConcealedCardKind where
  getField = concealedCardKind

instance HasField "id" ConcealedCard ConcealedCardId where
  getField = concealedCardId

instance HasField "flipped" ConcealedCard Bool where
  getField = concealedCardFlipped

instance Sourceable ConcealedCard where
  toSource = ConcealedCardSource . toId

instance Targetable ConcealedCard where
  toTarget = ConcealedCardTarget . toId

instance HasField "ability" ConcealedCard (Int -> Source) where
  getField c = toAbilitySource c

instance Entity ConcealedCard where
  type EntityId ConcealedCard = ConcealedCardId
  type EntityAttrs ConcealedCard = ConcealedCard
  toId = concealedCardId
  toAttrs = id
  overAttrs f = f


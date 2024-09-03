module Arkham.Card.CardDef where

import Arkham.Prelude

data CardDef

class HasCardDef a where
  toCardDef :: (HasCallStack) => a -> CardDef

{-# LANGUAGE RoleAnnotations #-}
module Arkham.Card.CardDef where

import Arkham.Card.CardType

type role CardDef phantom
type CardDef :: CardType -> *
data CardDef k

data SomeCardDef

class HasCardDef a where
  toCardDef :: a -> SomeCardDef

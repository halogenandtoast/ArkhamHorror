{-# LANGUAGE RoleAnnotations #-}
module Arkham.Card.CardDef where

import Arkham.Card.CardType
import Data.Kind

type role CardDef phantom
type CardDef :: CardType -> Type
data CardDef k

data SomeCardDef

class HasCardDef a where
  toCardDef :: a -> SomeCardDef

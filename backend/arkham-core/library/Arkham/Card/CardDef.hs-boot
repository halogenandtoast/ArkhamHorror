module Arkham.Card.CardDef where

data CardDef

class HasCardDef a where
  toCardDef :: a -> CardDef

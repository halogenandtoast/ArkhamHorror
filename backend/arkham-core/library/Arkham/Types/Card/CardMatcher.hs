module Arkham.Types.Card.CardMatcher where

import Arkham.Types.Card.CardCode
import Arkham.Types.Card.CardType
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

data CardMatcher = CardMatchByType (CardType, HashSet Trait) | CardMatchByCardCode CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

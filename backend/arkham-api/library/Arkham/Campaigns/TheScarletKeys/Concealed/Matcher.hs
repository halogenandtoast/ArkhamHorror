{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Concealed.Matcher where

import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Matcher.Base
import Arkham.Matcher.Location
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import {-# SOURCE #-} Arkham.Source
import Data.Aeson.TH

data ConcealedCardMatcher
  = ConcealedCardWithPlacement Placement
  | ConcealedCardAny
  | ExposableConcealedCard Source
  | ConcealedCardOneOf [ConcealedCardMatcher]
  | ConcealedCardMatchAll [ConcealedCardMatcher]
  | ConcealedCardAt LocationMatcher
  | ConcealedCardIs ConcealedCardKind
  deriving stock (Show, Eq, Ord, Data)

pattern IsDecoy :: ConcealedCardMatcher
pattern IsDecoy <- ConcealedCardIs Decoy
  where
    IsDecoy = ConcealedCardIs Decoy

instance OneOf ConcealedCardMatcher where
  oneOf = ConcealedCardOneOf

instance Semigroup ConcealedCardMatcher where
  ConcealedCardAny <> a = a
  a <> ConcealedCardAny = a
  ConcealedCardMatchAll xs <> ConcealedCardMatchAll ys = ConcealedCardMatchAll (xs <> ys)
  ConcealedCardMatchAll xs <> x = ConcealedCardMatchAll $ xs <> [x]
  x <> ConcealedCardMatchAll xs = ConcealedCardMatchAll (x : xs)
  x <> y = ConcealedCardMatchAll [x, y]

$(deriveJSON defaultOptions ''ConcealedCardMatcher)

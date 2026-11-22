{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.DreamingCard where

import {-# SOURCE #-} Arkham.Matcher.Investigator
import Arkham.Matcher.Value
import Arkham.Prelude
import Control.Lens.Plated (Plated)
import Data.Aeson.TH
import GHC.OverloadedLabels

{- | A card facedown in a dream zone.

Two constructors only. There is deliberately no @DreamingCardWithCard@: nothing
reads a facedown dreaming card's identity (every identity read happens while the
card is in hand or in deck), and leaving it out is the structural guarantee that
the dream zone is not addressable as if the card were in play.
-}
data DreamingCardMatcher
  = AnyDreamingCard
  | DreamingCardControlledBy InvestigatorMatcher
  | DreamingCardWithDreams ValueMatcher
  | DreamingCardMatches [DreamingCardMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup DreamingCardMatcher where
  AnyDreamingCard <> x = x
  x <> AnyDreamingCard = x
  DreamingCardMatches xs <> DreamingCardMatches ys = DreamingCardMatches (xs <> ys)
  DreamingCardMatches xs <> x = DreamingCardMatches (x : xs)
  x <> DreamingCardMatches xs = DreamingCardMatches (x : xs)
  x <> y = DreamingCardMatches [x, y]

instance Monoid DreamingCardMatcher where
  mempty = AnyDreamingCard

instance Plated DreamingCardMatcher

instance IsLabel "any" DreamingCardMatcher where
  fromLabel = AnyDreamingCard

$(deriveJSON defaultOptions ''DreamingCardMatcher)

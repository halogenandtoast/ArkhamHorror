module Arkham.Types.EventMatcher where

import Arkham.Prelude

import Arkham.Types.ClassSymbol
import Arkham.Types.Id
import Arkham.Types.Trait

data EventMatcher
  = EventWithTitle Text
  | EventWithFullTitle Text Text
  | EventWithId EventId
  | EventWithTrait Trait
  | EventWithClass ClassSymbol
  | EventOwnedBy InvestigatorId
  | EventMatches [EventMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup EventMatcher where
  EventMatches xs <> EventMatches ys = EventMatches (xs <> ys)
  EventMatches xs <> x = EventMatches (x : xs)
  x <> EventMatches xs = EventMatches (x : xs)
  x <> y = EventMatches [x, y]

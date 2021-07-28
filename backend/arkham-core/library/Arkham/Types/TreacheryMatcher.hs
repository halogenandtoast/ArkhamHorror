module Arkham.Types.TreacheryMatcher where

import Arkham.Prelude

import Arkham.Types.Id
import Arkham.Types.Trait

data TreacheryMatcher
  = TreacheryWithTitle Text
  | TreacheryWithFullTitle Text Text
  | TreacheryWithId TreacheryId
  | TreacheryWithTrait Trait
  | TreacheryOwnedBy InvestigatorId
  | TreacheryMatches [TreacheryMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup TreacheryMatcher where
  TreacheryMatches xs <> TreacheryMatches ys = TreacheryMatches (xs <> ys)
  TreacheryMatches xs <> x = TreacheryMatches (x : xs)
  x <> TreacheryMatches xs = TreacheryMatches (x : xs)
  x <> y = TreacheryMatches [x, y]

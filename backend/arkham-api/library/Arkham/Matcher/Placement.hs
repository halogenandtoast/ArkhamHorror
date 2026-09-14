{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Placement where

import Arkham.Matcher.Location
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Data.Aeson.TH

{- | Where a card came to rest, for windows that used to be able to assume a
location. A spawn does not always land at one: a @Concealed X@ enemy spawns
into the shadows, which is in play but "not at any location".
-}
data PlacementMatcher
  = AnyPlacement
  | {- | The placement resolves to a location matching this (a threat area and
    an attachment resolve to their host's location).
    -}
    PlacementAt LocationMatcher
  | PlacementIs Placement
  | PlacementOneOf [PlacementMatcher]
  | PlacementMatchAll [PlacementMatcher]
  | NotPlacement PlacementMatcher
  deriving stock (Show, Eq, Ord, Data)

instance Semigroup PlacementMatcher where
  AnyPlacement <> x = x
  x <> AnyPlacement = x
  PlacementMatchAll xs <> PlacementMatchAll ys = PlacementMatchAll (xs <> ys)
  PlacementMatchAll xs <> y = PlacementMatchAll (xs <> [y])
  x <> PlacementMatchAll ys = PlacementMatchAll (x : ys)
  x <> y = PlacementMatchAll [x, y]

instance Monoid PlacementMatcher where
  mempty = AnyPlacement

$(deriveJSON defaultOptions ''PlacementMatcher)

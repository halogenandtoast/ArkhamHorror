{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Key.Matcher where

import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Arkham.Matcher.Base
import Data.Aeson.TH

data ScarletKeyMatcher
  = ScarletKeyWithPlacement Placement
  | ScarletKeyAny
  | ScarletKeyOneOf [ScarletKeyMatcher]
  | ScarletKeyMatchAll [ScarletKeyMatcher]
  deriving stock (Show, Eq, Ord, Data)

instance OneOf ScarletKeyMatcher where
  oneOf = ScarletKeyOneOf

instance Semigroup ScarletKeyMatcher where
  ScarletKeyAny <> a = a
  a <> ScarletKeyAny = a
  ScarletKeyMatchAll xs <> ScarletKeyMatchAll ys = ScarletKeyMatchAll (xs <> ys)
  ScarletKeyMatchAll xs <> x = ScarletKeyMatchAll $ xs <> [x]
  x <> ScarletKeyMatchAll xs = ScarletKeyMatchAll (x : xs)
  x <> y = ScarletKeyMatchAll [x, y]

$(deriveJSON defaultOptions ''ScarletKeyMatcher)

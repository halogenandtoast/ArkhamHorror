{-# LANGUAGE TemplateHaskell #-}

module Arkham.Campaigns.TheScarletKeys.Key.Matcher where

import Arkham.Campaigns.TheScarletKeys.Key.Stability
import Arkham.Matcher.Base
import Arkham.Matcher.Investigator
import Arkham.Matcher.Enemy
import {-# SOURCE #-} Arkham.Placement
import Arkham.Prelude
import Data.Aeson.TH

data ScarletKeyMatcher
  = ScarletKeyWithPlacement Placement
  | ScarletKeyAny
  | ScarletKeyWithBearer InvestigatorMatcher
  | ScarletKeyWithEnemyBearer EnemyMatcher
  | ScarletKeyWithStability Stability
  | ScarletKeyOneOf [ScarletKeyMatcher]
  | ScarletKeyMatchAll [ScarletKeyMatcher]
  deriving stock (Show, Eq, Ord, Data)

pattern StableScarletKey :: ScarletKeyMatcher
pattern StableScarletKey <- ScarletKeyWithStability Stable
  where
    StableScarletKey = ScarletKeyWithStability Stable

pattern UnstableScarletKey :: ScarletKeyMatcher
pattern UnstableScarletKey <- ScarletKeyWithStability Unstable
  where
    UnstableScarletKey = ScarletKeyWithStability Unstable

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

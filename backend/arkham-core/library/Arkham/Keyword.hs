{-# LANGUAGE TemplateHaskell #-}
module Arkham.Keyword where

import Arkham.Prelude

import {-# SOURCE #-} Arkham.Matcher.Types
import Arkham.CampaignLogKey
import Data.Aeson.TH

data Keyword
  = Alert
  | Aloof
  | Fast
  | Hidden
  | Hunter
  | Massive
  | Peril
  | Retaliate
  | Surge
  | Uses Int
  | Exceptional
  | Permanent
  | Researched CampaignLogKey
  | Seal TokenMatcher
  deriving stock (Show, Eq, Ord)

class HasKeywords a where
  toKeywords :: a -> Set Keyword

$(deriveJSON defaultOptions ''Keyword)

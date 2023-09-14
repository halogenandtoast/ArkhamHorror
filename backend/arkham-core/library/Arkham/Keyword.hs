{-# LANGUAGE TemplateHaskell #-}

module Arkham.Keyword where

import Arkham.Prelude

import Arkham.CampaignLogKey
import {-# SOURCE #-} Arkham.Matcher.Types
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
  | Seal ChaosTokenMatcher
  deriving stock (Show, Eq, Ord, Data)

class HasKeywords a where
  toKeywords :: a -> Set Keyword

$(deriveJSON defaultOptions ''Keyword)

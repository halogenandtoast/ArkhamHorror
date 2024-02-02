{-# LANGUAGE TemplateHaskell #-}

module Arkham.Keyword where

import Arkham.Prelude

import Arkham.CampaignLogKey
import Arkham.Card.CardCode
import Arkham.GameValue
import {-# SOURCE #-} Arkham.Matcher.Types
import Control.Lens (Prism', prism')
import Data.Aeson.TH
import GHC.OverloadedLabels

data Keyword
  = Alert
  | Aloof
  | Fast
  | Hidden
  | Hunter
  | Massive
  | Myriad
  | Peril
  | Retaliate
  | Surge
  | Uses Int
  | Exceptional
  | Permanent
  | Researched CampaignLogKey
  | Seal ChaosTokenMatcher
  | Bonded Int CardCode
  | Patrol LocationMatcher
  | Swarming GameValue
  | Veiled
  | Customizable
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (NoThunks, NFData)

class HasKeywords a where
  toKeywords :: a -> Set Keyword

_Swarming :: Prism' Keyword GameValue
_Swarming = prism' Swarming $ \case
  Swarming n -> Just n
  _ -> Nothing

instance IsLabel "aloof" Keyword where
  fromLabel = Aloof

instance IsLabel "massive" Keyword where
  fromLabel = Massive

$(deriveJSON defaultOptions ''Keyword)

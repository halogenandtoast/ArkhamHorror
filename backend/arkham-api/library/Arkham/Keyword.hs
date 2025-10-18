{-# LANGUAGE TemplateHaskell #-}

module Arkham.Keyword where

import Arkham.CampaignLogKey
import Arkham.Campaigns.TheScarletKeys.Concealed.Kind
import Arkham.Card.CardCode
import Arkham.GameValue
import Arkham.Matcher.ChaosToken
import {-# SOURCE #-} Arkham.Matcher.Location
import Arkham.Prelude
import Control.Lens (Prism', prism')
import Data.Aeson.TH
import GHC.OverloadedLabels

data Sealing
  = Sealing ChaosTokenMatcher
  | SealUpTo Int ChaosTokenMatcher
  | SealUpToX ChaosTokenMatcher
  | SealOneOf (NonEmpty Sealing)
  deriving stock (Show, Eq, Ord, Data, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Keyword
  = Alert
  | Aloof
  | Elusive
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
  | Seal Sealing
  | Bonded Int CardCode
  | Patrol LocationMatcher
  | Swarming GameValue
  | Veiled
  | Customizable
  | Advanced
  | Replacement
  | Partner
  | Concealed ConcealedCardKind GameValue
  deriving stock (Show, Eq, Ord, Data)

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

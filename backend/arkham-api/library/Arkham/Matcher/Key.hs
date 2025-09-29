{-# LANGUAGE TemplateHaskell #-}

module Arkham.Matcher.Key where

import Arkham.Id
import Arkham.Key
import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data KeyWithDetails = KeyWithDetails
  { key :: ArkhamKey
  , keyLocation :: Maybe LocationId
  , keyInvestigator :: Maybe InvestigatorId
  , keyAsset :: Maybe AssetId
  , keySetAside :: Bool
  }
  deriving stock (Show, Eq, Ord, Generic)

data KeyMatcher = AnyKey | KeyOnCard | KeyIs ArkhamKey
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "any" KeyMatcher where
  fromLabel = AnyKey

$(deriveJSON defaultOptions ''KeyMatcher)

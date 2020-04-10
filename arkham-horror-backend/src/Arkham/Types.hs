{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Arkham.Types where

import           Data.Aeson       (withObject)
import           Data.Aeson.Types (ToJSONKey)
import           Import

newtype Scenario = Scenario { getScenario :: Text }
  deriving newtype (ToJSON)

newtype Cycle = Cycle { getCycle :: Text }
  deriving newtype (Eq, Ord, ToJSON, ToJSONKey)

instance FromJSON Cycle where
  parseJSON = withObject "Cycle" $ \v -> Cycle <$> v .: "name"

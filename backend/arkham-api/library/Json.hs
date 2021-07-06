module Json
  ( module X
  , aesonOptions
  ) where

import ClassyPrelude
import Data.Aeson as X
import Data.Aeson.Casing (camelCase)

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions { fieldLabelModifier = camelCase . drop len }
  where len = maybe 0 length ms

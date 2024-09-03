module Arkham.Json (
  module X,
  aesonOptions,
  camelCase,
) where

import Arkham.Prelude
import Data.Aeson as X
import Data.Aeson.Casing (camelCase)
import Data.Aeson.Text as X

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions {fieldLabelModifier = camelCase . drop len}
 where
  len = maybe 0 length ms

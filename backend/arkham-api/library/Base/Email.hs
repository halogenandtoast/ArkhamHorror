module Base.Email (normalizeEmail) where

import Data.Text qualified as T
import Relude

normalizeEmail :: Text -> Text
normalizeEmail = T.toLower . T.strip

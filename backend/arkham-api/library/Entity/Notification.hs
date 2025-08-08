{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.Notification where

import Database.Persist.TH
import Relude
import Data.Time.Clock

mkPersist
  sqlSettings
  [persistLowerCase|
Notification json sql=notifications
  body Text
  createdAt UTCTime
  deriving Show
|]

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.PasswordReset where

import Data.Time.Clock
import Data.UUID
import Database.Persist.TH
import Entity.User
import Orphans ()
import Relude

mkPersist
  sqlSettings
  [persistLowerCase|
PasswordReset json sql=password_resets
    Id UUID default=uuid_generate_v1mc()
    userId UserId
    expiresAt UTCTime
    deriving Show
|]

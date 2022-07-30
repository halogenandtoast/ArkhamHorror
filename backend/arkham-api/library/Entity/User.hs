{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.User where

import Database.Persist.TH
import Relude

mkPersist
  sqlSettings
  [persistLowerCase|
User json sql=users
    username Text
    email Text
    passwordDigest Text
    beta Bool
    UniqueUsername username
    UniqueEmail email
|]

deriving stock instance Show User

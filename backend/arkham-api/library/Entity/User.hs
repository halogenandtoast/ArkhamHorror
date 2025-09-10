{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.User where

import Database.Persist.TH
import Relude
import Entity

mkEntity $(discoverEntities)
  [persistLowerCase|
User json sql=users
    username Text
    email Text
    passwordDigest Text
    beta Bool
    admin Bool default=False
    UniqueUsername username
    UniqueEmail email
    deriving Show
|]

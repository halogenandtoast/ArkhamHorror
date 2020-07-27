{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.User where

import ClassyPrelude
import Database.Persist.TH

mkPersist sqlSettings [persistLowerCase|
User json sql=users
    username Text
    email Text
    passwordDigest Text
    UniqueUsername username
    UniqueEmail email
    deriving Show
|]

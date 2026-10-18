{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-missing-deriving-strategies #-}

module Entity.User where

import Database.Persist.TH
import Entity
import Relude

mkEntity
  $(discoverEntities)
  [persistLowerCase|
User json sql=users
    username Text
    email Text
    passwordDigest Text
    beta Bool
    phaseTransitionNotifications Bool default=False
    admin Bool default=False
    features Text default=''
    allowSpectatorsByDefault Bool default=False
    UniqueUsername username
    UniqueEmail email
    deriving Show
|]

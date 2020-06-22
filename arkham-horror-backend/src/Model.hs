{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Model
  ( module Model
  , module X
  )
where

import ClassyPrelude.Yesod

import Database.Persist.Postgresql.JSON ()
import Database.Persist.Quasi
import Database.Persist.Sql

import Entity.User as X
import Arkham.Entity.ArkhamGame as X

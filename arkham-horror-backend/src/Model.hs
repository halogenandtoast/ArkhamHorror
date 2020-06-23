{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# OPTIONS_GHC -fno-warn-missing-deriving-strategies #-}

module Model
  ( module X
  )
where

import ClassyPrelude.Yesod

import Database.Persist.Postgresql.JSON ()

import Arkham.Entity.ArkhamGame as X
import Entity.User as X

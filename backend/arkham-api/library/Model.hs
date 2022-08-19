{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Model
  ( module X
  ) where

import Database.Persist.Postgresql.JSON ()
import Entity.Arkham.ArkhamDBDecklist as X
import Entity.Arkham.Deck as X
import Entity.Arkham.Game as X
import Entity.Arkham.Player as X
import Entity.User as X

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Where third-edition tables are kept.

They used to live only in Redis, which costs a game every time the key is
evicted, expires or fails to decode after a deploy. The row is the table; the
whole 'ThirdEdition.Table.Table' rides in @payload@ as jsonb, with the few
fields the lobby queries lifted out beside it.
-}
module Entity.Arkham.ThirdEditionTable (module Entity.Arkham.ThirdEditionTable) where

import Data.Aeson (Value)
import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.Postgresql.JSON ()
import Database.Persist.TH
import Entity
import Entity.User
import Orphans ()
import Relude

mkEntity
  $(discoverEntities)
  [persistLowerCase|
ThirdEditionTable sql=arkham_third_edition_tables
  Id UUID default=uuid_generate_v4()
  hostId UserId OnDeleteCascade
  started Bool
  version Int
  payload Value
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show

ThirdEditionHistory sql=arkham_third_edition_history
  tableId ThirdEditionTableId OnDeleteCascade
  game Value
  createdAt UTCTime
  deriving Generic Show
|]

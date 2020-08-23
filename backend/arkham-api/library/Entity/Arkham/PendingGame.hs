{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Arkham.PendingGame
  ( module Entity.Arkham.PendingGame
  )
where

import ClassyPrelude
import Data.UUID
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import Entity.Arkham.GameSetupState
import Json
import Orphans ()

mkPersist sqlSettings [persistLowerCase|
ArkhamPendingGame sql=arkham_pending_games
  token UUID
  gameSetup GameSetupState
  deriving Generic Show
  UniqueArkhamPendingGameToken token
|]

instance ToJSON ArkhamPendingGame where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPendingGame"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPendingGame"

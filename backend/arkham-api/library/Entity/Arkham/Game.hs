{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Arkham.Game
  ( module Entity.Arkham.Game
  )
where

import Arkham.Types.GameJson
import ClassyPrelude
import Database.Persist.Postgresql.JSON ()
import Database.Persist.Sql
import Database.Persist.TH
import Json
import Orphans ()

mkPersist sqlSettings [persistLowerCase|
ArkhamGame sql=arkham_games
  currentData GameJson
  deriving Generic Show
|]

instance ToJSON ArkhamGame where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamGame"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamGame"

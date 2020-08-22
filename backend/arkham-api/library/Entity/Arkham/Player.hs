{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Arkham.Player
  ( module Entity.Arkham.Player
  )
where

import ClassyPrelude
import Database.Persist.TH
import Entity.Arkham.Game
import Entity.User
import Json
import Orphans ()

mkPersist sqlSettings [persistLowerCase|
ArkhamPlayer sql=arkham_players
  userId UserId
  arkhamGameId ArkhamGameId
  deriving Generic Show
|]

instance ToJSON ArkhamPlayer where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPlayer"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPlayer"

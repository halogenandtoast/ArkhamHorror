{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Entity.Arkham.Game
  ( ArkhamGame(..)
  , ArkhamGameId
  )
where

import Arkham.Types.GameJson
import ClassyPrelude
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

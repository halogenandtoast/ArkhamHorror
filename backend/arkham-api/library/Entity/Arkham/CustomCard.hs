{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.CustomCard (
  module Entity.Arkham.CustomCard,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.User
import Json
import Orphans ()
import Relude

{- | A card its author built in the card builder, kept against their account so
it outlives any one game and follows them between browsers.

@cardCode@ is the code minted when the card was first created and is what every
game refers to it by, so it is the identity here too -- saving an edit replaces
the row rather than adding one. @art@ is a URL or an inlined data URI and is
stored apart from @def@ so listing the library does not have to carry it.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamCustomCard sql=arkham_custom_cards
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  cardCode Text
  def Value
  art Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueUserCustomCard userId cardCode
  deriving Generic Show
|]

instance ToJSON ArkhamCustomCard where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCustomCard"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCustomCard"

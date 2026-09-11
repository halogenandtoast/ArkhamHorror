{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.CustomCardSet (
  module Entity.Arkham.CustomCardSet,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.User
import Json
import Orphans ()
import Relude

{- | A named collection of custom cards: the unit you build, export, and hand to
someone else.

Grouping used to be a string each card carried in its own def, which meant a set
existed only as far as every one of its cards agreed on the spelling, and
throwing one away meant deleting its cards one at a time. A set is a row now, so
it can be renamed in one place and deleted with what is in it.

@sourceCode@ is the id of the pack an imported set came from (arkham.build gives
one), so importing that pack again replaces this set rather than making a second
copy of it. A set built here has none.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamCustomCardSet sql=arkham_custom_card_sets
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  name Text
  sourceCode Text Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueUserCustomCardSetName userId name
  deriving Generic Show
|]

instance ToJSON ArkhamCustomCardSet where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCustomCardSet"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCustomCardSet"

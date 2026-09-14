{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.PublishedCardSet (
  module Entity.Arkham.PublishedCardSet,
) where

import Data.Time.Clock
import Data.UUID (UUID)
import Database.Persist.TH
import Entity
import Entity.Arkham.CustomCardSet
import Entity.User
import Json
import Orphans ()
import Relude

{- | A custom card set its author has put in the marketplace, the versions of it
they have published, and one of your own sets following it.

@ArkhamPublishedCardSet@ is the listing: one row however many times it is
published. @customCardSetId@ is the author's own copy, so publishing again knows
which listing to add to; it goes null rather than taking the listing with it if
the author deletes that copy, because subscribers still have something to read.

@ArkhamPublishedCardSetVersion@ holds a version's cards outright rather than
reading them back off the author's set. A version has to stay importable exactly
as published: someone who edited their copy and wants the published one back has
to be able to take it again, and the author has meanwhile moved on.

@ArkhamCardSetSubscription@ is one of your sets following a published one. It is
its own table rather than columns on @ArkhamCustomCardSet@ because the listing
already points at that table, and pointing back would make the two modules import
each other. The row is deleted rather than updated when the set is edited: what
is in it is then not what was published.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamPublishedCardSet sql=arkham_published_card_sets
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  customCardSetId ArkhamCustomCardSetId Maybe
  name Text
  latestVersion Int
  createdAt UTCTime
  updatedAt UTCTime
  deriving Generic Show

ArkhamPublishedCardSetVersion sql=arkham_published_card_set_versions
  Id UUID default=uuid_generate_v4()
  publishedCardSetId ArkhamPublishedCardSetId OnDeleteCascade
  version Int
  note Text Maybe
  name Text
  cards Value
  createdAt UTCTime
  UniquePublishedCardSetVersion publishedCardSetId version
  deriving Generic Show

ArkhamPublishedCardSetLike sql=arkham_published_card_set_likes
  Id UUID default=uuid_generate_v4()
  publishedCardSetId ArkhamPublishedCardSetId OnDeleteCascade
  userId UserId OnDeleteCascade
  createdAt UTCTime
  UniquePublishedCardSetLike publishedCardSetId userId
  deriving Generic Show

ArkhamCardSetSubscription sql=arkham_card_set_subscriptions
  Id UUID default=uuid_generate_v4()
  customCardSetId ArkhamCustomCardSetId OnDeleteCascade
  publishedCardSetId ArkhamPublishedCardSetId OnDeleteCascade
  version Int
  createdAt UTCTime
  updatedAt UTCTime
  UniqueCardSetSubscription customCardSetId
  deriving Generic Show
|]

instance ToJSON ArkhamPublishedCardSet where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPublishedCardSet"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPublishedCardSet"

instance ToJSON ArkhamPublishedCardSetVersion where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPublishedCardSetVersion"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPublishedCardSetVersion"

instance ToJSON ArkhamPublishedCardSetLike where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamPublishedCardSetLike"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamPublishedCardSetLike"

instance ToJSON ArkhamCardSetSubscription where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCardSetSubscription"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCardSetSubscription"

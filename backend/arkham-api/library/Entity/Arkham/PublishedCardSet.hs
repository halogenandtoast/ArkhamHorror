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

@description@ is the listing's own copy of what the set is. It is the listing's
rather than a version's because it is not part of what was reviewed: the author
can rewrite it at any time and nothing about the published cards changes, which
is the point of it not being on the version.

@url@ is the listing's own copy of where the set lives in the world, and travels
with the description for the same reason: it says where to read more about the
set, not what is in a version of it.

@ArkhamPublishedCardSetVersion@ holds a version's cards outright rather than
reading them back off the author's set. A version has to stay importable exactly
as published: someone who edited their copy and wants the published one back has
to be able to take it again, and the author has meanwhile moved on.

@ArkhamCardSetSubscription@ is one of your sets following a published one. It is
its own table rather than columns on @ArkhamCustomCardSet@ because the listing
already points at that table, and pointing back would make the two modules import
each other. The row is deleted rather than updated when the set is edited: what
is in it is then not what was published.

@ArkhamCardSetSubmission@ is a version waiting to be reviewed, or the record of
it having been. Publishing makes one; nothing is importable until someone has
approved it, and @approvedVersion@ on the listing is the newest that anyone has
-- null until the first one passes, which is what keeps an unreviewed set off the
shelf. @notify@ is the author's answer to "tell me what was decided", asked at
submission rather than kept on the account because it is a choice about this
submission. @reason@ is only ever set on a denial: an approval that needs
explaining is a denial.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamPublishedCardSet sql=arkham_published_card_sets
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  customCardSetId ArkhamCustomCardSetId Maybe
  name Text
  description Text Maybe
  url Text Maybe
  latestVersion Int
  approvedVersion Int Maybe
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

ArkhamCardSetSubmission sql=arkham_card_set_submissions
  Id UUID default=uuid_generate_v4()
  publishedCardSetId ArkhamPublishedCardSetId OnDeleteCascade
  versionId ArkhamPublishedCardSetVersionId OnDeleteCascade
  userId UserId OnDeleteCascade
  version Int
  status Text
  notify Bool default=True
  reason Text Maybe
  reviewedByUserId UserId Maybe OnDeleteSetNull
  reviewedAt UTCTime Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueCardSetSubmissionVersion versionId
  deriving Generic Show
|]

{- | The three states a submission can be in. Spelled out so the handlers and the
client agree on what the column says.
-}
submissionPending, submissionApproved, submissionDenied :: Text
submissionPending = "pending"
submissionApproved = "approved"
submissionDenied = "denied"

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

instance ToJSON ArkhamCardSetSubmission where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCardSetSubmission"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCardSetSubmission"

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Entity.Arkham.CardSetShare (
  module Entity.Arkham.CardSetShare,
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

{- | Handing an unpublished set to a few people, and the record of who took it.

The marketplace ("Entity.Arkham.PublishedCardSet") is the other way to put a set
in somebody else's hands, and it is the wrong one for a set that is not
finished: it is public, every version is reviewed before anyone can take it, and
what playtesting wants is for the people trying the cards to get whatever was
changed this afternoon. So nothing here is versioned -- a share hands out the
author's set as it stands.

@ArkhamCardSetShare@ is the link. One per set, a url you send to whoever you
like rather than an invitation addressed to each of them, so minting a new token
or deleting the row is how everyone is cut off at once and
@ArkhamCardSetShareAccess@ is how one person is.

@token@ is stored as given rather than hashed, unlike an api key: the owner has
to be able to see the link again to send it to a second friend, so there is
nothing to be gained by being unable to read it.

@ArkhamCardSetShareAccess@ is one person the link reached. A share is a url, so
who has opened it is the only record there is of who has access -- which is the
whole reason this table exists rather than the token living on the set.

@customCardSetId@ is that person's own copy, so taking the author's newest cards
replaces the copy they have rather than leaving them with two. It goes null
rather than taking the row with it if they delete that copy, because they still
have access and can take it again.

@takenAt@ is when that copy was last written, and is what both sides of the
"is anything out of date" question are answered from: the author has changes if
their set's @updatedAt@ is newer, and the copy has been edited since if the
copy's is. Neither is a flag anybody maintains, which is why saving a card has
to touch the set row it belongs to.

@revokedAt@ is the author cutting one person off without disturbing the link
everyone else is holding. It takes nothing away from them: the cards they have
taken are in their own library, and all this stops is reading the set and taking
it again.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamCardSetShare sql=arkham_card_set_shares
  Id UUID default=uuid_generate_v4()
  customCardSetId ArkhamCustomCardSetId OnDeleteCascade
  token Text
  createdAt UTCTime
  updatedAt UTCTime
  UniqueCardSetShare customCardSetId
  UniqueCardSetShareToken token
  deriving Generic Show

ArkhamCardSetShareAccess sql=arkham_card_set_share_accesses
  Id UUID default=uuid_generate_v4()
  cardSetShareId ArkhamCardSetShareId OnDeleteCascade
  userId UserId OnDeleteCascade
  customCardSetId ArkhamCustomCardSetId Maybe OnDeleteSetNull
  takenAt UTCTime Maybe
  revokedAt UTCTime Maybe
  createdAt UTCTime
  updatedAt UTCTime
  UniqueCardSetShareAccess cardSetShareId userId
  deriving Generic Show
|]

instance ToJSON ArkhamCardSetShare where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCardSetShare"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCardSetShare"

instance ToJSON ArkhamCardSetShareAccess where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCardSetShareAccess"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCardSetShareAccess"

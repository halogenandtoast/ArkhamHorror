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

@description@ is what the set is, in the author's own words. It lives on the
set rather than on a published version because it describes the set and not what
changed in a version -- which is what @note@ on a version is for -- so it can be
rewritten without asking anyone to review it again.

@url@ is where the set lives in the world -- the blog post it was announced in,
the thread it is discussed in, the page its art came from. It is the author's to
point anywhere, so it is stored as given and only checked for being an @http@
address, which is all that is needed to be sure a link made of it goes to a page
rather than running something.

@sourceCode@ is the id of the pack an imported set came from (arkham.build gives
one), so importing that pack again replaces this set rather than making a second
copy of it. A set built here has none.

@investigatorExpansion@ says the set is a box rather than a handful of cards,
and is what unlocks the three fields below it. They are asked about nowhere
else, so a set of two events is never made to answer for an icon it does not
have.

@icon@ is the expansion symbol, which the marketplace files the set under. It
is only ever shown; nothing in a game reads it.

@tokens@ is the set's own tokens -- @[{slug, name, image}]@ -- the markers its
cards place on themselves. A token here is a counter on a card, not a chaos
token: those are a scenario's, and come from "Arkham.Homebrew.Tokens".

@keywords@ is the set's own keywords -- @[{name, description, token, shape}]@.
@description@ is the rules text as the box prints it, which is the whole of what
a @plain@ keyword is. A @conditional@ one is the Tension shape: a condition and
a result, where satisfying the condition once activates the result for as long
as the card stays in play, and @token@ names the token that marks it.
-}
mkEntity
  $(discoverEntities)
  [persistLowerCase|
ArkhamCustomCardSet sql=arkham_custom_card_sets
  Id UUID default=uuid_generate_v4()
  userId UserId OnDeleteCascade
  name Text
  description Text Maybe
  url Text Maybe
  sourceCode Text Maybe
  investigatorExpansion Bool default=False
  icon Text Maybe
  tokens Value
  keywords Value
  createdAt UTCTime
  updatedAt UTCTime
  UniqueUserCustomCardSetName userId name
  deriving Generic Show
|]

instance ToJSON ArkhamCustomCardSet where
  toJSON = genericToJSON $ aesonOptions $ Just "arkhamCustomCardSet"
  toEncoding = genericToEncoding $ aesonOptions $ Just "arkhamCustomCardSet"

{- | Who has been let into which feature, and letting them in.

A feature rolls out to a few accounts at a time, so the two things this panel has
to answer are "who has it already" and "give it to this person". With no @q@ it
answers the first -- every account with any grant on it, which is a short list by
definition -- and with one it matches on username and email alike, because the
person asking to be let in may have reached you under either.

The write sets the whole list rather than toggling one name, because the panel
sends the state of its checkboxes: a toggle would have to agree with what the
page was showing, and the page may have been open since before the last change.

An unknown feature name is refused rather than dropped. 'Feature.parseFeatures'
would quietly ignore it, and a grant that reported success while granting nothing
is the failure this whole mechanism exists to make visible.
-}
module Api.Handler.Arkham.Admin.Users (
  getApiV1AdminUsersR,
  putApiV1AdminUserFeaturesR,
) where

import Auth.Feature qualified as Feature
import Data.Text qualified as T
import Database.Esqueleto.Experimental
import Database.Persist qualified as P
import Import hiding (update, (!=.), (=.), (==.), (||.))

data UserRow = UserRow
  { userRowId :: UserId
  , userRowUsername :: Text
  , userRowEmail :: Text
  {- ^ Shown because it is searched: a row that matched on email has to say so,
  or two accounts with similar names are indistinguishable.
  -}
  , userRowAdmin :: Bool
  , userRowFeatures :: [Feature.Feature]
  }

instance ToJSON UserRow where
  toJSON UserRow {..} =
    object
      [ "id" .= userRowId
      , "username" .= userRowUsername
      , "email" .= userRowEmail
      , "admin" .= userRowAdmin
      , "features" .= userRowFeatures
      ]

data UsersResponse = UsersResponse
  { usersResponseAllFeatures :: [Feature.Feature]
  {- ^ What may be granted, so the panel renders a checkbox per feature rather
  than a hardcoded list that drifts from 'Feature.allFeatures'.
  -}
  , usersResponseUsers :: [UserRow]
  }

instance ToJSON UsersResponse where
  toJSON UsersResponse {..} =
    object
      [ "allFeatures" .= usersResponseAllFeatures
      , "users" .= usersResponseUsers
      ]

toRow :: Entity User -> UserRow
toRow (Entity userId user) =
  UserRow
    { userRowId = userId
    , userRowUsername = user.username
    , userRowEmail = user.email
    , userRowAdmin = user.admin
    , userRowFeatures = Feature.parseFeatures user.features
    }

{- | How many accounts a search may return. A search is meant to find one
person, not to page through the users table.
-}
searchLimit :: Int64
searchLimit = 25

getApiV1AdminUsersR :: Handler UsersResponse
getApiV1AdminUsersR = do
  query <- maybe "" T.strip <$> lookupGetParam "q"
  rows <-
    runDB
      if T.null query
        then select do
          u <- from $ table @User
          where_ $ u.features !=. val ""
          orderBy [asc u.username]
          pure u
        else select do
          u <- from $ table @User
          -- Lowered on both sides rather than matched with ILIKE: the admin is
          -- typing something they were given, and the case it was given in is
          -- not something they should have to reproduce. Anywhere in the value
          -- rather than at the front of it, because an email is as likely to be
          -- remembered by its domain as by its local part.
          let fragment = (%) ++. val (T.toLower query) ++. (%)
          -- Parenthesised: `like` and `||.` are both infixr 2, so without them
          -- this groups as `username `like` (fragment ||. ...)`.
          where_ $ (lower_ u.username `like` fragment) ||. (lower_ u.email `like` fragment)
          orderBy [asc u.username]
          limit searchLimit
          pure u
  pure
    $ UsersResponse
      { usersResponseAllFeatures = Feature.allFeatures
      , usersResponseUsers = map toRow rows
      }

newtype FeaturesPut = FeaturesPut {features :: [Text]}
  deriving stock Generic
  deriving anyclass FromJSON

putApiV1AdminUserFeaturesR :: UserId -> Handler UserRow
putApiV1AdminUserFeaturesR userId = do
  FeaturesPut {features} <- requireCheckJsonBody
  let unknown = filter (`notElem` Feature.allFeatures) features
  unless (null unknown)
    $ invalidArgs
      [ "No such feature: "
          <> T.unwords unknown
          <> ". Known features: "
          <> Feature.renderFeatures Feature.allFeatures
      ]
  user <- runDB do
    void $ get404 userId
    P.update userId [UserFeatures P.=. Feature.renderFeatures features]
    get404 userId
  pure $ toRow (Entity userId user)

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

The third thing it answers is "what has this account been playing", because the
question that reaches you about an account is almost always about a game on it.
A game is reported as active or not from its own state rather than from whether
a room is open for it: a room is who is connected right now, and an account that
has not been seen for a week still has the games it left in progress.
-}
module Api.Handler.Arkham.Admin.Users (
  getApiV1AdminUsersR,
  getApiV1AdminUserGamesR,
  putApiV1AdminUserFeaturesR,
) where

import Api.Handler.Arkham.Games.Shared (
  GameDetails (..),
  GameDetailsEntry (..),
  tryGameDetailsEntry,
 )
import Arkham.Game.State (GameState (IsOver))
import Auth.Feature qualified as Feature
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time.Clock (UTCTime)
import Database.Esqueleto.Experimental
import Database.Persist qualified as P
import Entity.Arkham.GameRaw
import Import hiding (on, update, (!=.), (=.), (==.), (||.))

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

{- | How many of an account's games to report, most recently played first.

Decoding a game row means decoding the whole game blob, so this is the one
number that decides what the panel costs. Fifty is far more than anybody needs
to answer "what is this person playing", and the response says how many there
are in total so a truncated list says so rather than looking complete.
-}
gamesLimit :: Int64
gamesLimit = 50

data AdminUserGame = AdminUserGame
  { adminUserGameActive :: Bool
  , adminUserGameCreatedAt :: UTCTime
  , adminUserGameUpdatedAt :: UTCTime
  , adminUserGameGame :: GameDetailsEntry
  }

instance ToJSON AdminUserGame where
  toJSON AdminUserGame {..} =
    object
      [ "active" .= adminUserGameActive
      , "createdAt" .= adminUserGameCreatedAt
      , "updatedAt" .= adminUserGameUpdatedAt
      , "game" .= adminUserGameGame
      ]

data UserGamesResponse = UserGamesResponse
  { userGamesResponseTotal :: Int
  -- ^ Every game on the account, not just the ones below 'gamesLimit'.
  , userGamesResponseGames :: [AdminUserGame]
  }

instance ToJSON UserGamesResponse where
  toJSON UserGamesResponse {..} =
    object
      [ "total" .= userGamesResponseTotal
      , "games" .= userGamesResponseGames
      ]

toAdminUserGame :: MonadIO m => Map ArkhamGameId Int -> Entity ArkhamGameRaw -> m AdminUserGame
toAdminUserGame countMap game = do
  entry <- tryGameDetailsEntry game (fromMaybe 0 $ Map.lookup (coerce $ entityKey game) countMap)
  pure
    $ AdminUserGame
      { adminUserGameActive = isActiveEntry entry
      , adminUserGameCreatedAt = (entityVal game).createdAt
      , adminUserGameUpdatedAt = (entityVal game).updatedAt
      , adminUserGameGame = entry
      }

{- | A game that failed to decode is reported as inactive. Nobody can play it --
that is what the failure means -- and the panel still lists it, so a broken game
on an account is visible rather than quietly missing from both halves.
-}
isActiveEntry :: GameDetailsEntry -> Bool
isActiveEntry = \case
  FailedGameDetails {} -> False
  SuccessGameDetails GameDetails {gameState} -> gameState /= IsOver

getApiV1AdminUserGamesR :: UserId -> Handler UserGamesResponse
getApiV1AdminUserGamesR userId = do
  (total, games, countMap) <- runDB do
    void $ get404 userId
    total <-
      fmap (sum . map unValue . toList) . selectOne $ do
        players <- from $ table @ArkhamPlayer
        where_ $ players.userId ==. val userId
        pure (countRows @Int)

    games <- select do
      (players :& games) <-
        from
          $ table @ArkhamPlayer
          `innerJoin` table @ArkhamGameRaw
            `on` (\(players :& games) -> players.arkhamGameId ==. toBaseId games.id)
      where_ $ players.userId ==. val userId
      orderBy [desc games.updatedAt]
      limit gamesLimit
      pure games

    -- Seat counts, for `hasOpenSeats`: the one field of a game row that is not
    -- in the game blob.
    let gameIds = map (coerce . entityKey) games :: [ArkhamGameId]
    playerCounts <-
      if null gameIds
        then pure []
        else select do
          p <- from $ table @ArkhamPlayer
          where_ $ p.arkhamGameId `in_` valList gameIds
          groupBy p.arkhamGameId
          pure (p.arkhamGameId, countRows @Int)

    pure (total, games, Map.fromList [(gid, n) | (Value gid, Value n) <- playerCounts])

  -- Decoded outside the transaction: a game blob is large and this is the
  -- expensive half, so it should not be holding a connection.
  rows <- traverse (toAdminUserGame countMap) games

  pure $ UserGamesResponse {userGamesResponseTotal = total, userGamesResponseGames = rows}

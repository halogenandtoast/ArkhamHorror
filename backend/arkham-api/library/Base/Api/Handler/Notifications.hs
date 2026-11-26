{- | Site notifications: the announcements every player sees on the home page.

The public read is unauthenticated-ish and unfiltered -- every row is live the
moment it exists, and nobody can dismiss one. That shapes the admin endpoints
below: there is no draft state to save into, so creating a notification
publishes it, taking one down is a delete, and an edit reaches every player who
loads the page after it.

A notification may be filed under a 'notificationCategories' category, which is
what the home page stamps it with. No category is an ordinary announcement.
-}
module Base.Api.Handler.Notifications (
  getApiV1NotificationsR,
  getApiV1AdminNotificationsR,
  postApiV1AdminNotificationsR,
  getApiV1AdminNotificationR,
  putApiV1AdminNotificationR,
  deleteApiV1AdminNotificationR,
) where

import Data.Aeson (withObject, (.:!), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist
import Entity.Notification
import Import hiding (delete, update, (=.), (==.))
import Import qualified as P

-- | As long a body as an announcement can usefully hold.
maxBodyLength :: Int
maxBodyLength = 4000

{- | The categories an announcement can be filed under, and the only values the
column may hold besides NULL.

Kept here rather than in a sum type so that the one place a bad value can enter
-- a write -- is the one place that rejects it; see "Entity.Notification". The
home page knows the same three and styles each one; a category it does not know
would simply go unstamped, so the risk of the two lists drifting is cosmetic.
-}
notificationCategories :: [Text]
notificationCategories = ["important", "feature", "content"]

getApiV1NotificationsR :: Handler [Entity Notification]
getApiV1NotificationsR = runDB $ selectList [] [Desc NotificationCreatedAt]

{- | What the admin panel lists.

The same shape and the same order as the public read, so the panel shows the
notifications in the order players meet them rather than an order of its own.
-}
getApiV1AdminNotificationsR :: Handler [Entity Notification]
getApiV1AdminNotificationsR = runDB $ P.selectList [] [P.Desc NotificationCreatedAt]

getApiV1AdminNotificationR :: NotificationId -> Handler (Entity Notification)
getApiV1AdminNotificationR notificationId =
  Entity notificationId <$> runDB (get404 notificationId)

data NotificationPost = NotificationPost
  { notificationPostBody :: Text
  , notificationPostCategory :: Maybe Text
  -- ^ One of 'notificationCategories', or left out for an announcement with
  -- no stamp on it.
  , notificationPostCreatedAt :: Maybe UTCTime
  {- ^ Normally left out and taken as now. Settable because the list is ordered
  by it, which makes it the only way to put a notification above one that is
  already up.
  -}
  }

instance FromJSON NotificationPost where
  parseJSON = withObject "NotificationPost" \o ->
    NotificationPost <$> o .: "body" <*> o .:? "category" <*> o .:? "createdAt"

{- | Write a notification, which is the same thing as showing it to everyone.

Returned rather than just acknowledged: the caller needs the id, since that is
what every later edit and the players' dismissals are keyed by.
-}
postApiV1AdminNotificationsR :: Handler (Entity Notification)
postApiV1AdminNotificationsR = do
  post <- requireCheckJsonBody
  body <- checkedBody (notificationPostBody post)
  category <- checkedCategory (notificationPostCategory post)
  now <- liftIO getCurrentTime
  let notification =
        Notification
          { notificationBody = body
          , notificationCategory = category
          , notificationCreatedAt = fromMaybe now (notificationPostCreatedAt post)
          }
  notificationId <- runDB $ P.insert notification
  pure $ Entity notificationId notification

data NotificationPut = NotificationPut
  { notificationPutBody :: Maybe Text
  , notificationPutCategory :: Maybe (Maybe Text)
  {- ^ Two layers, because filing a notification under nothing has to be
  expressible: the key left out is "leave the category alone", and the key sent
  as @null@ is "take the stamp off". Read with '.:!' rather than '.:?', which
  answers @Nothing@ for both and would leave the stamp on -- or, when the
  category was the only edit, reject it as nothing to change.
  -}
  , notificationPutCreatedAt :: Maybe UTCTime
  }

instance FromJSON NotificationPut where
  parseJSON = withObject "NotificationPut" \o ->
    NotificationPut <$> o .:? "body" <*> o .:! "category" <*> o .:? "createdAt"

{- | Edit a notification in place.

Every field is optional and only what is sent is written, so re-dating one does
not require sending its body back unchanged. An edit reaches every player who
loads the home page after it.
-}
putApiV1AdminNotificationR :: NotificationId -> Handler (Entity Notification)
putApiV1AdminNotificationR notificationId = do
  void $ runDB $ get404 notificationId
  edit <- requireCheckJsonBody
  bodyUpdate <- traverse (fmap (NotificationBody P.=.) . checkedBody) (notificationPutBody edit)
  categoryUpdate <-
    traverse (fmap (NotificationCategory P.=.) . checkedCategory) (notificationPutCategory edit)
  let dateUpdate = (NotificationCreatedAt P.=.) <$> notificationPutCreatedAt edit
      updates = catMaybes [bodyUpdate, categoryUpdate, dateUpdate]
  when (null updates) $ invalidArgs ["Nothing to change: send a body, a category, a createdAt, or any of them"]
  Entity notificationId <$> runDB (P.updateGet notificationId updates)

{- | Take a notification down.

Deleted rather than hidden, because there is nothing to hide behind: the public
read has no filter, so a notification exists or it does not.
-}
deleteApiV1AdminNotificationR :: NotificationId -> Handler Aeson.Value
deleteApiV1AdminNotificationR notificationId = do
  void $ runDB $ get404 notificationId
  runDB $ P.delete notificationId
  pure $ Aeson.object ["deleted" Aeson..= notificationId]

{- | A category the home page knows, or none.

An empty string is taken as none rather than refused: it is what a @<select>@
with nothing chosen sends, and "no category" is exactly what that means.
-}
checkedCategory :: Maybe Text -> Handler (Maybe Text)
checkedCategory raw = case T.toLower . T.strip <$> raw of
  Nothing -> pure Nothing
  Just "" -> pure Nothing
  Just category -> do
    unless (category `elem` notificationCategories) $
      invalidArgs ["Unknown category: " <> category <> ". Use one of " <> T.intercalate ", " notificationCategories]
    pure (Just category)

-- | A body that is actually a body. Admin-only, so it is trusted as markup.
checkedBody :: Text -> Handler Text
checkedBody raw = do
  let body = T.strip raw
  when (T.null body) $ invalidArgs ["A notification needs something to say"]
  when (T.length body > maxBodyLength) $ invalidArgs ["That notification is too long"]
  pure body

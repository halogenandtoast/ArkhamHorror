{- | Site notifications: the banner every player sees on the home page.

The public read is unauthenticated-ish and unfiltered -- every row is live the
moment it exists, and dismissal is per-browser in @localStorage@ keyed by row id.
That has two consequences the admin endpoints below are shaped around: there is
no draft state to save into, so creating a notification publishes it; and editing
one does not bring it back for anyone who dismissed it, because the id is what
they dismissed. Re-announcing something means a new row.
-}
module Base.Api.Handler.Notifications (
  getApiV1NotificationsR,
  getApiV1AdminNotificationsR,
  postApiV1AdminNotificationsR,
  getApiV1AdminNotificationR,
  putApiV1AdminNotificationR,
  deleteApiV1AdminNotificationR,
) where

import Data.Aeson (withObject, (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time.Clock
import Database.Persist
import Entity.Notification
import Import hiding (delete, update, (=.), (==.))
import Import qualified as P

-- | As long a body as a banner can usefully hold.
maxBodyLength :: Int
maxBodyLength = 4000

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
  , notificationPostCreatedAt :: Maybe UTCTime
  {- ^ Normally left out and taken as now. Settable because the list is ordered
  by it, which makes it the only way to put a notification above one that is
  already up.
  -}
  }

instance FromJSON NotificationPost where
  parseJSON = withObject "NotificationPost" \o ->
    NotificationPost <$> o .: "body" <*> o .:? "createdAt"

{- | Write a notification, which is the same thing as showing it to everyone.

Returned rather than just acknowledged: the caller needs the id, since that is
what every later edit and the players' dismissals are keyed by.
-}
postApiV1AdminNotificationsR :: Handler (Entity Notification)
postApiV1AdminNotificationsR = do
  post <- requireCheckJsonBody
  body <- checkedBody (notificationPostBody post)
  now <- liftIO getCurrentTime
  let notification =
        Notification
          { notificationBody = body
          , notificationCreatedAt = fromMaybe now (notificationPostCreatedAt post)
          }
  notificationId <- runDB $ P.insert notification
  pure $ Entity notificationId notification

data NotificationPut = NotificationPut
  { notificationPutBody :: Maybe Text
  , notificationPutCreatedAt :: Maybe UTCTime
  }

instance FromJSON NotificationPut where
  parseJSON = withObject "NotificationPut" \o ->
    NotificationPut <$> o .:? "body" <*> o .:? "createdAt"

{- | Edit a notification in place.

Both fields are optional and only what is sent is written, so re-dating one does
not require sending its body back unchanged. An edit reaches anybody still
seeing it; it does not reach anybody who already dismissed it.
-}
putApiV1AdminNotificationR :: NotificationId -> Handler (Entity Notification)
putApiV1AdminNotificationR notificationId = do
  void $ runDB $ get404 notificationId
  edit <- requireCheckJsonBody
  bodyUpdate <- traverse (fmap (NotificationBody P.=.) . checkedBody) (notificationPutBody edit)
  let dateUpdate = (NotificationCreatedAt P.=.) <$> notificationPutCreatedAt edit
      updates = catMaybes [bodyUpdate, dateUpdate]
  when (null updates) $ invalidArgs ["Nothing to change: send a body, a createdAt, or both"]
  Entity notificationId <$> runDB (P.updateGet notificationId updates)

{- | Take a notification down.

Deleted rather than hidden, because there is nothing to hide behind: the public
read has no filter, so a notification exists or it does not. Players who
dismissed it keep its id in @localStorage@ against nothing, which costs them
nothing.
-}
deleteApiV1AdminNotificationR :: NotificationId -> Handler Aeson.Value
deleteApiV1AdminNotificationR notificationId = do
  void $ runDB $ get404 notificationId
  runDB $ P.delete notificationId
  pure $ Aeson.object ["deleted" Aeson..= notificationId]

-- | A body that is actually a body. Admin-only, so it is trusted as markup.
checkedBody :: Text -> Handler Text
checkedBody raw = do
  let body = T.strip raw
  when (T.null body) $ invalidArgs ["A notification needs something to say"]
  when (T.length body > maxBodyLength) $ invalidArgs ["That notification is too long"]
  pure body

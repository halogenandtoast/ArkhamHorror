module Base.Api.Handler.Notifications where

import Database.Persist
import Entity.Notification
import Import

getApiV1NotificationsR :: Handler [Entity Notification]
getApiV1NotificationsR = runDB $ selectList [] [Desc NotificationCreatedAt]

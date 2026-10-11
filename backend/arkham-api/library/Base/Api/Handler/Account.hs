module Base.Api.Handler.Account where

import Api.Handler.Arkham.BuildConnection (disconnectArkhamBuild)
import Database.Esqueleto.Experimental
import Import hiding (delete, (==.))

deleteApiV1AccountR :: Handler ()
deleteApiV1AccountR = do
  userId <- getRequestUserId
  -- The row cascades away with the user, but the grant at arkham.build would
  -- stay live until its refresh token expired.
  disconnectArkhamBuild userId
  runDB do
    delete do
      resets <- from $ table @PasswordReset
      where_ $ resets.userId ==. val userId
    deleteKey userId

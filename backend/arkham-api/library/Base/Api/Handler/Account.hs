module Base.Api.Handler.Account where

import Database.Esqueleto.Experimental
import Import hiding (delete, (==.))

deleteApiV1AccountR :: Handler ()
deleteApiV1AccountR = do
  userId <- getRequestUserId
  runDB do
    delete do
      resets <- from $ table @PasswordReset
      where_ $ resets.userId ==. val userId
    deleteKey userId

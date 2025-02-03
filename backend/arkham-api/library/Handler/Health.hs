{-# LANGUAGE PackageImports #-}
module Handler.Health (
  getHealthR,
  getErrorR,
) where

import Import
import "bugsnag-hs" Network.Bugsnag (exception_message, defaultException)

getHealthR :: Handler ()
getHealthR = pure ()

getErrorR :: Handler ()
getErrorR = bugsnag $ defaultException
  { exception_message = Just "This is an error message"
  }

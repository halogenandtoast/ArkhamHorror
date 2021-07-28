module Handler.Health
  ( getHealthR
  ) where

import Import hiding (delete, on, (==.))

getHealthR :: Handler ()
getHealthR = pure ()

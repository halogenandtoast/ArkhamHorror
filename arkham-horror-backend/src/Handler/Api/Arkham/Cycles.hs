{-# LANGUAGE NoImplicitPrelude #-}
module Handler.Api.Arkham.Cycles where

import           Import

getApiV1ArkhamCyclesR :: Handler [Entity ArkhamHorrorCycle]
getApiV1ArkhamCyclesR = runDB $ selectList [] []

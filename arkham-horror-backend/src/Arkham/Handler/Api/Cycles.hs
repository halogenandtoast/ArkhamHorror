{-# LANGUAGE NoImplicitPrelude #-}
module Arkham.Handler.Api.Cycles where

import           Import

getApiV1ArkhamCyclesR :: Handler [Entity ArkhamHorrorCycle]
getApiV1ArkhamCyclesR = runDB $ selectList [] []

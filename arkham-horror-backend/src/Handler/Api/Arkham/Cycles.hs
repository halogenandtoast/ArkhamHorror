{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.Api.Arkham.Cycles where

import Import

newtype Cycle = Cycle { getCycle :: Text }
  deriving newtype (ToJSON)

getApiV1ArkhamCyclesR :: Handler [Cycle]
getApiV1ArkhamCyclesR = pure [ Cycle "Night of the Zealot", Cycle "The Dunwich Legacy" ]

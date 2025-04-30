{-# LANGUAGE TemplateHaskell #-}

module Arkham.Agenda.AdvancementReason where

import Arkham.Prelude
import Data.Aeson.TH
import GHC.OverloadedLabels

data AgendaAdvancementReason = DoomThreshold
  deriving stock (Show, Eq, Ord, Data)

instance IsLabel "doom" AgendaAdvancementReason where
  fromLabel = DoomThreshold

$(deriveJSON defaultOptions ''AgendaAdvancementReason)

{-# LANGUAGE TemplateHaskell #-}
module Arkham.Agenda.AdvancementReason where

import Arkham.Prelude

import Data.Aeson.TH

data AgendaAdvancementReason = DoomThreshold
  deriving stock (Show, Eq, Ord)

$(deriveJSON defaultOptions ''AgendaAdvancementReason)

module Arkham.Types.Agenda.AdvancementReason where

import Arkham.Prelude

data AgendaAdvancementReason = DoomThreshold
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON, Hashable)

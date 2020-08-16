{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TimeIsRunningShort where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype TimeIsRunningShort = TimeIsRunningShort Attrs
  deriving newtype (Show, ToJSON, FromJSON)

timeIsRunningShort :: TimeIsRunningShort
timeIsRunningShort = TimeIsRunningShort
  $ baseAttrs "01122" "Time Is Running Short" "Agenda 2a" (Static 8)

instance (AgendaRunner env) => RunMessage env TimeIsRunningShort where
  runMessage msg a@(TimeIsRunningShort attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" -> do
      void $ error "Resolution 2"
      pure a
    _ -> TimeIsRunningShort <$> runMessage msg attrs

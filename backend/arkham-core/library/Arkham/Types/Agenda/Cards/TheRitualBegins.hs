{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheRitualBegins where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype TheRitualBegins = TheRitualBegins Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theRitualBegins :: TheRitualBegins
theRitualBegins =
  TheRitualBegins $ baseAttrs "01144" "The Ritual Begins" "Agenda 2a" (Static 5)

instance HasActions env investigator TheRitualBegins where
  getActions i window (TheRitualBegins x) = getActions i window x

instance (AgendaRunner env) => RunMessage env TheRitualBegins where
  runMessage msg (TheRitualBegins attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 2a" ->
      error "TODO"
    _ -> TheRitualBegins <$> runMessage msg attrs

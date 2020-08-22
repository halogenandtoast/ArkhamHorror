{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.TheArkhamWoods where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype TheArkhamWoods = TheArkhamWoods Attrs
  deriving newtype (Show, ToJSON, FromJSON)

theArkhamWoods :: TheArkhamWoods
theArkhamWoods =
  TheArkhamWoods $ baseAttrs "01143" "The Arkham Woods" "Agenda 1a" (Static 4)

instance HasActions env investigator TheArkhamWoods where
  getActions i window (TheArkhamWoods x) = getActions i window x

instance (AgendaRunner env) => RunMessage env TheArkhamWoods where
  runMessage msg (TheArkhamWoods attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 1a" ->
      error "TODO"
    _ -> TheArkhamWoods <$> runMessage msg attrs

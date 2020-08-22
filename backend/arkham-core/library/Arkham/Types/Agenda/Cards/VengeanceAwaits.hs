{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Agenda.Cards.VengeanceAwaits where

import Arkham.Json
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import ClassyPrelude hiding (sequence)

newtype VengeanceAwaits = VengeanceAwaits Attrs
  deriving newtype (Show, ToJSON, FromJSON)

vengeanceAwaits :: VengeanceAwaits
vengeanceAwaits =
  VengeanceAwaits $ baseAttrs "01145" "Vengeance Awaits" "Agenda 3a" (Static 5)

instance HasActions env investigator VengeanceAwaits where
  getActions i window (VengeanceAwaits x) = getActions i window x

instance (AgendaRunner env) => RunMessage env VengeanceAwaits where
  runMessage msg (VengeanceAwaits attrs@Attrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == "Agenda 3a" ->
      error "TODO"
    _ -> VengeanceAwaits <$> runMessage msg attrs

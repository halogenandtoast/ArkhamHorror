module Arkham.Types.Agenda.Cards.TheFestivitiesBegin
  ( TheFestivitiesBegin
  , theFestivitiesBegin
  ) where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype TheFestivitiesBegin = TheFestivitiesBegin AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theFestivitiesBegin :: TheFestivitiesBegin
theFestivitiesBegin = TheFestivitiesBegin
  $ baseAttrs "82002" "The Festivities Begin" (Agenda 1 A) (Static 8)

instance HasModifiersFor env TheFestivitiesBegin

instance HasActions env TheFestivitiesBegin where
  getActions i window (TheFestivitiesBegin x) = getActions i window x

instance AgendaRunner env => RunMessage env TheFestivitiesBegin where
  runMessage msg a@(TheFestivitiesBegin attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
      a <$ pushAll [NextAgenda aid "82003"]
    _ -> TheFestivitiesBegin <$> runMessage msg attrs

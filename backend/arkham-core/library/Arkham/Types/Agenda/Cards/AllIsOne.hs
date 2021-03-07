module Arkham.Types.Agenda.Cards.AllIsOne
  ( AllIsOne
  , allIsOne
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype AllIsOne = AllIsOne AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

allIsOne :: AllIsOne
allIsOne = AllIsOne $ baseAttrs "02312" "All is One" (Agenda 1 A) (Static 4)

instance HasModifiersFor env AllIsOne where
  getModifiersFor = noModifiersFor

instance HasActions env AllIsOne where
  getActions i window (AllIsOne x) = getActions i window x

instance AgendaRunner env => RunMessage env AllIsOne where
  runMessage msg a@(AllIsOne attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
      a <$ unshiftMessages [NextAgenda aid "02313"]
    _ -> AllIsOne <$> runMessage msg attrs

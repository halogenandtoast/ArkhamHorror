module Arkham.Types.Agenda.Cards.OutOfTime
  ( OutOfTime(..)
  , outOfTime
  )
where

import Arkham.Prelude

import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Resolution
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype OutOfTime = OutOfTime AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfTime :: OutOfTime
outOfTime = OutOfTime $ baseAttrs "02164" "Out of Time" (Agenda 5 A) (Static 3)

instance HasModifiersFor env OutOfTime where
  getModifiersFor = noModifiersFor

instance HasActions env OutOfTime where
  getActions i window (OutOfTime x) = getActions i window x

instance AgendaRunner env => RunMessage env OutOfTime where
  runMessage msg a@(OutOfTime attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 5 B -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ unshiftMessages
        ([ InvestigatorDefeated iid | iid <- investigatorIds ]
        <> [ SufferTrauma iid 0 1 | iid <- investigatorIds ]
        <> [ScenarioResolution $ Resolution 2]
        )
    _ -> OutOfTime <$> runMessage msg attrs

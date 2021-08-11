module Arkham.Types.Agenda.Cards.OutOfTime
  ( OutOfTime(..)
  , outOfTime
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype OutOfTime = OutOfTime AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

outOfTime :: AgendaCard OutOfTime
outOfTime = agenda (5, A) OutOfTime Cards.outOfTime (Static 3)

instance HasModifiersFor env OutOfTime
instance HasActions OutOfTime

instance AgendaRunner env => RunMessage env OutOfTime where
  runMessage msg a@(OutOfTime attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 5 B -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ pushAll
        ([ InvestigatorDefeated iid | iid <- investigatorIds ]
        <> [ SufferTrauma iid 0 1 | iid <- investigatorIds ]
        <> [ScenarioResolution $ Resolution 2]
        )
    _ -> OutOfTime <$> runMessage msg attrs

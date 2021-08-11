module Arkham.Types.Agenda.Cards.TheOldOnesHunger
  ( TheOldOnesHunger(..)
  , theOldOnesHunger
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Query

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: AgendaCard TheOldOnesHunger
theOldOnesHunger =
  agenda (2, A) TheOldOnesHunger Cards.theOldOnesHunger (Static 6)

instance HasModifiersFor env TheOldOnesHunger
instance HasActions TheOldOnesHunger

instance AgendaRunner env => RunMessage env TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      scenarioDeckCount <- unScenarioDeckCount <$> getCount ()
      if scenarioDeckCount >= 2
        then a <$ pushAll
          [ UseScenarioSpecificAbility leadInvestigatorId Nothing 1
          , NextAgenda agendaId "02198"
          ]
        else a <$ push (NextAgenda agendaId "02198")
    _ -> TheOldOnesHunger <$> runMessage msg attrs

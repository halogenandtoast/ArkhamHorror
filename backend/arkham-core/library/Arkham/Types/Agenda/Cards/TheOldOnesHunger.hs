module Arkham.Types.Agenda.Cards.TheOldOnesHunger
  ( TheOldOnesHunger(..)
  , theOldOnesHunger
  )
where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype TheOldOnesHunger = TheOldOnesHunger AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theOldOnesHunger :: TheOldOnesHunger
theOldOnesHunger = TheOldOnesHunger
  $ baseAttrs "02197" "The Old Ones Hunger" (Agenda 2 A) (Static 6)

instance HasModifiersFor env TheOldOnesHunger where
  getModifiersFor = noModifiersFor

instance HasActions env TheOldOnesHunger where
  getActions i window (TheOldOnesHunger x) = getActions i window x

instance AgendaRunner env => RunMessage env TheOldOnesHunger where
  runMessage msg a@(TheOldOnesHunger attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      leadInvestigatorId <- unLeadInvestigatorId <$> getId ()
      scenarioDeckCount <- unScenarioDeckCount <$> getCount ()
      if scenarioDeckCount >= 2
        then a <$ unshiftMessages
          [ UseScenarioSpecificAbility leadInvestigatorId 1
          , NextAgenda agendaId "02198"
          ]
        else a <$ unshiftMessage (NextAgenda agendaId "02198")
    _ -> TheOldOnesHunger <$> runMessage msg attrs

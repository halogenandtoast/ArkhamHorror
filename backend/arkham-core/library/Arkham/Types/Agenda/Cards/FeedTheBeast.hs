module Arkham.Types.Agenda.Cards.FeedTheBeast
  ( FeedTheBeast(..)
  , feedTheBeast
  )
where


import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner

newtype FeedTheBeast = FeedTheBeast AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheBeast :: FeedTheBeast
feedTheBeast =
  FeedTheBeast $ baseAttrs "02198" "Feed the Beast" (Agenda 3 A) (Static 7)

instance HasModifiersFor env FeedTheBeast where
  getModifiersFor = noModifiersFor

instance HasActions env FeedTheBeast where
  getActions i window (FeedTheBeast x) = getActions i window x

instance AgendaRunner env => RunMessage env FeedTheBeast where
  runMessage msg a@(FeedTheBeast attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ unshiftMessages [ Resign iid | iid <- investigatorIds ]
    _ -> FeedTheBeast <$> runMessage msg attrs

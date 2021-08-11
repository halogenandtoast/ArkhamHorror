module Arkham.Types.Agenda.Cards.FeedTheBeast
  ( FeedTheBeast(..)
  , feedTheBeast
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.InvestigatorId
import Arkham.Types.Message

newtype FeedTheBeast = FeedTheBeast AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheBeast :: AgendaCard FeedTheBeast
feedTheBeast = agenda (3, A) FeedTheBeast Cards.feedTheBeast (Static 7)

instance HasActions FeedTheBeast
instance HasModifiersFor env FeedTheBeast

instance AgendaRunner env => RunMessage env FeedTheBeast where
  runMessage msg a@(FeedTheBeast attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ pushAll [ Resign iid | iid <- investigatorIds ]
    _ -> FeedTheBeast <$> runMessage msg attrs

module Arkham.Agenda.Cards.FeedTheBeast
  ( FeedTheBeast(..)
  , feedTheBeast
  ) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Attrs
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.InvestigatorId
import Arkham.Message

newtype FeedTheBeast = FeedTheBeast AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheBeast :: AgendaCard FeedTheBeast
feedTheBeast = agenda (3, A) FeedTheBeast Cards.feedTheBeast (Static 7)

instance AgendaRunner env => RunMessage FeedTheBeast where
  runMessage msg a@(FeedTheBeast attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B -> do
      investigatorIds <- map unInScenarioInvestigatorId <$> getSetList ()
      a <$ pushAll [ Resign iid | iid <- investigatorIds ]
    _ -> FeedTheBeast <$> runMessage msg attrs

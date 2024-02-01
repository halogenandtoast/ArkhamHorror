module Arkham.Agenda.Cards.FeedTheBeast (
  FeedTheBeast (..),
  feedTheBeast,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher

newtype FeedTheBeast = FeedTheBeast AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

feedTheBeast :: AgendaCard FeedTheBeast
feedTheBeast = agenda (3, A) FeedTheBeast Cards.feedTheBeast (Static 7)

instance RunMessage FeedTheBeast where
  runMessage msg a@(FeedTheBeast attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && onSide B attrs -> do
      investigatorIds <- selectList UneliminatedInvestigator
      a <$ pushAll [Resign iid | iid <- investigatorIds]
    _ -> FeedTheBeast <$> runMessage msg attrs

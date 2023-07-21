module Arkham.Agenda.Cards.TheTempleWarden (
  TheTempleWarden (..),
  theTempleWarden,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Matcher hiding (InvestigatorDefeated)
import Arkham.Message

newtype TheTempleWarden = TheTempleWarden AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTempleWarden :: AgendaCard TheTempleWarden
theTempleWarden =
  agenda (2, A) TheTempleWarden Cards.theTempleWarden (StaticWithPerPlayer 12 1)

instance RunMessage TheTempleWarden where
  runMessage msg a@(TheTempleWarden attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      iids <- selectList UneliminatedInvestigator
      pushAll $ map (InvestigatorDefeated (toSource attrs)) iids
      pure a
    _ -> TheTempleWarden <$> runMessage msg attrs

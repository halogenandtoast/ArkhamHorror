module Arkham.Types.Agenda.Cards.TheTerrifyingTruth
  ( TheTerrifyingTruth
  , theTerrifyingTruth
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message

newtype TheTerrifyingTruth = TheTerrifyingTruth AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theTerrifyingTruth :: AgendaCard TheTerrifyingTruth
theTerrifyingTruth =
  agenda (2, A) TheTerrifyingTruth Cards.theTerrifyingTruth (Static 3)

instance AgendaRunner env => RunMessage env TheTerrifyingTruth where
  runMessage msg a@(TheTerrifyingTruth attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs ->
      a <$ pushAll [NextAgenda aid "TODO"]
    _ -> TheTerrifyingTruth <$> runMessage msg attrs

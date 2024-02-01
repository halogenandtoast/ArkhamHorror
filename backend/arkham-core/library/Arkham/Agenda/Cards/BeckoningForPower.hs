module Arkham.Agenda.Cards.BeckoningForPower (
  BeckoningForPower (..),
  beckoningForPower,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Classes
import Arkham.GameValue
import Arkham.Resolution

newtype BeckoningForPower = BeckoningForPower AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

beckoningForPower :: AgendaCard BeckoningForPower
beckoningForPower =
  agenda (2, A) BeckoningForPower Cards.beckoningForPower (Static 10)

instance RunMessage BeckoningForPower where
  runMessage msg a@(BeckoningForPower attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid
      | aid == agendaId && onSide B attrs ->
          a <$ push (ScenarioResolution $ Resolution 2)
    _ -> BeckoningForPower <$> runMessage msg attrs

module Arkham.Types.Agenda.Cards.BeckoningForPower
  ( BeckoningForPower
  , beckoningForPower
  ) where

import Arkham.Prelude

import qualified Arkham.Agenda.Cards as Cards
import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype BeckoningForPower = BeckoningForPower AgendaAttrs
  deriving anyclass IsAgenda
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beckoningForPower :: AgendaCard BeckoningForPower
beckoningForPower =
  agenda (2, A) BeckoningForPower Cards.beckoningForPower (Static 10)

instance HasModifiersFor env BeckoningForPower
instance HasActions BeckoningForPower

instance AgendaRunner env => RunMessage env BeckoningForPower where
  runMessage msg a@(BeckoningForPower attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 2 B ->
      a <$ push (ScenarioResolution $ Resolution 2)
    _ -> BeckoningForPower <$> runMessage msg attrs

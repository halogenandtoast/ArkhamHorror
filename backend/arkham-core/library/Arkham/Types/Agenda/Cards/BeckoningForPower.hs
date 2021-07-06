module Arkham.Types.Agenda.Cards.BeckoningForPower
  ( BeckoningForPower
  , beckoningForPower
  )
where

import Arkham.Prelude

import Arkham.Types.Agenda.Attrs
import Arkham.Types.Agenda.Runner
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Message
import Arkham.Types.Resolution

newtype BeckoningForPower = BeckoningForPower AgendaAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

beckoningForPower :: BeckoningForPower
beckoningForPower = BeckoningForPower
  $ baseAttrs "02276" "Beckoning for Power" (Agenda 2 A) (Static 10)

instance HasModifiersFor env BeckoningForPower where
  getModifiersFor = noModifiersFor

instance HasActions env BeckoningForPower where
  getActions i window (BeckoningForPower x) = getActions i window x

instance AgendaRunner env => RunMessage env BeckoningForPower where
  runMessage msg a@(BeckoningForPower attrs@AgendaAttrs {..}) = case msg of
    AdvanceAgenda aid | aid == agendaId && agendaSequence == Agenda 1 B ->
      a <$ push (ScenarioResolution $ Resolution 2)
    _ -> BeckoningForPower <$> runMessage msg attrs

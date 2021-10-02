module Arkham.Types.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message hiding (RevealToken)
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance EventRunner env => RunMessage env SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.When (RevealToken _ token)] _
      | eid == eventId -> e <$ pushAll
        [ CreateEffect "01088" Nothing (toSource attrs) (TokenTarget token)
        , Discard (toTarget attrs)
        ]
    _ -> SureGamble3 <$> runMessage msg attrs

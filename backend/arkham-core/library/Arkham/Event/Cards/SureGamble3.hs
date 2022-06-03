module Arkham.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Attrs
import Arkham.Event.Runner
import Arkham.Message hiding (RevealToken)
import Arkham.Target
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance EventRunner env => RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.When (RevealToken _ token)] _
      | eid == eventId -> e <$ pushAll
        [ CreateEffect "01088" Nothing (toSource attrs) (TokenTarget token)
        , Discard (toTarget attrs)
        ]
    _ -> SureGamble3 <$> runMessage msg attrs

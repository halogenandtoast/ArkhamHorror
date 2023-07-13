module Arkham.Event.Cards.SureGamble3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message hiding (RevealChaosToken)
import Arkham.Timing qualified as Timing
import Arkham.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance RunMessage SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ [Window Timing.When (RevealChaosToken _ token)] _
      | eid == eventId -> do
          pushAll
            [ CreateEffect "01088" Nothing (toSource attrs) (ChaosTokenTarget token)
            ]
          pure e
    _ -> SureGamble3 <$> runMessage msg attrs

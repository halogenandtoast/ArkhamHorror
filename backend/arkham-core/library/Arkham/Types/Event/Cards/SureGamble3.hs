module Arkham.Types.Event.Cards.SureGamble3 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Window

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: EventCard SureGamble3
sureGamble3 = event SureGamble3 Cards.sureGamble3

instance HasModifiersFor env SureGamble3

instance HasActions env SureGamble3 where
  getActions iid (InHandWindow ownerId (WhenRevealTokenWithNegativeModifier who token)) (SureGamble3 attrs)
    | ownerId == iid && iid == who
    = pure
      [InitiatePlayCard iid (toCardId attrs) (Just $ TokenTarget token) False]
  getActions i window (SureGamble3 attrs) = getActions i window attrs

instance EventRunner env => RunMessage env SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid _ [WhenRevealToken _ token] | eid == eventId ->
      e <$ pushAll
        [ CreateEffect "01088" Nothing (toSource attrs) (TokenTarget token)
        , Discard (toTarget attrs)
        ]
    _ -> SureGamble3 <$> runMessage msg attrs

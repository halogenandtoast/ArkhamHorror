module Arkham.Types.Event.Cards.SureGamble3 where


import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner

newtype SureGamble3 = SureGamble3 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sureGamble3 :: InvestigatorId -> EventId -> SureGamble3
sureGamble3 iid uuid = SureGamble3 $ baseAttrs iid uuid "01088"

instance HasModifiersFor env SureGamble3 where
  getModifiersFor = noModifiersFor

instance HasActions env SureGamble3 where
  getActions iid (InHandWindow ownerId (WhenRevealTokenWithNegativeModifier You tid)) (SureGamble3 attrs)
    | ownerId == iid
    = pure
      [InitiatePlayCard iid (getCardId attrs) (Just $ TokenTarget tid) False]
  getActions i window (SureGamble3 attrs) = getActions i window attrs

instance EventRunner env => RunMessage env SureGamble3 where
  runMessage msg e@(SureGamble3 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent _ eid (Just target@(TokenTarget _))
      | eid == eventId -> e <$ unshiftMessages
        [ CreateEffect "01088" Nothing (toSource attrs) target
        , Discard (toTarget attrs)
        ]
    _ -> SureGamble3 <$> runMessage msg attrs

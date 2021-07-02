module Arkham.Types.Event.Cards.WillToSurvive4 where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype WillToSurvive4 = WillToSurvive4 EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

willToSurvive4 :: EventCard WillToSurvive4
willToSurvive4 = event WillToSurvive4 Cards.willToSurvive4

instance HasModifiersFor env WillToSurvive4 where
  getModifiersFor = noModifiersFor

instance HasActions env WillToSurvive4 where
  getActions i window (WillToSurvive4 attrs) = getActions i window attrs

instance HasQueue env => RunMessage env WillToSurvive4 where
  runMessage msg e@(WillToSurvive4 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      e <$ unshiftMessages
        [ CreateEffect "01085" Nothing (toSource attrs) (InvestigatorTarget iid)
        , Discard (EventTarget eid)
        ]
    _ -> WillToSurvive4 <$> runMessage msg attrs

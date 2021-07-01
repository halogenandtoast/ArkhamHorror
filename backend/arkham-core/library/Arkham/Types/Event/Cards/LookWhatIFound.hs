module Arkham.Types.Event.Cards.LookWhatIFound where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Event.Runner
import Arkham.Types.Message
import Arkham.Types.Target

newtype LookWhatIFound = LookWhatIFound EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lookWhatIFound :: EventCard LookWhatIFound
lookWhatIFound = event LookWhatIFound Cards.lookWhatIFound

instance HasModifiersFor env LookWhatIFound where
  getModifiersFor = noModifiersFor

instance HasActions env LookWhatIFound where
  getActions i window (LookWhatIFound attrs) = getActions i window attrs

instance (EventRunner env) => RunMessage env LookWhatIFound where
  runMessage msg e@(LookWhatIFound attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ | eid == eventId -> do
      lid <- getId iid
      e <$ unshiftMessages
        [InvestigatorDiscoverClues iid lid 2 Nothing, Discard (EventTarget eid)]
    _ -> LookWhatIFound <$> runMessage msg attrs

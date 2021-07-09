module Arkham.Types.Event.Cards.EmergencyCache
  ( emergencyCache
  , EmergencyCache(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message
import Arkham.Types.Target

newtype EmergencyCache = EmergencyCache EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache :: EventCard EmergencyCache
emergencyCache = event EmergencyCache Cards.emergencyCache

instance HasModifiersFor env EmergencyCache where
  getModifiersFor = noModifiersFor

instance HasActions env EmergencyCache where
  getActions i window (EmergencyCache attrs) = getActions i window attrs

instance HasQueue env => RunMessage env EmergencyCache where
  runMessage msg e@(EmergencyCache attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == eventId ->
      e <$ pushAll [TakeResources iid 3 False, Discard (EventTarget eid)]
    _ -> EmergencyCache <$> runMessage msg attrs

module Arkham.Event.Cards.EmergencyCache2
  ( emergencyCache2
  , EmergencyCache2(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype EmergencyCache2 = EmergencyCache2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

emergencyCache2 :: EventCard EmergencyCache2
emergencyCache2 = event EmergencyCache2 Cards.emergencyCache2

instance RunMessage EmergencyCache2 where
  runMessage msg e@(EmergencyCache2 attrs@EventAttrs {..}) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId -> do
      drawing <- drawCards iid attrs 1
      pushAll
        [ TakeResources iid 3 (toSource attrs) False
        , drawing
        , discard attrs
        ]
      pure e
    _ -> EmergencyCache2 <$> runMessage msg attrs

module Arkham.Types.Event.Cards.AChanceEncounter
  ( aChanceEncounter
  , AChanceEncounter(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype AChanceEncounter = AChanceEncounter EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

aChanceEncounter :: EventCard AChanceEncounter
aChanceEncounter = event AChanceEncounter Cards.aChanceEncounter

instance HasActions env AChanceEncounter where
  getActions iid window (AChanceEncounter attrs) = getActions iid window attrs

instance HasModifiersFor env AChanceEncounter

instance RunMessage env AChanceEncounter where
  runMessage msg e@(AChanceEncounter attrs) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> AChanceEncounter <$> runMessage msg attrs

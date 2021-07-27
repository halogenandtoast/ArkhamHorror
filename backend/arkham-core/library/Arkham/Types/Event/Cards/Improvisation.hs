module Arkham.Types.Event.Cards.Improvisation
  ( improvisation
  , Improvisation(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype Improvisation = Improvisation EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

improvisation :: EventCard Improvisation
improvisation = event Improvisation Cards.improvisation

instance HasActions env Improvisation where
  getActions iid window (Improvisation attrs) = getActions iid window attrs

instance HasModifiersFor env Improvisation

instance RunMessage env Improvisation where
  runMessage msg e@(Improvisation attrs) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> Improvisation <$> runMessage msg attrs

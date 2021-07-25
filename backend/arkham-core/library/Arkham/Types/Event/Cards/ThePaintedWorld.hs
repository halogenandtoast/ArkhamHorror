module Arkham.Types.Event.Cards.ThePaintedWorld
  ( thePaintedWorld
  , ThePaintedWorld(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype ThePaintedWorld = ThePaintedWorld EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

thePaintedWorld :: EventCard ThePaintedWorld
thePaintedWorld = event ThePaintedWorld Cards.thePaintedWorld

instance HasActions env ThePaintedWorld where
  getActions iid window (ThePaintedWorld attrs) = getActions iid window attrs

instance HasModifiersFor env ThePaintedWorld

instance RunMessage env ThePaintedWorld where
  runMessage msg e@(ThePaintedWorld attrs) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> ThePaintedWorld <$> runMessage msg attrs

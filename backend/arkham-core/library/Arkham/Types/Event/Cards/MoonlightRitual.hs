module Arkham.Types.Event.Cards.MoonlightRitual
  ( moonlightRitual
  , MoonlightRitual(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype MoonlightRitual = MoonlightRitual EventAttrs
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

moonlightRitual :: EventCard MoonlightRitual
moonlightRitual = event MoonlightRitual Cards.moonlightRitual

instance HasActions env MoonlightRitual where
  getActions iid window (MoonlightRitual attrs) = getActions iid window attrs

instance HasModifiersFor env MoonlightRitual

instance RunMessage env MoonlightRitual where
  runMessage msg e@(MoonlightRitual attrs) = case msg of
    InvestigatorPlayEvent _ eid _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> MoonlightRitual <$> runMessage msg attrs

module Arkham.Event.Cards.DenyExistence
  ( denyExistence
  , DenyExistence(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype DenyExistence = DenyExistence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

denyExistence :: EventCard DenyExistence
denyExistence =
  event DenyExistence Cards.denyExistence

instance RunMessage DenyExistence where
  runMessage msg e@(DenyExistence attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> DenyExistence <$> runMessage msg attrs

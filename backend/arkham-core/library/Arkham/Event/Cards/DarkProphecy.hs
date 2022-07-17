module Arkham.Event.Cards.DarkProphecy
  ( darkProphecy
  , DarkProphecy(..)
  ) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message

newtype DarkProphecy = DarkProphecy EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

darkProphecy :: EventCard DarkProphecy
darkProphecy = event DarkProphecy Cards.darkProphecy

instance RunMessage DarkProphecy where
  runMessage msg e@(DarkProphecy attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> DarkProphecy <$> runMessage msg attrs

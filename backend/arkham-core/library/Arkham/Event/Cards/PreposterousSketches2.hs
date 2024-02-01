module Arkham.Event.Cards.PreposterousSketches2 (
  preposterousSketches2,
  PreposterousSketches2 (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner

newtype PreposterousSketches2 = PreposterousSketches2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity, NoThunks)

preposterousSketches2 :: EventCard PreposterousSketches2
preposterousSketches2 = event PreposterousSketches2 Cards.preposterousSketches2

instance RunMessage PreposterousSketches2 where
  runMessage msg e@(PreposterousSketches2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      drawing <- drawCards iid attrs 3
      push drawing
      pure e
    _ -> PreposterousSketches2 <$> runMessage msg attrs

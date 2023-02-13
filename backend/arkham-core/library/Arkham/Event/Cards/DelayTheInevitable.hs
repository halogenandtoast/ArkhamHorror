module Arkham.Event.Cards.DelayTheInevitable
  ( delayTheInevitable
  , DelayTheInevitable(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype DelayTheInevitable = DelayTheInevitable EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

delayTheInevitable :: EventCard DelayTheInevitable
delayTheInevitable =
  event DelayTheInevitable Cards.delayTheInevitable

instance RunMessage DelayTheInevitable where
  runMessage msg e@(DelayTheInevitable attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      pure e
    _ -> DelayTheInevitable <$> runMessage msg attrs

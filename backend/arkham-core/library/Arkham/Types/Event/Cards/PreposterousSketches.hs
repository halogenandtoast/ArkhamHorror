module Arkham.Types.Event.Cards.PreposterousSketches
  ( preposterousSketches
  , PreposterousSketches(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype PreposterousSketches = PreposterousSketches EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preposterousSketches :: EventCard PreposterousSketches
preposterousSketches = event PreposterousSketches Cards.preposterousSketches

instance RunMessage env PreposterousSketches where
  runMessage msg e@(PreposterousSketches attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll [DrawCards iid 3 False, Discard (toTarget attrs)]
    _ -> PreposterousSketches <$> runMessage msg attrs

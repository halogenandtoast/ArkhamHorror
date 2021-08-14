module Arkham.Types.Event.Cards.PreposterousSketches2
  ( preposterousSketches2
  , PreposterousSketches2(..)
  ) where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype PreposterousSketches2 = PreposterousSketches2 EventAttrs
  deriving anyclass IsEvent
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

preposterousSketches2 :: EventCard PreposterousSketches2
preposterousSketches2 = event PreposterousSketches2 Cards.preposterousSketches2

instance HasAbilities env PreposterousSketches2 where
  getAbilities iid window (PreposterousSketches2 attrs) =
    getAbilities iid window attrs

instance HasModifiersFor env PreposterousSketches2

instance RunMessage env PreposterousSketches2 where
  runMessage msg e@(PreposterousSketches2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs -> do
      e <$ pushAll [DrawCards iid 3 False, Discard (toTarget attrs)]
    _ -> PreposterousSketches2 <$> runMessage msg attrs

module Arkham.Event.Cards.Marksmanship2
  ( marksmanship2
  , Marksmanship2(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype Marksmanship2 = Marksmanship2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

marksmanship2 :: EventCard Marksmanship2
marksmanship2 =
  event Marksmanship2 Cards.marksmanship2

instance RunMessage Marksmanship2 where
  runMessage msg e@(Marksmanship2 attrs) = case msg of
    InvestigatorPlayEvent _ eid _ _ _ | eid == toId attrs -> do
      e <$ pushAll [Discard (toTarget attrs)]
    _ -> Marksmanship2 <$> runMessage msg attrs

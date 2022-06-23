module Arkham.Event.Cards.TimeWarp2
  ( timeWarp2
  , TimeWarp2(..)
  )
where

import Arkham.Prelude

import qualified Arkham.Event.Cards as Cards
import Arkham.Classes
import Arkham.Event.Runner
import Arkham.Message

newtype TimeWarp2 = TimeWarp2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWarp2 :: EventCard TimeWarp2
timeWarp2 =
  event TimeWarp2 Cards.timeWarp2

instance RunMessage TimeWarp2 where
  runMessage msg e@(TimeWarp2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      -- when we undo action timewarp will either be in hand or on the deck
      e <$ pushAll [UndoAction]
    _ -> TimeWarp2 <$> runMessage msg attrs

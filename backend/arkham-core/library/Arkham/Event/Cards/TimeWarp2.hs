module Arkham.Event.Cards.TimeWarp2
  ( timeWarp2
  , TimeWarp2(..)
  ) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Message
import Arkham.Projection

newtype TimeWarp2 = TimeWarp2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timeWarp2 :: EventCard TimeWarp2
timeWarp2 = event TimeWarp2 Cards.timeWarp2

instance RunMessage TimeWarp2 where
  runMessage msg e@(TimeWarp2 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      -- we need to extract the payments
      card <- field EventCard eid
      let pc = fromJustNote "has to be a player card" $ preview _PlayerCard card
      e <$ pushAll
        [ UndoAction
        -- when we undo action timewarp will either be in hand or on the deck
        -- so we just remove it from the game to find it no matter where it is
        , RemovePlayerCardFromGame card
        -- Then we add it directly to the discard
        , AddToDiscard iid pc
        -- Then we also handle payment
        , SpendResources iid 1 -- TODO: we need to get the exact payment msgs somehow
        ]
    _ -> TimeWarp2 <$> runMessage msg attrs
